---
title: "`concurrency` and Performance"
excerpt: >
  Testing concurrent code using DejaFu requires the use of `MonadConc` and `MonadSTM`
  instead of plain `IO` and `STM`. Is there any performance impact, and if there is,
  can it be recovered?
date: 2023-04-14 19:54 +0200
categories: "Development"
header:
  overlay_image: /assets/images/32902732016_430826ec3b_c.jpg
  overlay_filter: 0.4
  caption: "Photo credit: [**Mohan Picz**](https://www.flickr.com/photos/147265682@N02/32902732016/)"
---
When using [DejaFu](https://hackage.haskell.org/package/dejafu) to test concurrent code,
as introduced in [a previous post]({% post_url 2023-04-06-testing-concurrent-code-using-dejafu %}),
the [`MonadConc`](https://hackage.haskell.org/package/concurrency-1.11.0.2/docs/Control-Monad-Conc-Class.html#t:MonadConc)
and [`MonadSTM`](https://hackage.haskell.org/package/concurrency-1.11.0.2/docs/Control-Monad-STM-Class.html#t:MonadSTM)
type classes must be used to abstract over
the implementation of concurrency and STM. In *regular* code, the types and functions in the
standard `IO` and `STM` monads would be used instead. Luckily, there's an `instance MonadConc IO`
and an `instance MonadSTM STM`, so code using the [`concurrency`](https://hackage.haskell.org/package/concurrency-1.11.0.2)
abstractions will work
just fine in `IO` or `STM`, but going through a type class can have a performance impact due to
a dictionary lookup that comes with it. This was one of the questions raised in the
[Future Work]({% post_url 2023-04-06-testing-concurrent-code-using-dejafu %}#future-work) section
of last week's article.

In the implementation of `MonadConc` for `IO`, as well as the implementation of `MonadSTM` for
`STM`, most if not all functions map directly to their `IO` and `STM` counterparts from `base`
and `stm`. Hence, if we can convince the compiler to emit code not using the type class dictionary
when using testable functions (i.e., functions using `MonadConc` and `MonadSTM`) in environments
where *testability* is not needed (i.e., when used in `IO` and `STM`), there should be no
performance impact.

Since this is a Literate Haskell file, some boilerplate before we continue
(see [this article]({% post_url 2023-04-10-jekyll-literate-haskell %}) for more
information on how this works):

```haskell
{- cabal:
build-depends:
  , base ^>=4.17
  , concurrency ^>=1.11.0.2
  , stm ^>=2.5.1.0
  , tasty ^>=1.4.3
  , tasty-bench ^>=0.3.3
  , tasty-inspection-testing ^>=0.2
default-language: Haskell2010
build-tool-depends: markdown-unlit:markdown-unlit
ghc-options:
  -pgmL markdown-unlit
  -rtsopts=all "-with-rtsopts=-T -A32m"
  -fproc-alignment=64
  -ddump-simpl
  -dsuppress-idinfo
  -dsuppress-coercions
  -dsuppress-type-applications
  -dsuppress-uniques
  -dsuppress-module-prefixes
-}

{-# LANGUAGE TemplateHaskell #-}

module Main (
  main,
  benchSTM,
  benchConcurrencyNoInline,
  benchConcurrency,
  benchConcurrencySpecialized
  ) where

import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.Classy.STM as C
import qualified Control.Monad.Conc.Class as C
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Bench (Benchmark, bench, bcompare, bgroup, defaultMain, nfIO)
import Test.Tasty.Inspection (inspectTest, hasNoTypeClasses, (==-))
```

## Benchmark Baseline

To start, let's measure the baseline performance of a very simple STM transaction which takes three
`TVar Int`s, reads the contents of the first two, puts their sum in the third, then reads the
third and returns the value:

```haskell
benchSTM :: STM.TVar Int -> STM.TVar Int -> STM.TVar Int -> STM.STM Int
benchSTM v1 v2 v3 = do
  v1' <- STM.readTVar v1
  v2' <- STM.readTVar v2
  STM.writeTVar v3 $! v1' + v2'
  STM.readTVar v3

benchConcurrencyNoInline ::
  C.MonadSTM stm =>
  C.TVar stm Int ->
  C.TVar stm Int ->
  C.TVar stm Int ->
  stm Int
benchConcurrencyNoInline v1 v2 v3 = do
  v1' <- C.readTVar v1
  v2' <- C.readTVar v2
  C.writeTVar v3 $! v1' + v2'
  C.readTVar v3
{-# NOINLINE benchConcurrencyNoInline #-}

withVars:: (IO (STM.TVar Int, STM.TVar Int, STM.TVar Int) -> TestTree) -> TestTree
withVars = withResource mkTVars (const $ pure ())
  where
    mkTVars = (,,) <$> STM.newTVarIO 1
                   <*> STM.newTVarIO 2
                   <*> STM.newTVarIO 0

benchSTMvsBenchConcurrencyNoInline :: Benchmark
benchSTMvsBenchConcurrencyNoInline = withVars $ \getVars -> testGroup "baseline" [
    bench "STM" $ nfIO $ getVars >>= \(v1, v2, v3) ->
      STM.atomically $ benchSTM v1 v2 v3,
    bcompare "STM" $
      bench "ConcurrencyNoInline" $ nfIO $ getVars >>= \(v1, v2, v3) ->
        C.atomically $ benchConcurrencyNoInline v1 v2 v3
  ]
```

Running this benchmark yields not-so-stellar results:

```
  baseline
    STM:                 OK (0.40s)
      45.4 ns ± 4.2 ns,  63 B  allocated,   0 B  copied,  34 MB peak memory
    ConcurrencyNoInline: OK (0.27s)
      126  ns ± 5.5 ns, 505 B  allocated,   0 B  copied,  34 MB peak memory, 2.78x
```

## Automatic Specialization

The `NOINLINE` pragma on `benchConcurrencyNoInline` prohibits GHC to inline the
code (in the benchmark), otherwise specialization could trigger.

If we create a version of the code without the `INLINE` pragma and benchmark it again,
it appears the code performs roughly the same as the native `STM` version:

```haskell
benchConcurrency ::
  C.MonadSTM stm =>
  C.TVar stm Int ->
  C.TVar stm Int ->
  C.TVar stm Int ->
  stm Int
benchConcurrency v1 v2 v3 = do
  v1' <- C.readTVar v1
  v2' <- C.readTVar v2
  C.writeTVar v3 $! v1' + v2'
  C.readTVar v3

benchSTMvsBenchConcurrency :: Benchmark
benchSTMvsBenchConcurrency = withVars $ \getVars -> testGroup "plain" [
    bcompare "STM" $
      bench "Concurrency" $ nfIO $ getVars >>= \(v1, v2, v3) ->
        C.atomically $ benchConcurrency v1 v2 v3
  ]
```

yields

```
  plain
    Concurrency:         OK (0.39s)
      45.8 ns ± 3.4 ns,  63 B  allocated,   0 B  copied,  35 MB peak memory, 1.01x
```

Why does this happen, and can we be certain this is not a fluke?

## Inspecting Core
To investigate, we can pass some options to GHC so it prints the *Core* it generates. *Core* is a
language somewhat like a very simplified version of Haskell, without type classes (which are
desugared into dictionaries at this point), that can then be compiled into even more low-level
languages before binary code is generated. Here are the important bits:

```
benchSTM1
  :: TVar Int
     -> TVar Int
     -> TVar Int
     -> State# RealWorld
     -> (# State# RealWorld, Int #)
benchSTM1
  = \ (v1 :: TVar Int)
      (v2 :: TVar Int)
      (v3 :: TVar Int)
      (s :: State# RealWorld) ->
      case v1 of { TVar tvar# ->
      case readTVar# tvar# s of { (# ipv, ipv1 #) ->
      case v2 of { TVar tvar#1 ->
      case readTVar# tvar#1 ipv of { (# ipv2, ipv3 #) ->
      case ipv1 of { I# x ->
      case ipv3 of { I# y ->
      case v3 of { TVar tvar#2 ->
      case writeTVar# tvar#2 (I# (+# x y)) ipv2 of s2# { __DEFAULT ->
      readTVar# tvar#2 s2#
      }
      }
      }
      }
      }
      }
      }
      }

benchSTM :: TVar Int -> TVar Int -> TVar Int -> STM Int
benchSTM
  = benchSTM1
    `cast` <Co:15> :: (TVar Int
                       -> TVar Int
                       -> TVar Int
                       -> State# RealWorld
                       -> (# State# RealWorld, Int #))
                      ~R# (TVar Int -> TVar Int -> TVar Int -> STM Int)
```

With a bit of imagination, we can see `benchSTM1` resembles our original `benchSTM` code,
in a state monad over `State# RealWorld`. Keep in mind `TVar` is a
`newtype` constructor over the compiler-internal representation of a `TVar` (which is
`TVar# RealWorld a`), and `readTVar#` and friends are alike `readTVar`, but for the
internal `TVar#` type.

`benchSTM` is `benchSTM1` `cast`ed to plain `STM` (which is, like `IO`, a `newtype` of
`State# RealWorld -> (# State# RealWorld, a #)`).

Here's `benchConcurrencyNoInline`:

```
benchConcurrencyNoInline
  :: forall (stm :: * -> *).
     MonadSTM stm =>
     TVar stm Int -> TVar stm Int -> TVar stm Int -> stm Int
benchConcurrencyNoInline
  = \ (@(stm :: * -> *)) ($dMonadSTM :: MonadSTM stm) ->
      let {
        $dMonad :: MonadPlus stm
        $dMonad = $p2MonadSTM $dMonadSTM } in
      let {
        $dMonad1 :: Monad stm
        $dMonad1 = $p2MonadPlus $dMonad } in
      \ (v1 :: TVar stm Int) (v2 :: TVar stm Int) (v3 :: TVar stm Int) ->
        let {
          lvl15 :: stm Int
          lvl15 = readTVar $dMonadSTM v2 } in
        let {
          lvl16 :: stm Int
          lvl16 = readTVar $dMonadSTM v3 } in
        >>=
          $dMonad1
          (readTVar $dMonadSTM v1)
          (\ (v1' :: Int) ->
             >>=
               $dMonad1
               lvl15
               (\ (v2' :: Int) ->
                  >>
                    $dMonad1
                    (case v1' of { I# x ->
                     case v2' of { I# y -> writeTVar $dMonadSTM v3 (I# (+# x y)) }
                     })
                    lvl16))
```

Here, the `MonadSTM` dictionary is passed along (and then unpacked to retrieve the `MonadPlus`
and `Monad` instances as well), and every call to, e.g., `readTVar` becomes a field lookup
from the dictionary (`readTVar $dMonadSTM` is like a regular field lookup in a `data` record),
hence an indirect call. Also, the monadic binds (`>>=` and `>>`) can't be expanded as is the
case in `benchSTM`. It seems pretty clear why `benchConcurrencyNoInline` performs about 2.78x
slower than `benchSTM`!

Last but not least, the code GHC generates for `benchConcurrency`:

```
benchConcurrency
  :: forall (stm :: * -> *).
     MonadSTM stm =>
     TVar stm Int -> TVar stm Int -> TVar stm Int -> stm Int
benchConcurrency
  = \ (@(stm :: * -> *)) ($dMonadSTM :: MonadSTM stm) ->
      let {
        $dMonad :: MonadPlus stm
        $dMonad = $p2MonadSTM $dMonadSTM } in
      let {
        $dMonad1 :: Monad stm
        $dMonad1 = $p2MonadPlus $dMonad } in
      \ (v1 :: TVar stm Int) (v2 :: TVar stm Int) (v3 :: TVar stm Int) ->
        let {
          lvl15 :: stm Int
          lvl15 = readTVar $dMonadSTM v2 } in
        let {
          lvl16 :: stm Int
          lvl16 = readTVar $dMonadSTM v3 } in
        >>=
          $dMonad1
          (readTVar $dMonadSTM v1)
          (\ (v1' :: Int) ->
             >>=
               $dMonad1
               lvl15
               (\ (v2' :: Int) ->
                  >>
                    $dMonad1
                    (case v1' of { I# x ->
                     case v2' of { I# y -> writeTVar $dMonadSTM v3 (I# (+# x y)) }
                     })
                    lvl16))
```

This looks... suspiciously similar to the `benchConcurrencyNoInline` code. But when running
the benchmark, the performance of the regular version was aligned with the `benchSTM` code.
What's going on?

It turns out GHC spotted an opportunity for specialization, and performed the following two
steps:

- It created a specialized version of the function:

```
benchConcurrency1
  :: TVar STM Int
     -> TVar STM Int
     -> TVar STM Int
     -> State# RealWorld
     -> (# State# RealWorld, Int #)
benchConcurrency1
  = \ (v1 :: TVar STM Int)
      (v2 :: TVar STM Int)
      (v3 :: TVar STM Int)
      (eta :: State# RealWorld) ->
      case v1 `cast` <Co:3> :: TVar STM Int ~R# TVar Int of
      { TVar tvar# ->
      case readTVar# tvar# eta of { (# ipv, ipv1 #) ->
      case v2 `cast` <Co:3> :: TVar STM Int ~R# TVar Int of
      { TVar tvar#1 ->
      case readTVar# tvar#1 ipv of { (# ipv2, ipv3 #) ->
      case ipv1 of { I# x ->
      case ipv3 of { I# y ->
      case v3 `cast` <Co:3> :: TVar STM Int ~R# TVar Int of
      { TVar tvar#2 ->
      case writeTVar# tvar#2 (I# (+# x y)) ipv2 of s2# { __DEFAULT ->
      readTVar# tvar#2 s2#
      }
      }
      }
      }
      }
      }
      }
      }
```

Note this is, modulo some different but related `cast` invocations (which have no runtime impact)
the very same as `benchSTM1`. Indeed, this is a specialized version of `benchConcurrency` for
`TVar STM Int -> TVar STM Int -> TVar STM Int -> STM Int`, i.e., when using `benchConcurrency`
in the regular `STM` monad.

- It created a rule to rewrite calls to `benchConcurrency`, when used in the `STM` monad, to
  `benchConcurrency1`:

```
"SPEC benchConcurrency @STM"
    forall ($dMonadSTM :: MonadSTM STM).
      benchConcurrency $dMonadSTM
      = benchConcurrency1
        `cast` <Co:18> :: (TVar STM Int
                           -> TVar STM Int
                           -> TVar STM Int
                           -> State# RealWorld
                           -> (# State# RealWorld, Int #))
                          ~R# (TVar STM Int -> TVar STM Int -> TVar STM Int -> STM Int)
```

Note the `$dMonadSTM` dictionary isn't used on the right hand side of the rewrite rule,
so we can be certain no indirections through the `MonadSTM` dictionary can be used in
`benchConcurrency1`.

When inspecting the actual benchmark code, we can see a call to `benchConcurrency1` is
used instead of a call to `benchConcurrency`, and hence, we get the very same performance as the
`benchSTM` version:

```
{ (# ipv, ipv1 #) ->
case ipv1 of { (v1, v2, v3) ->
case atomically#
       (benchConcurrency1
          (v1 `cast` <Co:4> :: TVar Int ~R# TVar STM Int)
          (v2 `cast` <Co:4> :: TVar Int ~R# TVar STM Int)
          (v3 `cast` <Co:4> :: TVar Int ~R# TVar STM Int))
       ipv
```

## Explicit Specialization

Great, GHC is able to specialize `MonadSTM` code to the equivalent `STM` code when it
spots an opportunity to do so. However, this only works within a single module: if `benchConcurrency` were
defined in one module, and used from another to benchmark it, the implementation of
`benchConcurrency` is no longer visible to GHC, except if it decided the function is small enough
to be inlineable, or we told it (with the `INLINE` pragma) to do so. Many `STM` functions are
relatively large though, so we don't necessarily want to inline them. What to do?

As a library author, we can force GHC to create specialized versions of some code for us, and
generate the according rewrite rule as well, as if it decided to do so by itself within a single
module. Let's give this a try:

```haskell
benchConcurrencySpecialized ::
  C.MonadSTM stm =>
  C.TVar stm Int ->
  C.TVar stm Int ->
  C.TVar stm Int ->
  stm Int
benchConcurrencySpecialized v1 v2 v3 = do
  v1' <- C.readTVar v1
  v2' <- C.readTVar v2
  C.writeTVar v3 $! v1' + v2'
  C.readTVar v3
{-# SPECIALIZE benchConcurrencySpecialized ::
      STM.TVar Int -> STM.TVar Int -> STM.TVar Int -> STM.STM Int #-}

benchSTMvsBenchConcurrencySpecialized :: Benchmark
benchSTMvsBenchConcurrencySpecialized = withVars $ \getVars -> testGroup "specialized" [
    bcompare "STM" $
      bench "ConcurrencySpecialized" $ nfIO $ getVars >>= \(v1, v2, v3) ->
        C.atomically $ benchConcurrencySpecialized v1 v2 v3
  ]
```

Note the use of the `SPECIALIZE` pragma above. Running this gives

```
  specialized
    ConcurrencySpecialized: OK (0.20s)
      47.9 ns ± 4.4 ns,  63 B  allocated,   0 B  copied,  35 MB peak memory, 1.03x
```

Furthermore, the following *Core* is generated:

```
benchConcurrencySpecialized
  :: forall (stm :: * -> *).
     MonadSTM stm =>
     TVar stm Int -> TVar stm Int -> TVar stm Int -> stm Int
benchConcurrencySpecialized
  = \ (@(stm :: * -> *)) ($dMonadSTM :: MonadSTM stm) ->
      let {
        $dMonad :: MonadPlus stm
        $dMonad = $p2MonadSTM $dMonadSTM } in
      let {
        $dMonad1 :: Monad stm
        $dMonad1 = $p2MonadPlus $dMonad } in
      \ (v1 :: TVar stm Int) (v2 :: TVar stm Int) (v3 :: TVar stm Int) ->
        let {
          lvl19 :: stm Int
          lvl19 = readTVar $dMonadSTM v2 } in
        let {
          lvl20 :: stm Int
          lvl20 = readTVar $dMonadSTM v3 } in
        >>=
          $dMonad1
          (readTVar $dMonadSTM v1)
          (\ (v1' :: Int) ->
             >>=
               $dMonad1
               lvl19
               (\ (v2' :: Int) ->
                  >>
                    $dMonad1
                    (case v1' of { I# x ->
                     case v2' of { I# y -> writeTVar $dMonadSTM v3 (I# (+# x y)) }
                     })
                    lvl20))

"SPEC benchConcurrencySpecialized"
    forall ($dMonadSTM :: MonadSTM STM).
      benchConcurrencySpecialized $dMonadSTM
      = benchConcurrency1
        `cast` <Co:18> :: (TVar STM Int
                           -> TVar STM Int
                           -> TVar STM Int
                           -> State# RealWorld
                           -> (# State# RealWorld, Int #))
                          ~R# (TVar STM Int -> TVar STM Int -> TVar STM Int -> STM Int)
```

The compiler detected the specialized version of `benchConcurrencySpecialized` for `STM`
is the very same as the specialized version it already created for `benchConcurrency`, so
`benchConcurrency1` gets simply reused.

With the `SPECIALIZE` pragma in place, we can write and test code using `MonadSTM`, whilst
at the same time guaranteeing that users of the library using plain `STM` code won't incur
any performance hit.

## Testing

We can even go one step further, and ensure `MonadSTM` usage is completely erased: in a test
using the [`inspection-testing`](https://hackage.haskell.org/package/inspection-testing-0.5.0.1)
library. Even more, we can check whether two versions of a
function using our library function are (after inlining) the same:

```haskell
stmVersion :: IO ()
stmVersion = do
  v <- STM.newTVarIO 0
  _ <- STM.atomically $ benchSTM v v v
  pure ()

concurrencyVersion :: IO ()
concurrencyVersion = do
  v <- STM.newTVarIO 0
  _ <- STM.atomically $ benchConcurrencySpecialized v v v
  pure ()

testMonadSTMErased :: TestTree
testMonadSTMErased = $(inspectTest $ hasNoTypeClasses 'concurrencyVersion)

testFunctionsEqual :: TestTree
testFunctionsEqual = $(inspectTest $ 'concurrencyVersion ==- 'stmVersion)
```

```
  concurrencyVersion does not contain dictionary values: OK
  concurrencyVersion ==- stmVersion:                     OK
```

Success!

## Conclusion

This article showns how, with liberal use of `SPECIALIZE` pragmas in library code,
it's possible to eliminate the overhead of `MonadConc` and `MonadSTM` when library functions
are used in the regular `IO` and `STM` monads. Hence, let this be a call to library authors
to write their code using `MonadConc` and `MonadSTM` such that consumers of the library
can still write DejaFu tests for code using data structures and functions defined in it!

### Wrapping Up

To wrap up, run all benchmarks and tests:

```haskell
main :: IO ()
main = defaultMain [
  benchSTMvsBenchConcurrencyNoInline,
  benchSTMvsBenchConcurrency,
  benchSTMvsBenchConcurrencySpecialized,
  testMonadSTMErased,
  testFunctionsEqual
  ]
```
