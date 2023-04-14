---
title: "Testing concurrent code using DejaFu"
excerpt: >
  Writing correct concurrent code with shared mutable state is hard. Testing concurrent
  code with shared mutable state is hard. In Haskell, we can use STM to transactionally
  change state, and DejaFu to test concurrent code. In this post, we look into how this
  is done, and discuss an approach to simplify the testing effort.
date: 2023-04-06 20:17 +0100
last_modified_at: 2023-04-14 21:31 +0200
categories: "Development"
header:
  overlay_image: /assets/images/27174457817_7b6cc225e4_c.jpg
  overlay_filter: 0.4
  caption: "Photo credit: [**Mike Lawrence**](https://www.flickr.com/photos/157270154@N05/27174457817)"
---
Recently I was working on some code using [GHC](https://www.haskell.org/ghc/)
[Haskell](https://www.haskell.org)'s [STM](https://hackage.haskell.org/package/stm) (short for
Software Transactional Memory)
functionality. STM gives a programmer mutable variables (so-called `TVar`s), functions to work on such
variables in some context (e.g., `writeTVar :: TVar a -> a -> STM ()`), and a way to run a program
working with such variables transactionally (`atomically :: STM a -> IO a`). The transactional aspect
is what makes STM so powerful: when multiple threads of execution mutate the same `TVar`s concurrently,
a transaction will be automatically aborted and restarted by the runtime upon conflicts. As such, we
can construct programs working with one or more mutable variables, without the need for explicit locking.

The above only scratches the surface of what's possible with STM. To learn more, see
[chapter 28](https://book.realworldhaskell.org/read/software-transactional-memory.html)
of [Real World Haskell](https://book.realworldhaskell.org/).

STM makes writing concurrent code with shared mutable state easier, but still leaves room for mistakes.
Code with mutable state, especially with concurrent access to shared mutable state, is more difficult
to test than pure computations. Using [DejaFu](https://hackage.haskell.org/package/dejafu), we can
write tests of concurrent code which will simulate various schedulings of application threads and
validate the result, e.g., by ensuring a program yields the same value no matter the order in which
threads were scheduled, or by ensuring some invariants were maintained at every observable point in
time. In this post, I want to show two patterns I adopted when writing concurrent code, using STM,
with the ability to test it using DejaFu. If you're not familiar with the latter, you can find more
information on its [website](https://dejafu.readthedocs.io/en/latest/getting_started.html#why-deja-fu).

First, some boilerplate so this post can be executed as a Literal Haskell script:

```haskell
{- cabal: 
build-depends:
  , base ^>=4.17.0.0
  , concurrency ^>=1.11.0.2
  , exceptions ^>=0.10.7
  , tasty ^>=1.4.3
  , tasty-dejafu ^>=2.1.0.0
build-tool-depends: markdown-unlit:markdown-unlit
ghc-options: -threaded -pgmL markdown-unlit -Wall -Werror
default-language: Haskell2010
-}
```

To test code using DejaFu, it can't be written using, e.g., `readTVar :: TVar a -> STM a`, but
instead needs to use the abstractions provided by the
[`concurrency`](https://hackage.haskell.org/package/concurrency-1.11.0.2) package, e.g.,
`readTVar :: MonadSTM stm => TVar stm a -> stm a`. Notice how the `TVar` type is parametrized
by the monad in which it'll be used. This allows for use in the traditional `STM` monad (which
is an instance of `MonadSTM`), as well as in emulations of it provided by DejaFu.

Furthermore, we'll use the [`exceptions`](https://hackage.haskell.org/package/exceptions-0.10.7)
package to throw exceptions while checking invariants, and the
[`tasty`](https://hackage.haskell.org/package/tasty-1.4.3) test framework, including DejaFu
integration for it brought by
[`tasty-dejafu`](https://hackage.haskell.org/package/tasty-dejafu-2.1.0.0) to define and run
some tests.

All good Haskell code starts with some `LANGUAGE` pragmas and some `import`s, so let's get those out
of the way:

```haskell
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Concurrent.Classy.Async (concurrently_, wait, withAsync)
import Control.Concurrent.Classy.STM (TVar, readTVar, modifyTVar')
import Control.Monad (unless)
import Control.Monad.Catch (Exception(..), MonadThrow, throwM)
import Control.Monad.Conc.Class (MonadConc, STM, atomically, newTVarConc)
import Control.Monad.STM.Class (MonadSTM)
import Data.Functor.Identity (Identity(..))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.DejaFu (Basic, Program, WithSetup, inspectTVar, registerInvariant, testAuto, withSetup)
```

## State and Mutations

In the (admittedly contrived) example used throughout this post, our program keeps track of
two `Int` values, and provides two actions to mutate them: one to add an `Int` which will
add the given value to the first mutable variable and substract it from the second, and another
to substract an `Int` which will, as you may have guessed, substract the given value from the
first mutable variable and add it to the second. Hence, there's an invariant that should be
maintained at all times: the sum of both mutable variables should be `0`.

### State Type

In regular STM code, we'd use a datatype like the following to maintain the state:

```
data Store = Store
  { storeA :: TVar Int,
    storeB :: TVar Int
  }
```

This works, but as mentioned before, we need to use the types provided by the `concurrency` package
to be able to test our code using DejaFu. Hence, we need to use its `TVar stm a` type (family)
instead:

```
data Store stm = Store
  { storeA :: TVar stm Int,
    storeB :: TVar stm Int
  }
```

This goes in the right direction, and indeed, we could provide implementations for `add` and `sub`
and test those. However, there are still some shortcomings:

- When checking invariants, writing code doing so in a *pure* style (modulo throwing exceptions)
  simplifies the implementation a lot, instead of littering the validation code with `readTVar` calls
  and monadic binds. This, in turn, makes testing of the invariants validation code easier as well.

- DejaFu can be used to validate a concurrent computation always yields the same result, which imposes
  an `Eq` constraint on the result type. We often want to check the shared mutable state is the same
  no matter which scheduling of mutators happened, but a data type with `TVar`s inside can't have an
  `Eq` instance.

So, we build `Store` such that it's paramterized over a `cell` type which, for the mutable version
of `Store` will be some `TVar`, whilst `cell` can also be `Identity` in which case the resulting
type is immutable:

```haskell
data Store' cell = Store'
  { storeA :: cell Int,
    storeB :: cell Int
  }

deriving instance (forall a. Show a => Show (cell a)) => Show (Store' cell)
deriving instance (forall a. Eq a => Eq (cell a)) => Eq (Store' cell)

type FrozenStore = Store' Identity
type Store stm = Store' (TVar stm)
```

As in the above, we can derive a `Show` and `Eq` instance for `Store' cell` as long as there's
a `Show` and `Eq` instance for `cell a`, for all possible `a`s. This is not the case for `Store`
(e.g., there's no `Show` instance for `TVar stm a`), but it is for `FrozenStore`.

### Functions

We can now define a function to create a new `Store`, and implement `add` and `sub`:

```haskell
newStoreConc :: MonadConc m => m (Store (STM m))
newStoreConc =
  Store'
    <$> newTVarConc 0
    <*> newTVarConc 0

addSTM :: MonadSTM stm => Int -> Store stm -> stm ()
addSTM i store = do
  modifyTVar' (storeA store) (\a -> a + i)
  modifyTVar' (storeB store) (\b -> b - i)

add :: MonadConc m => Int -> Store (STM m) -> m ()
add i store = atomically (addSTM i store)

subSTM :: MonadSTM stm => Int -> Store stm -> stm ()
subSTM i store = do
  modifyTVar' (storeA store) (\a -> a - i)
  modifyTVar' (storeB store) (\b -> b - i)

sub :: MonadConc m => Int -> Store (STM m) -> m ()
sub i store = atomically (subSTM i store)
```

(Did you spot the bug?)

If the types in the code above are a bit daunting, when specialized to `IO` we'd get

```
import Control.Concurrent.STM (STM, TVar)

type Store = Store' TVar

-- | 'newStoreIO', so you want.
newStoreConc :: IO Store

addSTM :: Int -> Store -> STM ()
add :: Int -> Store -> IO ()

subSTM :: Int -> Store -> STM ()
sub :: Int -> Store -> IO ()
```

### Snapshots

The code above works on the mutable version of `Store'`. However, we went through this `cell`
hassle in order to get an immutable version of a `Store'` for use in invariant checks and
consistency tests. The following functions can be defined to turn a `Store stm` into an
`stm FrozenStore`:

```haskell
snapshotWith ::
  Applicative m =>
  (forall a. cell a -> m a) ->
  Store' cell ->
  m FrozenStore
snapshotWith readCell store =
  Store'
    <$> (Identity <$> readCell (storeA store))
    <*> (Identity <$> readCell (storeB store))

snapshotSTM :: MonadSTM stm => Store stm -> stm FrozenStore
snapshotSTM = snapshotWith readTVar

snapshot :: MonadConc m => Store (STM m) -> m FrozenStore
snapshot = atomically . snapshotSTM
```

The `snapshotWith` function is where the magic happens. It takes an action which, in some
`Applicative` context, can read the value of a `cell`, and can then turn a `Store' cell` into
a `FrozenStore` (where `cell` is `Identity`) within said context. `snapshotSTM` is a utility
to work with a `MonadSTM stm => Store stm` (i.e., snapshotting with `readTVar`), and `snapshot`
wraps `snapshotSTM` to run in a `MonadConc`.

We need `snapshotWith`, and not only `snapshotSTM`, for use in DejaFu's `Invariant` monad later.

### Invariants

Finally, we can define `checkInvariants`, which ensures invariants are met in a `FrozenStore`.
If not, it throws an `InvariantViolation` exception:

```haskell
newtype InvariantViolation = InvariantViolation String
  deriving (Show)

instance Exception InvariantViolation where
  displayException (InvariantViolation msg) = "Invariant violation: " <> msg

invariantViolation :: MonadThrow m => String -> m a
invariantViolation = throwM . InvariantViolation

checkInvariants :: MonadThrow m => FrozenStore -> m ()
checkInvariants store = do
  let Identity a = storeA store
      Identity b = storeB store

  unless (a + b == 0) $
    invariantViolation ("a + b /= 0, a = " <> show a <> ", b = " <> show b)
```

There are many ways to implement `checkInvariants`, and the above is only one of them. Some
alternatives include

- Making `checkInvariants` pure, returning, e.g., `Either String ()` or a `Bool` value, and
  wrapping it later to turn a non-successful result into an exception (as required by DejaFu).

- Using the 
  [`Validation` monad](https://hackage.haskell.org/package/either-5.0.2/docs/Data-Either-Validation.html)
  to capture all invariant violations instead of only the first one, and either returning
  them or throwing them at the end.

## Concurrent Access
With the above code in place, here's some code using a `Store`, mutating it
concurrently in various ways:

```haskell
-- | Add two values to a `Store`, concurrently.
concurrentAdd :: MonadConc m => Int -> Int -> Store (STM m) -> m ()
concurrentAdd i j store =
  concurrently_
    (add i store)
    (add j store)

-- | Add two values to a `Store`, concurrently. With a bug.
brokenConcurrentAdd :: MonadConc m => Int -> Int -> Store (STM m) -> m ()
brokenConcurrentAdd i j store =
  withAsync (add i store) $ \a ->
    withAsync (add j store) $ \_ ->
      wait a

-- | Add and substract a value from a `Store`, concurrently.
concurrentAddAndSub :: MonadConc m => Int -> Int -> Store (STM m) -> m ()
concurrentAddAndSub i j store =
  concurrently_
    (add i store)
    (sub j store)
```

## Testing

Finally, we can write some tests for the above code. First, `withStore` is a helper to
create a `Store'` (within the appropriate DejaFu `MonadConc` environment), and register
an invariant check. The latter will create a `FrozenStore` from the mutable one using
`snapshotWith inspectTVar`. `inspectTVar` is a function in DejaFu's `Invariant` monad
which allows to peek inside the contents of a `TVar` in the model:

```haskell
withStore ::
  Monad m =>
  (Store (STM (Program Basic m)) -> Program Basic m a) ->
  Program (WithSetup (Store (STM (Program Basic m)))) m a
withStore = withSetup $ do
  store <- newStoreConc

  registerInvariant $
    checkInvariants =<< snapshotWith inspectTVar store

  pure store
```

Then, the tests. Each test runs the default DejaFu validations calling one of the
concurrent functions on a provisioned `Store'`, and returns a snapshot of the
store. DejaFu will then check whether every interleaving of concurrent actions
results in the same value/state.

```haskell
main :: IO ()
main = defaultMain $ testGroup "store-app" [
  testAuto "concurrentAdd" $ withStore $ \store -> do
    concurrentAdd 10 20 store

    snapshot store,

  testAuto "brokenConcurrentAdd" $ withStore $ \store -> do
    brokenConcurrentAdd 10 20 store

    snapshot store,

  testAuto "concurrentAddAndSub" $ withStore $ \store -> do
    concurrentAddAndSub 10 20 store

    snapshot store
  ]
```

The first test succeeds, as desired:

```
store-app
  concurrentAdd
    Never Deadlocks:   OK (0.13s)
    No Exceptions:     OK (0.02s)
    Consistent Result: OK (0.02s)
```

The second one, however, fails, as expected:

```
  brokenConcurrentAdd
    Never Deadlocks:   OK
    No Exceptions:     OK
    Consistent Result: FAIL
      Failed:
        Store' {storeA = Identity 30, storeB = Identity (-30)} S0-----------S1---------S0---S2---------S0-------------

        Store' {storeA = Identity 10, storeB = Identity (-10)} S0-----------S1---------S0---S2--P0---S2---S0-----------

      Use -p '/brokenConcurrentAdd.Consistent Result/' to rerun this test only.
```

Indeed, `brokenConcurrentAdd` fails to `wait` for the inner `Async`. Hence, there are
two possible interleavings: one where the second `add` succeeds before the thread
running it is killed (the first interleaving above, where `storeA` is `30`), or another
one where the thread is killed before its `add` action has completed, and the
snapshotted state has `10` in `storeA` (the second interleaving above).

The last test also fails, in this case not because some concurrency bug as above,
but because the invariants we expect our state to adhere to are violated:

```
  concurrentAddAndSub
    Never Deadlocks:   OK
    No Exceptions:     OK
    Consistent Result: FAIL
      Failed:
        [invariant failure] S0-------S1--------S2---

        [invariant failure] S0-------S2---

      Use -p '/concurrentAddAndSub.Consistent Result/' to rerun this test only.

2 out of 9 tests failed (0.19s)
```

Indeed, once the second thread (which runs `sub j store`) ran, no matter
whether the first thread (running `add i store`) ran before or not, the `a + b == 0`
invariant no longer holds. Indeed, there's a bug in the `subSTM` function: instead of
substracting `i` from `storeB`, it should add it.

Sadly, at this point `tasty-dejafu` won't show which exception was raised
while checking invariants, and only displays `invariant failure`. I opened an
[issue](https://github.com/barrucadu/dejafu/issues/385) to maybe improve this.

## Conclusion
STM allows for safe concurrent access to shared mutable state with fine-grained
locking. However, when multiple variables are at play, defining invariants between
their values and ensuring these invariants aren't breached in some interleaving
of concurrent mutations is important. The DejaFu library can help to achieve this,
so building your code on the `concurrency` abstractions from the start allows to
write all kinds of tests later.

### Future Work
- The trick used above to use `Identity` as the `cell` type allows for immutable
  versions of the data type bundling all variables. However, the `Identity` wrapper
  is a bit of a burden when dissecting `FrozenStore`. It'd be nice if `cell a` could
  become a plain `a` removing the need to unpack the `Identity` constructor. I have
  a hunch this might be possible by using some closed type family, but didn't pursue
  this further yet.

- Implementing `snapshotWith` is quite boring. I imagine it's possible to have
  a generic version of it using `GHC.Generics`.

- Using the `concurrency` abstraction brings a `MonadConc` or `MonadSTM` constraint
  and hence a dictionary lookup at runtime. I'd love to see some benchmarks where
  a library is defined in terms of `MonadConc` and `MonadSTM`, but every function
  is `INLINEABLE` and/or `SPECIALIZE`d to `Control.Concurrent.STM.STM` and `IO` so
  there's, ideally, no performance hit for *regular application* consumers of the
  library vs. using plain `STM` and `IO` in the library code. [Update: [here's the
  answer]({% post_url 2023-04-14-concurrency-and-performance %})]
