---
title: "xinU, a Haskell Experiment"
excerpt: >
    Thanks to its extensive FFI support, Haskell is a great platform to develop
    system applications. The `unix` library exposes a large set of POSIX APIs
    to Haskell code. However, it comes with some drawbacks. In this article,
    we'll dive into some of them, and look at some experiments in the `xinu`
    package to alleviate them.
date: 2022-09-16 00:01 +0100
categories: "Development"
header:
  overlay_image: /assets/images/5984972724_63465763ac_k.jpg
  caption: "Photo credit: [**Daniel Spiess**](https://www.flickr.com/photos/deegephotos/5984972724/)"
---
Even though [Haskell](https://www.haskell.org) is considered to be a high-level
programming language, it's a good fit for various system-level applications and
services as well, similar to how [Go](https://go.dev) is used for many
applications.

A lot of functionality can be implemented in high-level user-space libraries,
but at some point we must interact with the system and its kernel through
*[syscalls](https://en.wikipedia.org/wiki/System_call)*. In the
[GHC](https://www.haskell.org/ghc/) Haskell ecosystem, we use the wrappers
provided by the system C library instead of directly calling into the kernel,
which doesn't take a lot of effort thanks to GHC's excellent [Foreign Function
Interface](https://wiki.haskell.org/Foreign_Function_Interface) support.
Exposing the
```c
int openat(int dirfd, const char *pathname, int flags)
```
function from *libc* to Haskell code is as simple as

```haskell
foreign import capi safe "fcntl.h openat"
    c_openat :: CInt -> CString -> CInt -> IO CInt
```

Now Haskell code can call `c_openat` to open a file.

The above code is, however, a bit too low-level for most practical purposes:

- When `openat` fails, it returns `-1` and `errno` is set. Checking whether the
  return value of the `c_openat` action is `-1` is not very Haskell'esque: this
  is where we expect an exception to be thrown.
- The
  [`CInt`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Foreign-C-Types.html#t:CInt)
  type is very generic. Instead, we'd like to use a type specific to
  file-descriptors, so a corresponding `read`, `write` or `close` action will
  take such type as an argument, not any arbitrary `CInt`.
- `c_openat` takes a
  [`CString`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Foreign-C-String.html#t:CString)
  argument, which is a `Ptr CChar`. In high-level code, this is not a type we
  regularly work with. An API using a less clumsy path type would be welcome.

Luckily, there's a popular library in the ecosystem which provides just that:
[`unix`](https://hackage.haskell.org/package/unix). In its
[`System.Posix.IO`](https://hackage.haskell.org/package/unix-2.8.0.0/docs/System-Posix-IO.html)
module, it provides

```haskell
openFdAt :: Maybe Fd -> FilePath -> OpenMode -> OpenFileFlags -> IO Fd
```

which is a wrapper around `c_openat`. It's using the [`Fd`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Posix-Types.html#t:Fd)
type where applicable, it will throw an [`IOError`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#t:IOError)
when the call fails using handy utility functions like
```haskell
Foreign.C.Error.throwErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
```
and takes a
[`FilePath`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html#t:FilePath) (a.k.a. `String`) as argument, internally turning this into a
`CString` for the lifetime of the call to `c_openat` using
```haskell
Foreign.C.String.withCString :: String -> (CString -> IO a) -> IO a
```

## Some problems with `unix`
The `unix` package has been serving the Haskell ecosystem well for many years
(and, without a doubt, for many years to come). However, it's not without its
shortcomings:

- The `FilePath` type (as used in, e.g., the `System.Posix.IO` module) being a
  `String` comes with performance implications. Hence, the `System.Posix.IO`
  module was cloned into
  [`System.Posix.IO.ByteString`](https://hackage.haskell.org/package/unix-2.8.0.0/docs/System-Posix-IO-ByteString.html)
  and reworked to use `ByteString` values as paths. This duplication requires
  certain code changes to be applied in multiple modules.
- Neither `FilePath` nor `ByteString` are suitable types for paths, because of
  encoding issues. Hence, new clones of applicable modules were created now
  using
  [`PosixPath`](https://hackage.haskell.org/package/unix-2.8.0.0/docs/System-Posix-PosixString.html#t:PosixPath)
  values (e.g., `System.Posix.IO.PosixString`), further increasing
  code-duplication.
- The `unix` library exposes functions that are not necessarily available on all
  supported platforms, e.g., the
  [WASM](https://webassembly.org/)/[WASI](https://wasi.dev/) platform lacks
  some. Availability of library functions is checked using a `configure` script
  at build time, but when some library function is not found, the corresponding
  binding in `unix` is implemented, unconditionally, as
  ```haskell
  ioError (ioeSetLocation unsupportedOperation "...")
  ```
  This breaks the "If it compiles, it works" mantra, since any call to the
  function will most definitely not work.
- `unix` provides somewhat-high-level access the system functions, though one
  can argue the wrappers are, sometimes, too high-level, and lower-level
  interfaces are not made available. As an example, the `openAt` call above
  takes an [`OpenFileFlags`](https://hackage.haskell.org/package/unix-2.8.0.0/docs/System-Posix-IO.html#t:OpenFileFlags)
  argument which is a structure whose fields are mapped to bits in the `flags`
  argument of `openat`. However, if one want to use a *flag* that's not
  available in `OpenFileFlags` (say, `O_PATH` on a Linux system), this is not
  possible. This forced me to implement another [set of bindings for
  `openat`](https://github.com/NicolasT/landlock-hs/blob/b5638684869ad4f85bea53f10a3f0b921f000202/landlock/internal/System/Landlock/OpenPath.hsc)
  in [`landlock-hs`](https://github.com/NicolasT/landlock-hs): not a lot of
  effort, but duplication nonetheless.
- Most system functions are effectful (that's why they exist in the first
  place), so a big part of the `unix` API lives in `IO`. When working with monad
  stacks layered on top of `IO`, this requires a lot of `liftIO`ing.

## The `xinu` experiment
To experiment with alternative implementation strategies to expose system
functions to Haskell code, I created the [`xinu`](https://github.com/NicolasT/xinu)
package. Actually, two packages:

- `xinu-ffi`, which exposes FFI bindings to library functions, using a
  `configure` script to detect system capabilities. If a library function is not
  found at `configure` time, `xinu-ffi` will not provide a binding to it. Hence,
  the library API can depend on the build environment. However, a `xinu-ffi.h`
  header file is installed so dependent package can detect availability of
  functions using the `CPP` language extension.

- `xinu`, and a couple of internal libraries, which expose higher-level APIs to
  a developer.

The `xinu` library, similar to `unix`, supports multiple path types. However,
unlike `unix`, this is not implemented by copying the modules. Instead, it
leverages GHC's
[*Backpack*](https://gitlab.haskell.org/ghc/ghc/-/wikis/backpack) feature,
which brings ML module functor-like functionality to Haskell. Basically,
Backpack allows us to write code, abstracted over a module for which we only
provide the signature (i.e., the types and functions it must expose). Then, at
build time, we can specify one or more implementations of this signature and
create instanciations of the abstract module. Hence, without any code
duplication, there's `System.Xinu.IO.FilePath` and `System.Xinu.IO.ByteString`,
both instanciations of `System.Xinu.IO.Abstract` (over
`System.Xinu.Path.FilePath` and `System.Xinu.Path.ByteString`). The latter are
two modules implementing a rather simple signature:

```haskell
signature System.Xinu.Path (
      Path
    , toString
    , withPath
    ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.String (CString)

data Path

-- Execute an action, passing the given Path as a CString.
withPath :: (MonadIO m, MonadMask m) => Path -> (CString -> m a) -> m a

-- Used for error reporting.
toString :: Path -> String
```

Unlike the `unix` library `xinu` will expose a different API depending on
library function availability of the build environment (based on findings of
`xinu-ffi`'s `configure` script). Similar to `xinu-ffi`, it provides a `xinu.h`
header file, so dependents who care about availability of functions (e.g., to
provide different implementations based on what's available) can use `CPP`.

Finally, `xinu` functions aren't `IO` (though they're `SPECIALIZE`d for it).
Instead, it relies on the `MonadIO` type-class to `liftIO` `xinu-ffi` functions
where necessary. Furthermore, errors are reported and (where applicable) safely
handled using the `MonadThrow` and `MonadMask` type-classes from the
[`exceptions`](https://hackage.haskell.org/package/exceptions) package. This
allows `xinu` functions to be used within arbitrary monad stacks (assuming an
instance of `MonadIO` and `MonadThrow`/`MonadMask` is present).

So, `xinu-ffi` exposes
```haskell
System.Xinu.IO.FFI.c_openat :: Int32 -> CString -> Int32 -> Word32 -> IO Int32
```
while `xinu` exposes, among others,
```haskell
System.Xinu.IO.FilePath.openat :: (MonadIO m, MonadMask m)
                               => Maybe Int32
                               -> FilePath
                               -> Int32
                               -> Maybe Word32
                               -> m Int32
```
and
```haskell
System.Xinu.IO.ByteString.openat :: (MonadIO m, MonadMask m)
                                 => Maybe Int32
                                 -> ByteString
                                 -> Int32
                                 -> Maybe Word32
                                 -> m Int32
```
Of course, the intent is to use higher-level types (like `Fd`) instead. This
should be fairly simple to do, especially using
[`coerce`](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Coerce.html#v:coerce)
since indeed, we expect (or rather, require) such `Fd` to be equal to an `Int32`
(at least on this platform).

## Learnings and Questions
It's a bit too soon to know where this experiment could be heading. However,
some early experiences:

- Backpack doesn't work when a package has `Build-Type` `Configure`, which is a
  bummer: it forces `xinu-ffi` to be a separate package, which increases
  maintenance burden. If it could be a (public) internal library, this would
  make things quite a bit easier.
- A multi-package repository (using `cabal.project`) and Backpack seems to
  trigger a dependency resolution issue in Cabal, causing [`cabal install
  --dependencies-only all` to
  fail](https://github.com/NicolasT/xinu/actions/runs/3060684207/jobs/4939496825#step:20:1).
- `stack` doesn't support Backpack. This is a [known
  issue](https://github.com/commercialhaskell/stack/issues/2540)
  but limits adoption of Backpack in the ecosystem, which is a shame, since
  ML-style module functors are a great way to reduce code duplication without
  incurring any runtime performance overhead.
- Haddock doesn't like `Mixins`, internal libraries and `Reexported-Modules`.
  Basically, no decent API documentation of `xinu` can be generated. There are
  [several](https://github.com/haskell/haddock/issues/563) [related](https://github.com/haskell/haddock/issues/958)
  [issues](https://github.com/haskell/cabal/issues/4905)
  [filed](https://github.com/haskell/cabal/issues/6035) upstream.
- Given [the new `OsPath` family of
  types](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html),
  does it make sense to keep providing `FilePath`, `ByteString` and other
  implementations?
- More tests to ensure exception handling is working as desired are needed. How
  is this code different from the
  [`unliftio`](https://hackage.haskell.org/package/unliftio) package?

## Conclusion
Even though `xinu` is in no way meant to be as extensive as `unix`, it's always
interesting to explore different approaches to a given problem, especially when
new functionality can be leveraged which wasn't available when the original
solution was coded.

I'd love to hear your feedback. Would a library like `xinu` be of any use to
you? What's missing? Head to the repository's
[Discussions](https://github.com/NicolasT/xinu/discussions) section!
