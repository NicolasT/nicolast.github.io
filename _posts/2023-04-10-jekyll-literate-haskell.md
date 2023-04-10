---
title: "Using Literate Haskell with Jekyll"
excerpt: >
  With a little bit of configuration, Haskell code in Jekyll articles
  can be executed or loaded in a REPL: a great way to ensure the code
  actually works!
date: 2023-04-10 23:28 +0200
categories: "Various"
header:
  overlay_image: /assets/images/3776224595_9f783c6f32_c.jpg
  overlay_filter: 0.3
  caption: "Photo credit: [**Mandy Lackey**](https://www.flickr.com/photos/mandaloo/3776224595/)"
---
This blog is powered by the [Jekyll](https://jekyllrb.com/) static site generator, hosted on
[GitHub Pages](https://pages.github.com/).
With Jekyll, you write posts in
[Markdown](https://daringfireball.net/projects/markdown/syntax)-formatted files, which are
then rendered into HTML using some templates. When writing technical articles including code
examples, as an author I want to make sure these examples are correct, i.e., they compile
cleanly and, ideally, some tests pass. I assume the reader appreciates this as well (I know
I do!).

One way to tackle this is keeping the code in some source file(s), ensure these compile, and
copy-paste snippets into the Markdown source of an article. This works, but copy-paste leaves
room for error, and updating source-code while writing the article, then making sure all snippets
are updated correctly, is definitely error-prone.

Wouldn't it be nice if the article file itself would be executable (or, at least, checked by the
compiler)? Luckily this is possible, without all too much hassle! Given a working Jekyll
installation, here's what we want:

- A way to turn an article, including code blocks, into input the compiler can work with.
- A way to *run* the article file, or load it in a REPL, including any dependencies the code
  relies on.
- A way for Jekyll to include such articles in the rendered website.

## Compiling Markdown

The [GHC](https://www.haskell.org/ghc/) [Haskell](https://www.haskell.org/) compiler has
built-in support for Literate Haskell, which allows to interleave code blocks with text.
The native Literate Haskell support doesn't know about Markdown formatting, but with some
flags, GHC can be instructed to use some custom preprocessor which knows how to extract code
from some literate source file.

The [markdown-unlit](https://github.com/sol/markdown-unlit) package does exactly this for
Markdown-formatted files: it extracts all Haskell code blocks and passes these to the compiler.
So Markdown-formatted input like

````
Here's how to print the string "Hello, world" on the console:

```haskell
printHelloWorld :: IO ()
printHelloWorld = putStrLn "Hello, world"
```

Simple!
````

becomes

```haskell
printHelloWorld :: IO ()
printHelloWorld = putStrLn "Hello, world"
```

For GHC to use the `markdown-unlit` preprocessor, the `-pgmL` option must be used, and the
source file must have the `.lhs` file extension.

### Aside: Testing `README.md`

Next to running Markdown articles, `markdown-unlit` can be used to test your project's
`README.md`, as I do in the [`landlock`](https://github.com/NicolasT/landlock-hs/tree/main/landlock)
package. See the [this example](https://github.com/sol/markdown-unlit#extended-example)
for more information!

## Running Articles

Now we know how to extract code blocks from article source files for the compiler to process
them, we need a way to invoke the compiler (or launch it in REPL mode) with some source file.
Of course the `ghc` binary can be invoked as-is, passing the necessary `-pgmL` option and
any others, but this has several drawbacks, especially when the code in the article depends on
some other packages:

- `markdown-unlit` must be pre-installed
- We must not forget the `-pgmL` option, including the path to `markdown-unlit` unless it's
  installed in `$PATH`
- Any dependencies must be installed in a way for GHC to find them
- Dependencies may require `-package ...` or other options to be passed to GHC

In the Haskell ecosystem, the `cabal` tool is used to install dependencies, invoke
the compiler with the right options to find these dependencies, etc. In regular Haskell
packages, a Cabal project description file (`packagename.cabal`) contains all information
needed for `cabal` to do its job. A Jekyll article Markdown source file doesn't come with
a project description file, so how can the `cabal-install` functionality be reused? I recently
found out `cabal` nowadays has a *scripting mode*: it can read metadata, enclosed in a comment block,
from a source file, and use it accordingly. So, including the following in the Markdown source
of an article will instruct `cabal` to provide all dependencies (none but `base` for this
article), ensure `markdown-unlit` is available, and invoke `ghc` with the right options:

```haskell
{- cabal:
build-depends: base ^>=4.17
default-language: Haskell2010
build-tool-depends: markdown-unlit:markdown-unlit
ghc-options: -pgmL markdown-unlit -Wall -Werror
-}
```

The above snippet could be put in any kind of section, but I prefer to put it in a
`haskell` code-block, so it's retained in the output of `markdown-unlit`.

Next to `build-depends` and similar fields in a `cabal` section, configuration used in
a `cabal.project` file can be put in a `project` section. See
[the documentation](https://cabal.readthedocs.io/en/stable/getting-started.html#run-a-single-file-haskell-script)
for more details.

With all this in place, articles can be executed using `cabal run article.lhs`, or
even better, loaded in a REPL session, including all dependencies, using
`cabal repl article.lhs`.

## Rendering Literate Haskell Articles

By default, Jekyll won't read `.lhs` files in the `_drafts` or `_posts` folders as Markdown
files. By default, it will only read files with the `.md` extension (and several others)
as Markdown. To treat `.lhs` files the same way, add the following to your site's
`_config.yml`:

```yaml
markdown_ext: "markdown,mkdown,mkdn,mkd,md,lhs"
```

Any `.lhs` files in the `_drafts`, `_posts` and other folders will now be rendered as Markdown
sources.

## Finishing Up

Finally, let's wrap things up for this particular article:

```haskell
main :: IO ()
main = printHelloWorld
```

and indeed:

```
$ cabal run _drafts/jekyll-literate-haskell.lhs
Hello, world
```

Of course, this could launch a [tasty](https://hackage.haskell.org/package/tasty) test-suite
or similar, see [here]({% post_url 2023-04-06-testing-concurrent-code-using-dejafu %}#testing)
for an example.

Happy hacking^Wwriting!
