---
layout: post
title: The Indexed State Monad
category: articles
tags: [haskell, programming]
---

Once in a while when writing [Haskell](http://www.haskell.org) code, it makes
sense to use some form of state in a computation, with corresponding actions to
retrieve this state and modify it.

The go-to solution for this is the `State` monad.

State
=====
To get started, we'll walk through the `State` monad and the default action it
provides. If you're familiar with this, feel free to skip this.

Actions running within some state have the according type `State s r`, where `s`
is the type of the state available to the computation, and `r` is the return
type of the action.

A couple of basic actions are provided:

{%highlight haskell %}
get :: State s s
put :: s -> States s ()
state :: (s -> (r, s)) -> State s r
modify :: (s -> s) -> State s ()

runState :: State s r -> s -> (r, s)
{% endhighlight %}

In `mtl`, all of this is polymorphic using a type-class, but I changed things a
bit to use the `State` instance as-is.

This allows writing code like

{% highlight haskell %}
updateCounter :: Int -> State Int Int
updateCounter incr = do
    current <- get
    put $ current + incr
    return current

updateCounter' :: Int -> State Int Int
updateCounter' incr = state (\current -> (current, current + incr))
{% endhighlight %}

Indexed State
=============
While `State` is pretty useful once in a while, it has a major drawback: an
action running in it can't change the *type* of the state. One can work around
this by wrapping the different types in a datatype with different constructors,
but that's rather clumsy, even ugly.

Here the *indexed* state monad comes to the rescue: unlike `State` which is
parametrized over a single type, its indexed cousin `IxState` is parametrized
over two distinct types. An action of type `IxState i o r` has *input* state of
type `i`, and *output* state of type `o`, so it can *change* the type of the
state.

Here are some of the actions defined in `Control.Monad.Indexed.State`, also
adapted to use the `IxState` instance instead of `IxMonadState`:

{% highlight haskell %}
iget :: IxState i i i
iput :: o -> IxState i o ()
imodify :: (i -> o) -> IxState i o ()

runIxState :: IxState i o r -> i -> (r, o)
{% endhighlight %}
