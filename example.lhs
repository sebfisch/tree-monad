% Non-Determinism Monad for Tree Search
% Sebastian Fischer <sebf@informatik.uni-kiel.de>

This Haskell library provides an implementation of the `MonadPlus`
type class that represents the search space as a tree whose
constructors represent `mzero`, `return`, and `mplus`.

Installation
---

This library is on [Hackage]. The easiest way to install it is to use
[cabal-install] and simply type

~~~
$ cabal install tree-monad
~~~

in a shell.

[Hackage]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/tree-monad
[cabal-install]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall

Usage
---

To use this library import it as follows.

<div class="nodisplay">

> import Control.Monad

</div>

> import Control.Monad.SearchTree

Now monadic actions can be represented as trees. For example, the
monadic action

~~~ { .Haskell }
mzero `mplus` return 42
~~~

describes the following search tree.

~~~ { .Haskell }
Choice None (One 42)
~~~

Different search strategies can be implemented as traversals of this
tree structure. For example, depth-first search looks as follows:

> dfs :: SearchTree a -> [a]
> dfs None         = []
> dfs (One x)      = [x]
> dfs (Choice l r) = dfs l ++ dfs r

Because `SearchTree` is a monad, we can use `do`-notation to build
trees. For example, consider the following function that creates a
full binary tree of given height.

> full :: Int -> SearchTree Int
> full 1 = return 1
> full n = do m <- full (n-1)
>             return (2*m) `mplus` return (2*m+1)

