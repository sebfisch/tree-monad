% Non-Determinism Monad for Tree Search
% Sebastian Fischer <sebf@informatik.uni-kiel.de>

This Haskell library provides an implementation of the `MonadPlus`
type class that represents the search space as a tree that can be used
to define different search strategies.

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

> full 1     = return 1
> full (n+1) = do i <- full n
>                 return (n-i) `mplus` return (i+1)

Here are some example calls of `full` with corresponding results:

~~~
*Main> full 1 :: SearchTree Int
One 1
*Main> full 2 :: SearchTree Int
Choice (One 0) (One 2)
*Main> full 3 :: SearchTree Int
Choice (Choice (One 2) (One 1)) (Choice (One 0) (One 3))
~~~

Performance
---

Although `SearchTree` is a full-fledged instance of `MonadPlus`, the
implementation of the bind operation is not optimal. If you are
interested in the details, then read Janis Voigtlaenders paper on
[Asymptotic Improvement of Computations over Free Monads][janis]. If
not, simply note that nesting calls of `>>=` to the left is bad for
performance.

[janis]: http://wwwtcs.inf.tu-dresden.de/~voigt/mpc08.pdf

There is a different type `Search` that is also an instance of `Monad`
and `MonadPlus` and does not suffer from this performance penalty. The
function

~~~ { .Haskell }
searchTree :: Search a -> SearchTree a
~~~

converts a value of type `Search a` into one of type `SearchTree
a`. As an aside, the implementation of `>>=` for `Search` is also more
efficient if it is not nested to the left because it does not involve
pattern matching on tree constructors.

Incidentally, the function `full` nests `>>=` to the left, so we can
use it to compare the performance of both monads. The following
example is borrowed from Janis's paper:

> zigzag :: SearchTree a -> a
> zigzag = zig
>  where
>   zig (One x)      = x
>   zig (Choice l _) = zag l
>
>   zag (One x)      = x
>   zag (Choice _ r) = zig r

This function descends a single path of a search tree and yields the
leaf at its end. Because of behaviour similar to the naive reverse
function in terms of `++`, the function `zigzag . full` has quadratic
run time. Similar to the solution in case of reverse where functional
lists can be used to achieve linear run time, the implementation of
`Search` uses continuations and the function `zigzag . searchTree
. full` has linear run time.

Applications
---

There is a blog post on [parallel tree search][par] that implements a
parallel version of depth-first search based on search trees using
explicit parallelism. The package [parallel-tree-search] on Hackage
uses the library described here to implement the function `parSearch`
shown there.

[par]: http://www-ps.informatik.uni-kiel.de/~sebf/haskell/speedup-search-with-parallelism.lhs.html
[parallel-tree-search]: http://hackage.haskell.org/cgi-bin/hackage-scripts/package/parallel-tree-search

Feedback
---

For bug reports or feedback contact [Sebastian Fischer][sebf].

[sebf]: mailto:sebf@informatik.uni-kiel.de
