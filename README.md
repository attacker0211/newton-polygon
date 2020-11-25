# newton-polygon

## Installation and build
* Prerequisite: [cabal and ghc](https://www.haskell.org/ghcup/)

```
git clone https://github.com/attacker0211/newton-polygon.git ~
cd newton-polygon
cabal v2-repl 
```
## Usage from repl
* `ppNewton x`: pretty-print newton polygons of bound `x`
* `ppNewtonL list`: pretty-print newton polygons of bounds from the `list`
* `ppNewtonGen numbranchA numBranchB sumA sumB bound`: general case, default `numBranchA = 3, numBranchB = 4, sumA = bound, sumB = 2*bound`
