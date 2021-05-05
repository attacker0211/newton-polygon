# newton-polygon

## Installation and build
* Prerequisite: [cabal and ghc](https://www.haskell.org/ghcup/)

```
git clone https://github.com/attacker0211/newton-polygon.git ~/newton-polygon 
(or git pull origin master to update)
cd newton-polygon
cabal v2-install
cabal v2-repl 
```
## Usage from repl
* `ppNewton x`: pretty-print newton polygons of bound `x`
* `ppNewtonL list`: pretty-print newton polygons of bounds from the `list`
* `ppNewtonGen numbranchA numBranchB multiplicativeA multiplicativeA bound`: general case, default `numBranchA = 3, numBranchB = 4, sumA = bound*multiplicativeA, sumB = bound*multiplicativeB`
* `ppNewtonGenL numbranchA numBranchB multiplicativeA multiplicativeB list_of_bounds`: general case
* `ppNewtonMonoGen bound ramies`: single case, e.g `ppNewtonMonoGen 7 [2,4,4,4]`
