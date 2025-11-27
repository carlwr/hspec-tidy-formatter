tested with Cabal version: `3.14.2.0`


## enable project settings suitable for local development

-> create a file:

```cabal
-- --- ./cabal.project.local ---
import: misc/dev.project
```


## re-generate README screenshot

-> see `./scripts/montage/Makefile`


## tests

test commands and what they target:

```sh
cabal test doctest       # the actual (few) tests there are
cabal test dev-example   # example tests suitable for dev iteration
cabal test --jobs=1      # all tests (the above + README tests)

```

## verify a `GHC==X.X.X` version for `<project>.cabal`

```sh
mv cabal.project.local{,_}  # disable
rm -rf dist-newstyle; cabal --with-compiler=ghc-X.X.X --prefer-oldest build
rm -rf dist-newstyle; cabal --with-compiler=ghc-X.X.X --prefer-oldest test
mv cabal.project.local{_,}  # re-enable
```
