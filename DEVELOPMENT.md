# Development

tested with Cabal version: `3.14.2.0`

### Enable project settings suitable for local development

-> create a file:

```haskell
--- ./cabal.project.local ---
import: misc/dev.project
```

### Tests

Test commands and what they target:

```sh
cabal test doctest       # the actual (few) tests there are
cabal test dev-example   # example tests suitable for dev iteration
cabal test --jobs=1      # all tests (the above + README tests)
```


# Maintainer

Bump version:
```sh
vim <proj>.cabal          # edit "version: " field
git add <proj>.cabal
git commit
git tag -m 0.0.1 0.0.1    # will tag the latest commit
git push --tags

```

Release/upload:
```sh
# prepare:
cabal haddock --haddock-for-hackage
cabal sdist --list-only
cabal check

# upload to hackage:
f=$(cabal sdist|grep '\.gz') && cabal upload           $f  # candidate
f=$(cabal sdist|grep '\.gz') && cabal upload --publish $f
```

Verify a `GHC==X.X.X` version for `<project>.cabal`:

```sh
mv cabal.project.local{,_}  # disable
rm -rf dist-newstyle; cabal --with-compiler=ghc-X.X.X --prefer-oldest build
rm -rf dist-newstyle; cabal --with-compiler=ghc-X.X.X --prefer-oldest test
mv cabal.project.local{_,}  # re-enable
```

Re-generate README screenshot:
* -> see `./scripts/montage/Makefile`
