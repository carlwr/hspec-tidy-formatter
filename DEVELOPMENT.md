# Development

tested with Cabal version: `3.14.2.0`

### Enable project settings suitable for local development

-> create a file:
```haskell
--- ./cabal.project.local ---
import: misc/dev.project
```

If a cabal.project.local is present it can be ignored with:
```sh
cabal --project-file=cabal.project.no-local ..
```

### Tests

Test commands and what they target:
```sh
cabal test doctest       # the actual (few) tests there are
cabal test dev-example   # example tests suitable for dev iteration
cabal test --jobs=1      # all tests (the above + README tests)

stack test --flag hspec-tidy-formatter:doctest
                         # build and test with stack

stack test --flag hspec-tidy-formatter:doctest --snapshot nightly
                         # ...with latest nightly snapshot

```

Useful command to run all tests in a robust way, without transitive output:
```sh
BUILDKITE=true command cabal test -j1
```

Note: `BUILDKITE=true` is a hack to disable transitive output (`hspec` checks the presence and value of this environment variable before running tests).

Run dev-example tests with explicit, but default, options; and (necessarily) run doctests separately:
```sh
cabal build && command cabal -v0 test doctest && command cabal -v0 test test:dev-example -j1 --test-options='--no-times --no-expert'
```

# Maintainer

Bump version:
```sh
vim <proj>.cabal              # edit "version: " field
git add <proj>.cabal
git commit -m 'chore: set version to 0.0.1.0'
git tag -m 0.0.1.0 0.0.1.0    # will tag the latest commit
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

Verify `tested-with` GHC versions:
```sh
./scripts/test-tested-with
```

Verify exercising `doctest` flag, with any `cabal.project.local` disabled:
```sh
cabal --project-file=cabal.project.no-local build --flags='hspec-tidy-formatter -doctest' && \
cabal --project-file=cabal.project.no-local test  --flags='hspec-tidy-formatter -doctest' && echo "OK\n" && \
cabal --project-file=cabal.project.no-local build --flags='hspec-tidy-formatter +doctest' && \
cabal --project-file=cabal.project.no-local test  --flags='hspec-tidy-formatter +doctest' && echo "OK\n"
```

Trigger CI manually:
```sh
gh workflow run ci.yml --ref <branch>
```


Re-generate README screenshot:
* -> see `./scripts/montage/Makefile`
