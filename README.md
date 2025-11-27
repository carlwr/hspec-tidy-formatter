# `hspec-tidy-formatter`

_A custom [`hspec`] formatter for terminal output._

![GitHub License](https://img.shields.io/github/license/carlwr/hspec-tidy-formatter)
![Hackage Version](https://img.shields.io/hackage/v/hspec-tidy-formatter)

<br>

<p align="center">
  <ins>left</ins>: this formatter, <ins>right</ins>: hspec default<br>
</p>

![Screenshot](https://raw.githubusercontent.com/carlwr/hspec-tidy-formatter/refs/heads/main/assets/montage.png)

<p align="center">
  <sub>(color choices: &copy;<code>hspec</code>&nbsp;&nbsp;<del>me</del>&nbsp;&nbsp;<del>terminal theme</del>)</sub>
</p>
<br>

---

The formatter should work with with any test runner backend. It may be particularly useful with [Hedgehog] tests through [`hspec-hedgehog`]: it omits the _"passed 100 tests."_ otherwise printed after each spec item, yet includes manually added test output (`Hedgehog.collect`, `Hedgehog.label` etc.).


<!--
```haskell
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- module Main (main) where
-- import Prelude

```
-->


## How to enable

### With `hspec-discover`

Place a file `SpecHook.hs` in the source directory of the test component:

```haskell
-- --- test/SpecHook.hs ---

import Test.Hspec
import qualified Test.Hspec.TidyFormatter as TidyFormatter

hook :: Spec -> Spec

-- use by default:
hook = TidyFormatter.use

-- to instead use only if requested (`hspec --format=tidy`):
-- hook = TidyFormatter.register
```

`hspec --help` can be used to inspect which formatters `hspec` is aware of:

```diff
 $ cabal test hspec --test-options=--help | grep -A3 FORMATTER

 FORMATTER OPTIONS
   -f NAME  --format=NAME     use a custom formatter; can be one of
-                             checks, specdoc, progress,
+                             checks, specdoc, progress, tidy
                              failed-examples or silent
```

### By modifying `Spec`-s directly

```haskell
main :: IO ()
main = hspec . TidyFormatter.use $ spec

spec :: Spec
spec = it "adds" $ 1+1 `shouldBe` (2::Int)
```


## Functionality, options

* supports transient output/progress

* honors most `hspec` options, including: `--times`, `--no-unicode`, `--no-color`, `--print-cpu-time`, `--print-slow-items=[=N]`

#### NOTE:
`hspec` allows test runners to pass it additional text together with the outcome of each test. This formatter, by default, prints such text only if it spans more than one line. To instead always print all such text unconditionally, use the `--times` option. E.g. with [`hspec-hedgehog`], which passes the number of tests run for each item as a single line of text:

```diff
- $  hspec --format=tidy
+ $  hspec --format=tidy --times

  [...]
    is Applicative-lawful per
-     [✔] Identity
+     [✔] Identity  (5ms) (passed 100 tests.)
```



<!-- links -->

[Hedgehog]: https://hackage.haskell.org/package/hedgehog
[`hspec`]: http://hspec.github.io
[`hspec-hedgehog`]: https://hackage.haskell.org/package/hspec-hedgehog
