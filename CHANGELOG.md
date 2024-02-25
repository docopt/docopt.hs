### 0.7.0.8

- Add support for `containers-0.7` [#58](https://github.com/docopt/docopt.hs/pull/58), [#60](https://github.com/docopt/docopt.hs/pull/60)

- Extend Template Haskell Quasi-quotation support to GHC 8.0-8.6. Template Haskell support is no longer optional. The package now supports all GHC's from 8.0 to 9.8. [#56](https://github.com/docopt/docopt.hs/pull/56), [#58](https://github.com/docopt/docopt.hs/pull/58)

### 0.7.0.7

- update bounds, fix warnings, require ghc 8.0+

### 0.7.0.6

- Fixes issue causing compilation error to happen with ghc-8.8.2 [#33](https://github.com/docopt/docopt.hs/issues/33), [#34](https://github.com/docopt/docopt.hs/pull/34)

### 0.7.0.5

- Fix an issue where in some cases pattern lines were matched out of order [#16](https://github.com/docopt/docopt.hs/issues/16)
- Strip leading & trailing newlines from usage, for quasiquoter ease [#28](https://github.com/docopt/docopt.hs/issues/28)
- Fix tests run against latest aeson 1.0.2.0 [#29](https://github.com/docopt/docopt.hs/issues/29)

### 0.7.0.4

- Fix the test suite when run from a distributed tarball [#21](https://github.com/docopt/docopt.hs/pull/21)
- Make the test suite more developer-friendly

### 0.7.0.3

- Fix `isPresent` treatment of repeatable arguments/options [#15](https://github.com/docopt/docopt.hs/issues/15)
- Fix build failure for stackage inclusion [#20](https://github.com/docopt/docopt.hs/pull/20)

### 0.7.0.2

- Minor docs/README tweaks [#13](https://github.com/docopt/docopt.hs/issues/13)

### 0.7.0.1

- Fix docs in README and in Docopt.hs

# 0.7.0.0

- Add usage parsing QuasiQuoters [#7](https://github.com/docopt/docopt.hs/pull/7)
  - Add `docopt` usage parsing QuasiQuoter
  - Add `docoptFile` usage parsing QuasiQuoter
  - Add `System.Docopt.NoTH` module
    - Add `parseUsage`
    - Add `parseUsageOrExit`
- New API organization [#10](https://github.com/docopt/docopt.hs/issues/10)
  - Remove `optionsWithUsage`
  - Remove `optionsWithUsageDebug`
  - Remove `optionsWithUsageFile`
  - Remove `optionsWithUsageFileDebug`
  - Add `Docopt` type to represent a parsed usage string
  - Add `usage`
  - Add `parseArgs`
  - Add `parseArgsOrExit`
  - Add `exitWithUsage`
  - Add `exitWithUsageMessage`
  - Monomorphize `getArg` from `Monad m` to `Maybe`
  - Add `getArgOrExitWith`
  - Deprecate `getAllArgsM`
  - Deprecate `notPresentM`
  - Deprecate `isPresentM`
  - Deprecate `getFirstArg`
- Add thorough haddock API documentation
  
### 0.6.0.2

- Make `argument` not require its named option wrapped in angle brackets. [#4](https://github.com/docopt/docopt.hs/pull/4), [#5](https://github.com/docopt/docopt.hs/pull/5)

### 0.6.0.1

- Fix haddock docs.

# 0.6.0.0

First release! Tracks features of reference Python implementation at version `0.6`.
