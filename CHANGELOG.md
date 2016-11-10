### 0.7.0.5

- Fix an issue where in some cases pattern lines were matched out of order [#16]
- Strip leading & trailing newlines from usage, for quasiquoter ease [#28]
- Fix tests run against latest aeson 1.0.2.0 [#29]

### 0.7.0.4

- Fix the test suite when run from a distributed tarball [#21]
- Make the test suite more developer-friendly

### 0.7.0.3

- Fix `isPresent` treatment of repeatable arguments/options [#15]
- Fix build failure for stackage inclusion [#20]

### 0.7.0.2

- Minor docs/README tweaks [#13]

### 0.7.0.1

- Fix docs in README and in Docopt.hs

# 0.7.0.0

- Add usage parsing QuasiQuoters [#7]
  - Add `docopt` usage parsing QuasiQuoter
  - Add `docoptFile` usage parsing QuasiQuoter
  - Add `System.Docopt.NoTH` module
    - Add `parseUsage`
    - Add `parseUsageOrExit`
- New API organization [#10]
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

- Make `argument` not require its named option wrapped in angle brackets. [#4, #5]

### 0.6.0.1

- Fix haddock docs.

# 0.6.0.0

First release! Tracks features of reference Python implementation at version `0.6`.
