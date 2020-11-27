# HaskellExperiments
Scratchpad for Haskell Club

Early adventures into learning Haskell together with Nudge Team.

### Resources
 - [Learn You a Haskell For Great Good!](http://learnyouahaskell.com/chapters)
 - [CIS 194 2013](https://www.cis.upenn.edu/~cis194/spring13/)

### Project Setup
- [Install GHCI and Cabal](https://www.haskell.org/downloads/)

### Running Unit Tests
```
$ cabal configure --enable-tests --test-show-details=direct
$ cabal build
$ cabal test
```