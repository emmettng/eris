# Benchmark Note

## criterion

- ### stack & Cabal
  - package.yaml
  ```
  benchmarks:  
    fib-bench:  
      main:               TestFib.hs
      source-dirs:        bench/SimpleBench
      dependencies:
      - criterion
  ```
  TestFib.hs is module Main as in test

  - .cabal
  ```
  benchmark fib-bench
  type: exitcode-stdio-1.0
  main-is: TestFib.hs
  hs-source-dirs:
      bench/SimpleBench
  build-depends:
      base >=4.7 && <5
    , criterion
    , testCriterion
  other-modules:
      Paths_testCriterion
  default-language: Haskell2010
  ```
- ### Criterion chart report
  #### issue [#496](https://github.com/commercialhaskell/stack/issues/496)
  ```
  stack bench --benchmark-arguments '--output=whatevername.html'
  ```
- ### Tutorial & Summary
  - [Beginning](http://www.serpentine.com/criterion/tutorial.html)
  - Summary : TODO 
