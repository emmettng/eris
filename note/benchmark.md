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
  - #### understanding this report
    ```
    'name'              'estimate'  '(95% boundary)'
    benchmarking fib/1
    time                 6.757 ns   (6.746 ns .. 6.772 ns)
                         1.000 R²   (1.000 R² .. 1.000 R²) -- 0.99 and 1
    mean                 6.759 ns   (6.754 ns .. 6.769 ns)
    std dev              24.06 ps   (16.05 ps .. 37.75 ps)
    ```
  - #### issue        [#496](https://github.com/commercialhaskell/stack/issues/496)  
    generate a html chart report as being introduced in [this tutorial](http://www.serpentine.com/criterion/tutorial.html).
    ```
    stack bench --benchmark-arguments '--output=whatevername.html'
    ```
- ### Tutorial & Summary
  - [Introduction](http://www.serpentine.com/criterion/tutorial.html)
  - Summary : TODO
