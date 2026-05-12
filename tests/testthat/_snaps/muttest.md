# session with ProgressReporter prints results

    Code
      print(p)
    Output
      -- Mutation Test Plan ----------------------------------------------------------
      2 mutants across 1 file
      
      R/calculate.R  + → -
      R/calculate.R  * → /
    Code
      print(result)
    Output
      
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 1 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]
      
      -- Survived Mutants ------------------------------------------------------------
      calculate.R  + → -
        2-   (x + y) * 0
        2+   (x - y) * 0

