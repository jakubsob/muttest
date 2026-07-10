# session with ProgressReporter prints results

    Code
      print(p)
    Output
      -- Mutation Test Plan ----------------------------------------------------------
      2 mutants across 1 file
      
      R/shipping.R  > → <
      R/shipping.R  > → >=
    Code
      print(result)
    Output
      
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 1 | NO COVERAGE 0 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]
      
      -- Survived Mutants ------------------------------------------------------------
      shipping.R  > → >=
        2-   if (weight_kg > 5) 15.00 else 5.00
        2+   if (weight_kg >= 5) 15.00 else 5.00

# no-coverage mutants are reported and excluded from the score

    Code
      print(p)
    Output
      -- Mutation Test Plan ----------------------------------------------------------
      2 mutants across 2 files
      
      R/shipping.R  > → <
      R/discount.R  - → +
    Code
      print(result)
    Output
      
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 0 | NO COVERAGE 1 | ERRORS 0 | TOTAL 2 | SCORE 100.0% ]

