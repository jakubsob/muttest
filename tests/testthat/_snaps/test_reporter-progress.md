# progress reporter shows all killed

    Code
      muttest(plan, reporter = ProgressMutationReporter$new(min_time = Inf,
        survived_detail = "none"))
    Output
      i Mutation Testing
      |   K |   S |   N |   E |   T |   % | Mutator  | File
      v |   1 |   0 |   0 |   0 |   1 | 100 | > → <    | shipping.R
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 0 | NO COVERAGE 0 | ERRORS 0 | TOTAL 1 | SCORE 100.0% ]

# progress reporter shows survived mutants in summary

    Code
      muttest(plan, reporter = ProgressMutationReporter$new(min_time = Inf,
        survived_detail = "summary"))
    Output
      i Mutation Testing
      |   K |   S |   N |   E |   T |   % | Mutator  | File
      v |   1 |   0 |   0 |   0 |   1 | 100 | > → <    | shipping.R
      x |   1 |   1 |   0 |   0 |   2 |  50 | > → >=   | shipping.R
      -- Survived Mutants ------------------------------------------------------------
      shipping.R  > → >=
      2-   if (weight_kg > 5) 15.00 else 5.00
      2+   if (weight_kg >= 5) 15.00 else 5.00
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 1 | NO COVERAGE 0 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

# progress reporter doesn't show survived mutant details

    Code
      muttest(plan, reporter = ProgressMutationReporter$new(min_time = Inf,
        survived_detail = "none"))
    Output
      i Mutation Testing
      |   K |   S |   N |   E |   T |   % | Mutator  | File
      v |   1 |   0 |   0 |   0 |   1 | 100 | > → <    | shipping.R
      x |   1 |   1 |   0 |   0 |   2 |  50 | > → >=   | shipping.R
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 1 | NO COVERAGE 0 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

