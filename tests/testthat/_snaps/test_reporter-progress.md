# progress reporter shows all killed

    Code
      muttest(plan, reporter = ProgressMutationReporter$new(min_time = Inf,
        survived_detail = "none"))
    Output
      i Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File
      x |   0 |   1 |   0 |   1 |   0 | + → -    | calculate.R
      -- Results ---------------------------------------------------------------------
      [ KILLED 0 | SURVIVED 1 | ERRORS 0 | TOTAL 1 | SCORE 0.0% ]

# progress reporter shows survived mutants inline

    Code
      muttest(plan, reporter = ProgressMutationReporter$new(min_time = Inf,
        survived_detail = "inline"))
    Output
      i Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File
      v |   1 |   0 |   0 |   1 | 100 | * → /    | calculate.R
      x |   1 |   1 |   0 |   2 |  50 | + → -    | calculate.R
      calculate.R  + → -
      2-   (x + y) * 0
      2+   (x - y) * 0
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 1 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

# progress reporter shows survived mutants in summary

    Code
      muttest(plan, reporter = ProgressMutationReporter$new(min_time = Inf,
        survived_detail = "summary"))
    Output
      i Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File
      v |   1 |   0 |   0 |   1 | 100 | * → /    | calculate.R
      x |   1 |   1 |   0 |   2 |  50 | + → -    | calculate.R
      -- Survived Mutants ------------------------------------------------------------
      calculate.R  + → -
      2-   (x + y) * 0
      2+   (x - y) * 0
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 1 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

# progress reporter shows survived mutants in both

    Code
      muttest(plan, reporter = ProgressMutationReporter$new(min_time = Inf,
        survived_detail = "both"))
    Output
      i Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File
      v |   1 |   0 |   0 |   1 | 100 | * → /    | calculate.R
      x |   1 |   1 |   0 |   2 |  50 | + → -    | calculate.R
      calculate.R  + → -
      2-   (x + y) * 0
      2+   (x - y) * 0
      -- Survived Mutants ------------------------------------------------------------
      calculate.R  + → -
      2-   (x + y) * 0
      2+   (x - y) * 0
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 1 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

# progress reporter shows doesn't show survived mutants

    Code
      muttest(plan, reporter = ProgressMutationReporter$new(min_time = Inf,
        survived_detail = "none"))
    Output
      i Mutation Testing
      |   K |   S |   E |   T |   % | Mutator  | File
      v |   1 |   0 |   0 |   1 | 100 | * → /    | calculate.R
      x |   1 |   1 |   0 |   2 |  50 | + → -    | calculate.R
      -- Results ---------------------------------------------------------------------
      [ KILLED 1 | SURVIVED 1 | ERRORS 0 | TOTAL 2 | SCORE 50.0% ]

