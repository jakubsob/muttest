# muttest_plan can first n mutants

    Code
      print(p, nrows = 1)
    Output
      Mutation test plan with 2 mutants across 1 files:
      filename mutator
      R/calculate.R  + -> -
      ... and 1 more mutants

# muttest_plan prints all mutants

    Code
      print(p, nrows = 2)
    Output
      Mutation test plan with 2 mutants across 1 files:
      filename mutator
      R/calculate.R  + -> -
      R/calculate.R  + -> -

