muttest::muttest_plan(
  source_files = "R/calculate.R",
  mutators = muttest::arithmetic_operators()
)
