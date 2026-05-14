muttest::muttest_plan(
  source_files = "R/classify.R",
  mutators = muttest::condition_mutations()
)
