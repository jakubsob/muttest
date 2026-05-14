muttest::muttest_plan(
  source_files = "R/aggregate.R",
  mutators = list(
    muttest::call_name("any", "all"),
    muttest::call_name("all", "any"),
    muttest::call_name("max", "min"),
    muttest::call_name("min", "max"),
    muttest::call_name("sum", "prod")
  )
)
