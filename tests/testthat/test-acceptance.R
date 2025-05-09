# Run acceptance tests only when calculating the coverage
testthat::skip_if_not(covr::in_covr())

cucumber::test("../acceptance")
