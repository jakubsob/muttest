describe("boolean_literal", {
  it("should mutate TRUE to FALSE", {
    mutator <- boolean_literal("TRUE", "FALSE")
    expect_mutates_to(mutator, c("x <- TRUE"), list(c("x <- FALSE")))
  })

  it("should mutate FALSE to TRUE", {
    mutator <- boolean_literal("FALSE", "TRUE")
    expect_mutates_to(mutator, c("x <- FALSE"), list(c("x <- TRUE")))
  })

  it("should mutate T to F", {
    mutator <- boolean_literal("T", "F")
    expect_mutates_to(mutator, c("x <- T"), list(c("x <- F")))
  })

  it("should mutate F to T", {
    mutator <- boolean_literal("F", "T")
    expect_mutates_to(mutator, c("x <- F"), list(c("x <- T")))
  })

  it("should return NULL when no matching literal exists", {
    mutator <- boolean_literal("TRUE", "FALSE")
    expect_no_mutations(mutator, c("x <- FALSE"))
  })

  it("should generate one mutation per occurrence", {
    mutator <- boolean_literal("TRUE", "FALSE")
    expect_mutation_count(mutator, c("f(TRUE, TRUE)"), 2)
    mutations <- mutate_codes(mutator, c("f(TRUE, TRUE)"))
    expect_equal(mutations[[1]], c("f(FALSE, TRUE)"))
    expect_equal(mutations[[2]], c("f(TRUE, FALSE)"))
  })

  it("should handle multiline code", {
    mutator <- boolean_literal("TRUE", "FALSE")
    code <- c("x <- TRUE", "y <- TRUE")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("x <- FALSE", "y <- TRUE"))
    expect_equal(mutations[[2]], c("x <- TRUE",  "y <- FALSE"))
  })

  it("should not mutate FALSE when looking for TRUE", {
    mutator <- boolean_literal("TRUE", "FALSE")
    expect_no_mutations(mutator, c("if (FALSE) 1"))
  })

  it("should work with = assignment operator", {
    mutator <- boolean_literal("TRUE", "FALSE")
    expect_mutates_to(mutator, c("x = TRUE"), list(c("x = FALSE")))
  })
})
