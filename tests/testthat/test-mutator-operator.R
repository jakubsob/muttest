describe("operator", {
  it("should generate mutations for a single operator", {
    code <- c("x <- 1 + 2")
    mutator <- operator("+", "-")
    mutations <- mutate_code(code, mutator)
    expect_equal(mutations, list(c("x <- 1 - 2")))
  })

  it("should return NULL when no mutations are possible", {
    code <- c("x <- 1 - 2")
    mutator <- operator("+", "-")
    mutations <- mutate_code(code, mutator)
    expect_null(mutations)
  })

  it("should generate multiple mutations for multiple occurrences", {
    code <- c("x <- 1 + 2 + 3")
    mutator <- operator("+", "-")
    mutations <- mutate_code(code, mutator)
    expect_equal(
      mutations,
      list(
        c("x <- 1 - 2 + 3"),
        c("x <- 1 + 2 - 3")
      )
    )
  })

  it("should work for multiline code", {
    code <- c("x <- 1 + 2", "y <- x + 3")
    mutator <- operator("+", "-")
    mutations <- mutate_code(code, mutator)
    expect_equal(
      mutations,
      list(
        c("x <- 1 - 2", "y <- x + 3"),
        c("x <- 1 + 2", "y <- x - 3")
      )
    )
  })
})
