describe("operator", {
  it("should generate mutations for a single operator", {
    # Arrange
    code <- c("x <- 1 + 2")
    mutator <- operator("+", "-")

    # Act
    mutations <- mutator$mutate(code)

    # Assert
    expect_equal(
      mutations,
      list(
        c("x <- 1 - 2")
      )
    )
  })

  it("should return NULL when no mutations are possible", {
    # Arrange
    code <- c("x <- 1 - 2")
    mutator <- operator("+", "-")

    # Act
    mutations <- mutator$mutate(code)

    # Assert
    expect_null(mutations)
  })

  it("should generate multiple mutations for multiple occurrences", {
    # Arrange
    code <- c("x <- 1 + 2 + 3")
    mutator <- operator("+", "-")

    # Act
    mutations <- mutator$mutate(code)

    # Assert
    expect_equal(
      mutations,
      list(
        c("x <- 1 - 2 + 3"),
        c("x <- 1 + 2 - 3")
      )
    )
  })

  it("should work for multiline code", {
    # Arrange
    code <- c("x <- 1 + 2", "y <- x + 3")
    mutator <- operator("+", "-")

    # Act
    mutations <- mutator$mutate(code)

    # Assert
    expect_equal(
      mutations,
      list(
        c("x <- 1 - 2", "y <- x + 3"),
        c("x <- 1 + 2", "y <- x - 3")
      )
    )
  })
})
