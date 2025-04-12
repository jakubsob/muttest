describe("mutate_code", {
  it("should generate mutations for a single operator", {
    # Arrange
    code <- c("x <- 1 + 2")
    mutator <- operator("+", "-")

    # Act
    mutations <- mutate_code(code, mutator)

    # Assert
    expect_equal(
      mutations,
      list(
        list(
          code = c("x <- 1 - 2"),
          mutator = mutator
        )
      )
    )
  })

  it("should return NULL when no mutations are possible", {
    # Arrange
    code <- c("x <- 1 - 2")
    mutator <- operator("+", "-")

    # Act
    mutations <- mutate_code(code, mutator)

    # Assert
    expect_null(mutations)
  })

  it("should generate multiple mutations for multiple occurrences", {
    # Arrange
    code <- c("x <- 1 + 2 + 3")
    mutator <- operator("+", "-")

    # Act
    mutations <- mutate_code(code, mutator)

    # Assert
    expect_equal(
      mutations,
      list(
        list(
          code = c("x <- 1 - 2 + 3"),
          mutator = mutator
        ),
        list(
          code = c("x <- 1 + 2 - 3"),
          mutator = mutator
        )
      )
    )
  })

  it("should work for multiline code", {
    # Arrange
    code <- c("x <- 1 + 2", "y <- x + 3")
    mutator <- operator("+", "-")

    # Act
    mutations <- mutate_code(code, mutator)

    # Assert
    expect_equal(
      mutations,
      list(
        list(
          code = c("x <- 1 - 2", "y <- x + 3"),
          mutator = mutator
        ),
        list(
          code = c("x <- 1 + 2", "y <- x - 3"),
          mutator = mutator
        )
      )
    )
  })
})
