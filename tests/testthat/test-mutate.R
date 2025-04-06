describe("mutate_code", {
  it("should mutate operators", {
    # Arrange
    code <- c(
      "x <- 1 + 2",
      "x <- 1 - 2",
      "x <- 1 * 2",
      "x <- 1 / 2",
      "x <- 1 %% 2",
      "x <- 1 %/% 2",
      "x > y",
      "x < y",
      "x == y",
      "x != y",
      "x >= y",
      "x <= y",
      "x && y",
      "x || y",
      "x & y",
      "x | y"
    )
    mutators <- list(
      operator("+", "-"),
      operator("-", "+"),
      operator("*", "/"),
      operator("/", "*"),
      operator("%%", "/"),
      operator("%/%", "/"),
      operator("==", "!="),
      operator("!=", "=="),
      operator("<", ">"),
      operator(">", "<"),
      operator("<=", ">="),
      operator(">=", "<="),
      operator("&", "|"),
      operator("|", "&"),
      operator("&&", "||"),
      operator("||", "&&")
    )

    # Act
    result <- mutate_code(code, mutators)

    expect_equal(
      result,
      c(
        "x <- 1 - 2",
        "x <- 1 + 2",
        "x <- 1 / 2",
        "x <- 1 * 2",
        "x <- 1 / 2",
        "x <- 1 / 2",
        "x < y",
        "x > y",
        "x != y",
        "x == y",
        "x <= y",
        "x >= y",
        "x || y",
        "x && y",
        "x | y",
        "x & y"
      )
    )
  })
})
