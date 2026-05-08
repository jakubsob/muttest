describe("delete_statement", {
  it("should delete a single top-level assignment", {
    mutator <- delete_statement()
    expect_mutates_to(mutator, c("x <- 5"), list(character(0)))
  })

  it("should delete a single top-level call", {
    mutator <- delete_statement()
    expect_mutates_to(mutator, c('cat("hi")'), list(character(0)))
  })

  it("should produce one mutant per deletable statement", {
    mutator <- delete_statement()
    code <- c("x <- 5", 'cat("hi")')
    expect_mutation_count(mutator, code, 2)
    mutations <- mutator$mutate(code)
    expect_equal(mutations[[1]], c('cat("hi")'))
    expect_equal(mutations[[2]], c("x <- 5"))
  })

  it("should delete an = assignment", {
    mutator <- delete_statement()
    expect_mutates_to(mutator, c("x = 5"), list(character(0)))
  })

  it("should not delete function definitions", {
    mutator <- delete_statement()
    expect_no_mutations(mutator, c("f <- function(x) x + 1"))
  })

  it("should not delete = function definitions", {
    mutator <- delete_statement()
    expect_no_mutations(mutator, c("f = function(x) x + 1"))
  })

  it("should delete = assignment when rhs contains a nested function", {
    mutator <- delete_statement()
    expect_mutates_to(
      mutator,
      c("result = Map(function(x) x + 1, data)"),
      list(character(0))
    )
  })

  it("should not delete multi-line function definitions", {
    mutator <- delete_statement()
    code <- c("f <- function(x) {", "  x + 1", "}")
    expect_no_mutations(mutator, code)
  })

  it("should delete an assignment inside a function body", {
    mutator <- delete_statement()
    code <- c("f <- function(a) {", "  x <- 5", "  x", "}")
    expect_mutates_to(
      mutator,
      code,
      list(c("f <- function(a) {", "  x", "}"))
    )
  })

  it("should delete a call inside a function body", {
    mutator <- delete_statement()
    code <- c("f <- function() {", '  cat("hi")', "  invisible(NULL)", "}")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("f <- function() {", "  invisible(NULL)", "}"))
    expect_equal(mutations[[2]], c("f <- function() {", '  cat("hi")', "}"))
  })

  it("should delete a multi-line statement", {
    mutator <- delete_statement()
    code <- c(
      "x <- some_function(",
      "  argument1,",
      "  argument2",
      ")",
      "cat(x)"
    )
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("cat(x)"))
    expect_equal(mutations[[2]], c(
      "x <- some_function(",
      "  argument1,",
      "  argument2",
      ")"
    ))
  })

  it("should return NULL when no deletable statements exist", {
    mutator <- delete_statement()
    expect_no_mutations(mutator, c("f <- function(x) x"))
  })

  it("should return NULL when there are no assignments or calls", {
    mutator <- delete_statement()
    expect_no_mutations(mutator, c("42"))
  })
})
