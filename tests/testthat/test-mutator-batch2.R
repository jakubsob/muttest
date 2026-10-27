describe("remove_else_branch", {
  it("drops the else branch", {
    expect_mutates_to(remove_else_branch(), "if (a) b else c", list("if (a) b"))
  })

  it("drops a multi-line else block", {
    code <- c("if (a) {", "  b", "} else {", "  c", "  d", "}", "z")
    expect_mutates_to(remove_else_branch(), code, list(c("if (a) {", "  b", "}", "z")))
  })

  it("ignores if without else", {
    expect_no_mutations(remove_else_branch(), "if (a) b")
  })

  it("handles else-if chains one level at a time", {
    mutations <- mutate_codes(remove_else_branch(), "if (a) b else if (c) d else e")
    expect_equal(mutations[[1]], "if (a) b")
    expect_equal(mutations[[2]], "if (a) b else if (c) d")
  })
})

describe("empty_if_body", {
  it("empties a bare consequence", {
    expect_mutates_to(empty_if_body(), "if (a) b else c", list("if (a) {} else c"))
  })

  it("empties a multi-line block", {
    code <- c("if (a) {", "  b", "}", "z")
    expect_mutates_to(empty_if_body(), code, list(c("if (a) {}", "z")))
  })

  it("skips bodies that are already empty", {
    expect_no_mutations(empty_if_body(), "if (a) {}")
  })
})

describe("remove_null_guard", {
  it("removes a guard with a bare return", {
    expect_mutates_to(remove_null_guard(), "if (is.null(x)) return(1)", list("NULL"))
  })

  it("removes a guard with a braced return", {
    code <- c("f <- function(x) {", "  if (is.null(x)) {", "    return(NULL)", "  }", "  x", "}")
    expect_mutates_to(
      remove_null_guard(),
      code,
      list(c("f <- function(x) {", "  NULL", "  x", "}"))
    )
  })

  it("ignores guards that do not return", {
    expect_no_mutations(remove_null_guard(), "if (is.null(x)) x <- 1")
  })

  it("ignores conditions without is.null", {
    expect_no_mutations(remove_null_guard(), "if (x > 1) return(1)")
  })

  it("ignores guards with an else branch", {
    expect_no_mutations(remove_null_guard(), "if (is.null(x)) return(1) else 2")
  })

  it("ignores guards with several statements", {
    code <- c("if (is.null(x)) {", "  message('a')", "  return(1)", "}")
    expect_no_mutations(remove_null_guard(), code)
  })
})

describe("replace_implicit_return", {
  it("replaces the last expression of a braced body", {
    code <- c("f <- function(x) {", "  y <- x + 1", "  y", "}")
    expect_mutates_to(
      replace_implicit_return(),
      code,
      list(c("f <- function(x) {", "  y <- x + 1", "  NULL", "}"))
    )
  })

  it("replaces a non-braced body", {
    expect_mutates_to(replace_implicit_return("NA"), "f <- function(x) x + 1", list("f <- function(x) NA"))
  })

  it("skips functions ending in return()", {
    expect_no_mutations(replace_implicit_return(), "f <- function(x) { return(x) }")
  })

  it("ignores trailing comments", {
    code <- c("f <- function(x) {", "  x", "  # done", "}")
    expect_mutates_to(
      replace_implicit_return(),
      code,
      list(c("f <- function(x) {", "  NULL", "  # done", "}"))
    )
  })

  it("mutates nested functions independently", {
    code <- "f <- function() { g <- function() 1; g() }"
    expect_mutation_count(replace_implicit_return(), code, 2)
  })

  it("skips empty bodies and bodies already equal to the replacement", {
    expect_no_mutations(replace_implicit_return(), c("f <- function() {}", "g <- function() NULL"))
  })
})

describe("replace_call_result", {
  it("replaces a call with NULL", {
    expect_mutates_to(replace_call_result(), "y <- compute(x)", list("y <- NULL"))
  })

  it("replaces nested calls independently", {
    expect_mutates_to(
      replace_call_result("0"),
      "f(g(x))",
      list("0", "f(0)")
    )
  })

  it("skips return calls and assignment targets", {
    expect_no_mutations(replace_call_result(), c("return(x)", "names(x) <- 'a'") )
  })

  it("still mutates the right-hand side of replacement-function assignment", {
    expect_mutates_to(replace_call_result(), "names(x) <- f(y)", list("names(x) <- NULL"))
  })
})
