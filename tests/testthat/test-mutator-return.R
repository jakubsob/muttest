describe("replace_return_value default (NULL)", {
  it("should replace the return value with NULL", {
    mutator <- replace_return_value()
    expect_mutates_to(
      mutator,
      c("return(x)"),
      list(c("return(NULL)"))
    )
  })

  it("should not mutate return(NULL) when replacement is NULL", {
    mutator <- replace_return_value()
    expect_no_mutations(mutator, c("return(NULL)"))
  })

  it("should replace a complex return expression", {
    mutator <- replace_return_value()
    expect_mutates_to(
      mutator,
      c("return(x + 1)"),
      list(c("return(NULL)"))
    )
  })

  it("should replace a nested call return expression", {
    mutator <- replace_return_value()
    expect_mutates_to(
      mutator,
      c("return(invisible(x))"),
      list(c("return(NULL)"))
    )
  })

  it("should generate one mutation per return call", {
    mutator <- replace_return_value()
    code <- c("if (x) return(a)", "return(b)")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("if (x) return(NULL)", "return(b)"))
    expect_equal(mutations[[2]], c("if (x) return(a)",    "return(NULL)"))
  })

  it("should work across multiple lines", {
    mutator <- replace_return_value()
    code <- c("f <- function(x) {", "  return(x)", "}")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 1)
    expect_equal(mutations[[1]], c("f <- function(x) {", "  return(NULL)", "}"))
  })

  it("should return NULL when no return() calls are present", {
    mutator <- replace_return_value()
    expect_no_mutations(mutator, c("x + 1"))
  })

  it("should not match return() with no argument", {
    mutator <- replace_return_value()
    expect_no_mutations(mutator, c("return()"))
  })
})

describe("replace_return_value implicit", {
  it("should mutate implicit returns", {
    skip("TODO: Implement implicit return mutation.")
    mutator <- replace_return_value()
    expect_mutates_to(mutator, c("f <- function(x) { x }"), list(c("f <- function(x) { NULL }")))
  })

  it("should mutate the last expression in a function body", {
    skip("TODO: Implement implicit return mutation.")
    mutator <- replace_return_value()
    expect_mutates_to(
      mutator,
      c("f <- function(x) {", "  y <- x + 1", "  y <- 2", "}"),
      list(c("f <- function(x) {", "  y <- x + 1", "  NULL", "}"))
    )
  })
})

describe("replace_return_value with NA replacement", {
  it("should replace the return value with NA", {
    mutator <- replace_return_value("NA")
    expect_mutates_to(
      mutator,
      c("return(x)"),
      list(c("return(NA)"))
    )
  })

  it("should not mutate return(NA) when replacement is NA", {
    mutator <- replace_return_value("NA")
    expect_no_mutations(mutator, c("return(NA)"))
  })
})

describe("replace_return_value with string replacements", {
  it('should insert the string "NULL" when replacement is \'\"NULL\"\'', {
    mutator <- replace_return_value('"NULL"')
    expect_mutates_to(
      mutator,
      c("return(x)"),
      list(c('return("NULL")'))
    )
  })

  it('should not mutate return("NULL") when replacement is \'\"NULL\"\'', {
    mutator <- replace_return_value('"NULL"')
    expect_no_mutations(mutator, c('return("NULL")'))
  })

  it('should insert the string "NA" when replacement is \'\"NA\"\'', {
    mutator <- replace_return_value('"NA"')
    expect_mutates_to(
      mutator,
      c("return(x)"),
      list(c('return("NA")'))
    )
  })

  it('should not mutate return("NA") when replacement is \'\"NA\"\'', {
    mutator <- replace_return_value('"NA"')
    expect_no_mutations(mutator, c('return("NA")'))
  })
})
