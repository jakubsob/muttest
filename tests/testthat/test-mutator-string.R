describe("string_empty", {
  it("should replace a non-empty string with empty string", {
    mutator <- string_empty()
    expect_mutates_to(mutator, c('x <- "hello"'), list(c('x <- ""')))
  })

  it("should work with = assignment operator", {
    mutator <- string_empty()
    expect_mutates_to(mutator, c('x = "hello"'), list(c('x = ""')))
  })

  it("should return NULL when only empty strings are present", {
    mutator <- string_empty()
    expect_no_mutations(mutator, c('x <- ""'))
  })

  it("should generate one mutation per non-empty string occurrence", {
    mutator <- string_empty()
    code <- c('paste("foo", "bar")')
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c('paste("", "bar")'))
    expect_equal(mutations[[2]], c('paste("foo", "")'))
  })

  it("should work across multiple lines", {
    mutator <- string_empty()
    code <- c('a <- "first"', 'b <- "second"')
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c('a <- ""', 'b <- "second"'))
    expect_equal(mutations[[2]], c('a <- "first"', 'b <- ""'))
  })
})

describe("string_fill", {
  it("should replace an empty string with a placeholder", {
    mutator <- string_fill()
    expect_mutates_to(mutator, c('x <- ""'), list(c('x <- "mutant"')))
  })

  it("should work with = assignment operator", {
    mutator <- string_fill()
    expect_mutates_to(mutator, c('x = ""'), list(c('x = "mutant"')))
  })

  it("should return NULL when no empty strings are present", {
    mutator <- string_fill()
    expect_no_mutations(mutator, c('x <- "hello"'))
  })

  it("should generate one mutation per empty string occurrence", {
    mutator <- string_fill()
    code <- c('f("", "")')
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c('f("mutant", "")'))
    expect_equal(mutations[[2]], c('f("", "mutant")'))
  })

  it("should use a custom fill value", {
    mutator <- string_fill(fill = "PLACEHOLDER")
    expect_mutates_to(mutator, c('x <- ""'), list(c('x <- "PLACEHOLDER"')))
  })

  it("should reflect custom fill in the to label", {
    mutator <- string_fill(fill = "X")
    expect_equal(mutator$to, '"X"')
  })
})
