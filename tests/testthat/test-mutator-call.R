describe("call_name", {
  it("should mutate a function name", {
    mutator <- call_name("any", "all")
    expect_mutates_to(mutator, c("any(x > 0)"), list(c("all(x > 0)")))
  })

  it("should return NULL when the function is not present", {
    mutator <- call_name("any", "all")
    expect_no_mutations(mutator, c("all(x > 0)"))
  })

  it("should mutate each occurrence independently", {
    mutator <- call_name("min", "max")
    code <- c("min(a, min(b))")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("max(a, min(b))"))
    expect_equal(mutations[[2]], c("min(a, max(b))"))
  })

  it("should work across multiple lines", {
    mutator <- call_name("sum", "prod")
    code <- c("a <- sum(x)", "b <- sum(y)")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("a <- prod(x)", "b <- sum(y)"))
    expect_equal(mutations[[2]], c("a <- sum(x)", "b <- prod(y)"))
  })

  it("does not mutate non-call identifiers with the same name", {
    mutator <- call_name("max", "min")
    # 'max' appearing as a variable, not as a call
    expect_no_mutations(mutator, c("max <- 10"))
  })

  it("should work with = assignment operator", {
    mutator <- call_name("sum", "prod")
    expect_mutates_to(mutator, c("x = sum(y)"), list(c("x = prod(y)")))
  })
})
