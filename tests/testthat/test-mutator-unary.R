describe("remove_negation", {
  it("should remove ! from a simple expression", {
    mutator <- remove_negation()
    expect_mutates_to(mutator, c("!x"), list(c("x")))
  })

  it("should remove ! leaving the argument intact", {
    mutator <- remove_negation()
    expect_mutates_to(mutator, c("if (!done) break"), list(c("if (done) break")))
  })

  it("should remove ! from a call expression", {
    mutator <- remove_negation()
    expect_mutates_to(mutator, c("!is.na(x)"), list(c("is.na(x)")))
  })

  it("should remove ! from a grouped expression", {
    mutator <- remove_negation()
    expect_mutates_to(mutator, c("!(a > b)"), list(c("(a > b)")))
  })

  it("should return NULL when no ! is present", {
    mutator <- remove_negation()
    expect_no_mutations(mutator, c("x > 0"))
  })

  it("should generate one mutation per negation occurrence", {
    mutator <- remove_negation()
    code <- c("!a && !b")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("a && !b"))
    expect_equal(mutations[[2]], c("!a && b"))
  })

  it("should handle negation on multiple lines independently", {
    mutator <- remove_negation()
    code <- c("x <- !a", "y <- !b")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("x <- a", "y <- !b"))
    expect_equal(mutations[[2]], c("x <- !a", "y <- b"))
  })

  it("should work with = assignment operator", {
    mutator <- remove_negation()
    expect_mutates_to(mutator, c("x = !a"), list(c("x = a")))
  })

  it("should NOT match rlang's !! operator", {
    skip("TODO: Currently matches on !!.")
  })

  it("should NOT match rlang's !!! operator", {
    skip("TODO: Currently matches on !!!.")
  })
})
