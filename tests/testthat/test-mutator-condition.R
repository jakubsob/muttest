describe("negate_condition", {
  it("should negate the condition of an if statement", {
    mutator <- negate_condition()
    expect_mutates_to(
      mutator,
      c("if (x > 0) TRUE"),
      list(c("if (!(x > 0)) TRUE"))
    )
  })

  it("should negate the condition of a while statement", {
    mutator <- negate_condition()
    expect_mutates_to(
      mutator,
      c("while (done) x <- x + 1"),
      list(c("while (!(done)) x <- x + 1"))
    )
  })

  it("should return NULL when there are no if/while statements", {
    mutator <- negate_condition()
    expect_no_mutations(mutator, c("x <- 1 + 2"))
  })

  it("should generate one mutation per condition", {
    mutator <- negate_condition()
    code <- c("if (a) 1", "if (b) 2")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("if (!(a)) 1", "if (b) 2"))
    expect_equal(mutations[[2]], c("if (a) 1",    "if (!(b)) 2"))
  })

  it("should handle a compound condition", {
    mutator <- negate_condition()
    expect_mutates_to(
      mutator,
      c("if (x > 0 && y < 10) TRUE"),
      list(c("if (!(x > 0 && y < 10)) TRUE"))
    )
  })
})

describe("remove_condition_negation", {
  it("should remove ! from a negated if condition", {
    mutator <- remove_condition_negation()
    expect_mutates_to(
      mutator,
      c("if (!done) break"),
      list(c("if (done) break"))
    )
  })

  it("should remove ! from a negated while condition", {
    mutator <- remove_condition_negation()
    expect_mutates_to(
      mutator,
      c("while (!ready) wait()"),
      list(c("while (ready) wait()"))
    )
  })

  it("should return NULL when no condition is negated", {
    mutator <- remove_condition_negation()
    expect_no_mutations(mutator, c("if (x > 0) TRUE"))
  })

  it("should not touch negations outside of conditions", {
    mutator <- remove_condition_negation()
    expect_no_mutations(mutator, c("x <- !is.null(y)"))
  })

  it("should generate one mutation per negated condition", {
    mutator <- remove_condition_negation()
    code <- c("if (!a) 1", "if (!b) 2")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("if (a) 1",  "if (!b) 2"))
    expect_equal(mutations[[2]], c("if (!a) 1", "if (b) 2"))
  })

  it("should leave non-negated conditions untouched", {
    mutator <- remove_condition_negation()
    code <- c("if (!a) 1", "if (b) 2")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 1)
    expect_equal(mutations[[1]], c("if (a) 1", "if (b) 2"))
  })
})

describe("negate_condition with statements filter", {
  it("should only negate if conditions when statements = 'if'", {
    mutator <- negate_condition(statements = "if")
    code <- c("if (a) 1", "while (b) 2")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 1)
    expect_equal(mutations[[1]], c("if (!(a)) 1", "while (b) 2"))
  })

  it("should only negate while conditions when statements = 'while'", {
    mutator <- negate_condition(statements = "while")
    code <- c("if (a) 1", "while (b) 2")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 1)
    expect_equal(mutations[[1]], c("if (a) 1", "while (!(b)) 2"))
  })

  it("should error on unknown statement type", {
    expect_error(negate_condition(statements = "for"))
  })
})

describe("remove_condition_negation with statements filter", {
  it("should only remove negation from if conditions when statements = 'if'", {
    mutator <- remove_condition_negation(statements = "if")
    code <- c("if (!a) 1", "while (!b) 2")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 1)
    expect_equal(mutations[[1]], c("if (a) 1", "while (!b) 2"))
  })

  it("should only remove negation from while conditions when statements = 'while'", {
    mutator <- remove_condition_negation(statements = "while")
    code <- c("if (!a) 1", "while (!b) 2")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 1)
    expect_equal(mutations[[1]], c("if (!a) 1", "while (b) 2"))
  })
})

describe("remove_condition_negation with rlang operators", {
  it("should NOT match rlang's !! operator in conditions", {
    skip("TODO: Currently matches on !!.")
  })

  it("should NOT match rlang's !!! operator in conditions", {
    skip("TODO: Currently matches on !!!.")
  })
})
