describe("remove_unary_minus", {
  it("removes the minus from a negated identifier", {
    expect_mutates_to(remove_unary_minus(), "y <- -x", list("y <- x"))
  })

  it("removes the minus from a grouped expression", {
    expect_mutates_to(remove_unary_minus(), "-(a + b)", list("(a + b)"))
  })

  it("leaves logical negation and unary plus alone", {
    expect_no_mutations(remove_unary_minus(), c("!x", "+x"))
  })

  it("does not touch binary minus", {
    expect_no_mutations(remove_unary_minus(), "a - b")
  })
})

describe("integer_to_double", {
  it("drops the L suffix", {
    expect_mutates_to(integer_to_double(), "x <- 5L", list("x <- 5"))
  })

  it("ignores double literals", {
    expect_no_mutations(integer_to_double(), "x <- 5")
  })
})

describe("double_to_integer", {
  it("adds an L suffix to whole numbers", {
    expect_mutates_to(double_to_integer(), "x <- 5", list("x <- 5L"))
  })

  it("ignores fractional, exponent and integer literals", {
    expect_no_mutations(double_to_integer(), c("a <- 1.5", "b <- 1e3", "c <- 2L"))
  })
})

describe("numeric_replace", {
  it("replaces with zero, preserving the integer suffix", {
    expect_mutates_to(numeric_replace("zero"), "x <- 5 + 2L", list("x <- 0 + 2L", "x <- 5 + 0L"))
  })

  it("negates literals", {
    expect_mutates_to(numeric_replace("negate"), "x <- 2.5", list("x <- -2.5"))
  })

  it("doubles literals", {
    expect_mutates_to(numeric_replace("double"), c("x <- 4", "y <- 3L"), list(c("x <- 8", "y <- 3L"), c("x <- 4", "y <- 6L")))
  })

  it("never mutates zero", {
    for (kind in c("zero", "negate", "double")) {
      expect_no_mutations(numeric_replace(kind), "x <- 0")
    }
  })

  it("rejects unknown kinds", {
    expect_error(numeric_replace("bogus"))
  })
})

describe("condition_constant", {
  it("replaces an if condition with TRUE", {
    expect_mutates_to(
      condition_constant(TRUE),
      "if (x > 0) y",
      list("if (TRUE) y")
    )
  })

  it("replaces an if condition with FALSE", {
    expect_mutates_to(
      condition_constant(FALSE),
      "if (x > 0) y",
      list("if (FALSE) y")
    )
  })

  it("skips conditions that already equal the constant", {
    expect_no_mutations(condition_constant(TRUE), "if (TRUE) y")
  })

  it("does not touch while loops by default", {
    expect_no_mutations(condition_constant(FALSE), "while (x) y")
  })

  it("can target while loops", {
    expect_mutates_to(
      condition_constant(FALSE, statements = "while"),
      "while (x) y",
      list("while (FALSE) y")
    )
  })
})

describe("keep_operand", {
  it("keeps the left operand", {
    expect_mutates_to(keep_operand("lhs"), "if (a && b) x", list("if (a) x"))
  })

  it("keeps the right operand", {
    expect_mutates_to(keep_operand("rhs"), "if (a || b) x", list("if (b) x"))
  })

  it("handles vectorised operators", {
    expect_mutates_to(keep_operand("lhs"), "a & b | c", list("a & b", "a | c"))
  })

  it("ignores other binary operators", {
    expect_no_mutations(keep_operand("lhs"), "a + b == c")
  })
})

describe("membership operator", {
  it("can be swapped via operator()", {
    expect_mutates_to(operator("%in%", "=="), "x %in% y", list("x == y"))
  })
})
