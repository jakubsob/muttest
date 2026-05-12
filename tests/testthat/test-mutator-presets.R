describe("arithmetic_operators", {
  it("returns a non-empty list of mutators", {
    mutators <- arithmetic_operators()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("includes + to - mutator", {
    mutators <- arithmetic_operators()
    froms <- purrr::map_chr(mutators, \(m) m$from)
    expect_true("+" %in% froms)
  })

  it("each mutator generates mutations on matching code", {
    mutators <- arithmetic_operators()
    results <- purrr::map(mutators, \(m) m$mutate(c(sprintf("x <- 1 %s 2", m$from))))
    expect_true(any(!purrr::map_lgl(results, is.null)))
  })
})

describe("comparison_operators", {
  it("returns a non-empty list of mutators", {
    mutators <- comparison_operators()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("includes boundary shift < to <= mutator", {
    mutators <- comparison_operators()
    pairs <- purrr::map(mutators, \(m) c(m$from, m$to))
    expect_true(any(purrr::map_lgl(pairs, \(p) p[1] == "<" && p[2] == "<=")))
  })

  it("< to > generates one mutation", {
    mutator <- operator("<", ">")
    expect_mutates_to(mutator, c("if (x < 10) TRUE"), list(c("if (x > 10) TRUE")))
  })
})

describe("boolean_literals", {
  it("returns a non-empty list of mutators", {
    mutators <- boolean_literals()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("includes TRUE to FALSE mutator", {
    mutators <- boolean_literals()
    froms <- purrr::map_chr(mutators, \(m) m$from)
    expect_true("TRUE" %in% froms)
  })

  it("TRUE to FALSE generates one mutation", {
    mutator <- boolean_literal("TRUE", "FALSE")
    expect_mutates_to(mutator, c("x <- TRUE"), list(c("x <- FALSE")))
  })

  it("T to F generates one mutation", {
    mutator <- boolean_literal("T", "F")
    expect_mutates_to(mutator, c("x <- T"), list(c("x <- F")))
  })
})

describe("logical_operators", {
  it("returns a non-empty list of mutators", {
    mutators <- logical_operators()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("&& to || generates one mutation", {
    mutator <- operator("&&", "||")
    expect_mutates_to(mutator, c("x && y"), list(c("x || y")))
  })

  it("& to | generates one mutation on vectorised operator", {
    mutator <- operator("&", "|")
    expect_mutates_to(mutator, c("x & y"), list(c("x | y")))
  })
})

describe("na_literals", {
  it("returns a non-empty list of mutators", {
    mutators <- na_literals()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("includes NA to NULL and NULL to NA mutators", {
    mutators <- na_literals()
    pairs <- purrr::map(mutators, \(m) c(m$from, m$to))
    expect_true(any(purrr::map_lgl(pairs, \(p) p[1] == "NA" && p[2] == "NULL")))
    expect_true(any(purrr::map_lgl(pairs, \(p) p[1] == "NULL" && p[2] == "NA")))
  })

  it("NA to NULL generates one mutation", {
    mutator <- na_literal("NA", "NULL")
    expect_mutates_to(mutator, c("x <- NA"), list(c("x <- NULL")))
  })
})

describe("numeric_literals", {
  it("returns a non-empty list of mutators", {
    mutators <- numeric_literals()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("contains increment and decrement mutators", {
    mutators <- numeric_literals()
    expect_equal(length(mutators), 2)
  })

  it("increment generates one mutation", {
    mutator <- numeric_increment()
    expect_mutates_to(mutator, c("x <- 5"), list(c("x <- 6")))
  })
})

describe("index_mutations", {
  it("returns a non-empty list of mutators", {
    mutators <- index_mutations()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("contains increment and decrement mutators", {
    mutators <- index_mutations()
    expect_equal(length(mutators), 2)
  })

  it("increment generates one mutation", {
    mutator <- index_increment()
    expect_mutates_to(mutator, c("x[1]"), list(c("x[1 + 1L]")))
  })
})

describe("string_literals", {
  it("returns a non-empty list of mutators", {
    mutators <- string_literals()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("contains empty and fill mutators", {
    mutators <- string_literals()
    expect_equal(length(mutators), 2)
  })

  it("string_empty generates one mutation", {
    mutator <- string_empty()
    expect_mutates_to(mutator, c('x <- "hello"'), list(c('x <- ""')))
  })
})

describe("condition_mutations", {
  it("returns a non-empty list of mutators", {
    mutators <- condition_mutations()
    expect_true(length(mutators) > 0)
    expect_true(all(purrr::map_lgl(mutators, \(m) inherits(m, "Mutator"))))
  })

  it("contains negate and remove-negation mutators", {
    mutators <- condition_mutations()
    expect_equal(length(mutators), 2)
  })

  it("negate_condition generates one mutation", {
    mutator <- negate_condition()
    expect_mutates_to(mutator, c("if (x > 0) 1"), list(c("if (!(x > 0)) 1")))
  })
})
