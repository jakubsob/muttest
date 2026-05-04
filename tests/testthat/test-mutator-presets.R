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
