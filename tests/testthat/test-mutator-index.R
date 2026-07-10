describe("index_increment", {
  it("should increment an identifier index in single brackets", {
    mutator <- index_increment()
    expect_mutates_to(mutator, c("x[i]"), list(c("x[i + 1L]")))
  })

  it("should increment a numeric literal index", {
    mutator <- index_increment()
    expect_mutates_to(mutator, c("x[1]"), list(c("x[1 + 1L]")))
  })

  it("should increment an integer literal index", {
    mutator <- index_increment()
    expect_mutates_to(mutator, c("x[1L]"), list(c("x[1L + 1L]")))
  })

  it("should increment an identifier index in double brackets", {
    mutator <- index_increment()
    expect_mutates_to(mutator, c("x[[i]]"), list(c("x[[i + 1L]]")))
  })

  it("should not mutate complex index expressions", {
    mutator <- index_increment()
    expect_no_mutations(mutator, c("x[a + b]"))
  })

  it("should return NULL when no subscript is present", {
    mutator <- index_increment()
    expect_no_mutations(mutator, c("f(i)"))
  })

  it("should generate one mutation per subscript", {
    mutator <- index_increment()
    code <- c("x[i] + y[j]")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("x[i + 1L] + y[j]"))
    expect_equal(mutations[[2]], c("x[i] + y[j + 1L]"))
  })

  it("should work across multiple lines", {
    mutator <- index_increment()
    code <- c("a <- x[i]", "b <- y[j]")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("a <- x[i + 1L]", "b <- y[j]"))
    expect_equal(mutations[[2]], c("a <- x[i]",      "b <- y[j + 1L]"))
  })
})

describe("index_decrement", {
  it("should decrement an identifier index in single brackets", {
    mutator <- index_decrement()
    expect_mutates_to(mutator, c("x[i]"), list(c("x[i - 1L]")))
  })

  it("should decrement a numeric literal index", {
    mutator <- index_decrement()
    expect_mutates_to(mutator, c("x[1]"), list(c("x[1 - 1L]")))
  })

  it("should decrement an integer literal index", {
    mutator <- index_decrement()
    expect_mutates_to(mutator, c("x[1L]"), list(c("x[1L - 1L]")))
  })

  it("should decrement an identifier index in double brackets", {
    mutator <- index_decrement()
    expect_mutates_to(mutator, c("x[[i]]"), list(c("x[[i - 1L]]")))
  })

  it("should not mutate complex index expressions", {
    mutator <- index_decrement()
    expect_no_mutations(mutator, c("x[a + b]"))
  })

  it("should generate one mutation per subscript", {
    mutator <- index_decrement()
    code <- c("x[i] + y[j]")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("x[i - 1L] + y[j]"))
    expect_equal(mutations[[2]], c("x[i] + y[j - 1L]"))
  })
})
