describe("numeric_increment", {
  it("should increment an integer literal", {
    mutator <- numeric_increment()
    expect_mutates_to(mutator, c("x <- 5"), list(c("x <- 6")))
  })

  it("should increment a float literal", {
    mutator <- numeric_increment()
    expect_mutates_to(mutator, c("x <- 1.5"), list(c("x <- 2.5")))
  })

  it("should increment zero", {
    mutator <- numeric_increment()
    expect_mutates_to(mutator, c("x <- 0"), list(c("x <- 1")))
  })

  it("should return NULL when no numeric literals present", {
    mutator <- numeric_increment()
    expect_no_mutations(mutator, c('x <- "hello"'))
  })

  it("should generate one mutation per literal occurrence", {
    mutator <- numeric_increment()
    code <- c("f(1, 2)")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("f(2, 2)"))
    expect_equal(mutations[[2]], c("f(1, 3)"))
  })

  it("should work across multiple lines", {
    mutator <- numeric_increment()
    code <- c("a <- 10", "b <- 20")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("a <- 11", "b <- 20"))
    expect_equal(mutations[[2]], c("a <- 10", "b <- 21"))
  })

  it("should work with = assignment operator", {
    mutator <- numeric_increment()
    expect_mutates_to(mutator, c("x = 5"), list(c("x = 6")))
  })
})

describe("numeric_decrement", {
  it("should decrement an integer literal", {
    mutator <- numeric_decrement()
    expect_mutates_to(mutator, c("x <- 5"), list(c("x <- 4")))
  })

  it("should decrement a float literal", {
    mutator <- numeric_decrement()
    expect_mutates_to(mutator, c("x <- 2.5"), list(c("x <- 1.5")))
  })

  it("should decrement zero to negative one", {
    mutator <- numeric_decrement()
    expect_mutates_to(mutator, c("x <- 0"), list(c("x <- -1")))
  })

  it("should generate one mutation per literal occurrence", {
    mutator <- numeric_decrement()
    code <- c("f(3, 7)")
    mutations <- mutate_codes(mutator, code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("f(2, 7)"))
    expect_equal(mutations[[2]], c("f(3, 6)"))
  })

  it("should work with = assignment operator", {
    mutator <- numeric_decrement()
    expect_mutates_to(mutator, c("x = 5"), list(c("x = 4")))
  })
})

describe("numeric_increment with custom by", {
  it("should increment by the given amount", {
    mutator <- numeric_increment(by = 2)
    expect_mutates_to(mutator, c("x <- 5"), list(c("x <- 7")))
  })

  it("should increment by a fractional amount", {
    mutator <- numeric_increment(by = 0.5)
    expect_mutates_to(mutator, c("x <- 1.0"), list(c("x <- 1.5")))
  })
})

describe("numeric_decrement with custom by", {
  it("should decrement by the given amount", {
    mutator <- numeric_decrement(by = 3)
    expect_mutates_to(mutator, c("x <- 10"), list(c("x <- 7")))
  })

  it("should decrement by a fractional amount", {
    mutator <- numeric_decrement(by = 0.5)
    expect_mutates_to(mutator, c("x <- 2.0"), list(c("x <- 1.5")))
  })
})
