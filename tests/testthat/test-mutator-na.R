describe("na_literal: NA → NULL", {
  it("should replace NA with NULL", {
    mutator <- na_literal("NA", "NULL")
    expect_mutates_to(mutator, c("x <- NA"), list(c("x <- NULL")))
  })

  it("should return NULL when NA is not present", {
    mutator <- na_literal("NA", "NULL")
    expect_no_mutations(mutator, c("x <- 1"))
  })

  it("should not match NULL when targeting NA", {
    mutator <- na_literal("NA", "NULL")
    expect_no_mutations(mutator, c("x <- NULL"))
  })

  it("should generate one mutation per occurrence", {
    mutator <- na_literal("NA", "NULL")
    code <- c("f(NA, NA)")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("f(NULL, NA)"))
    expect_equal(mutations[[2]], c("f(NA, NULL)"))
  })

  it("should work across multiple lines", {
    mutator <- na_literal("NA", "NULL")
    code <- c("a <- NA", "b <- NA")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("a <- NULL", "b <- NA"))
    expect_equal(mutations[[2]], c("a <- NA",   "b <- NULL"))
  })
})

describe("na_literal: NULL → NA", {
  it("should replace NULL with NA", {
    mutator <- na_literal("NULL", "NA")
    expect_mutates_to(mutator, c("x <- NULL"), list(c("x <- NA")))
  })

  it("should return NULL when NULL is not present", {
    mutator <- na_literal("NULL", "NA")
    expect_no_mutations(mutator, c("x <- 1"))
  })

  it("should not match NA when targeting NULL", {
    mutator <- na_literal("NULL", "NA")
    expect_no_mutations(mutator, c("x <- NA"))
  })

  it("should generate one mutation per occurrence", {
    mutator <- na_literal("NULL", "NA")
    code <- c("f(NULL, NULL)")
    mutations <- mutator$mutate(code)
    expect_length(mutations, 2)
    expect_equal(mutations[[1]], c("f(NA, NULL)"))
    expect_equal(mutations[[2]], c("f(NULL, NA)"))
  })
})

describe("na_literal: typed NAs", {
  it("should replace NA_real_ with NA", {
    mutator <- na_literal("NA_real_", "NA")
    expect_mutates_to(mutator, c("x <- NA_real_"), list(c("x <- NA")))
  })

  it("should replace NA_integer_ with NA", {
    mutator <- na_literal("NA_integer_", "NA")
    expect_mutates_to(mutator, c("x <- NA_integer_"), list(c("x <- NA")))
  })

  it("should replace NA_character_ with NA", {
    mutator <- na_literal("NA_character_", "NA")
    expect_mutates_to(mutator, c("x <- NA_character_"), list(c("x <- NA")))
  })

  it("should not match plain NA when targeting NA_real_", {
    mutator <- na_literal("NA_real_", "NA")
    expect_no_mutations(mutator, c("x <- NA"))
  })

  it("should replace NA with NA_real_", {
    mutator <- na_literal("NA", "NA_real_")
    expect_mutates_to(mutator, c("x <- NA"), list(c("x <- NA_real_")))
  })

  it("should not match NA_real_ when targeting plain NA", {
    mutator <- na_literal("NA", "NA_real_")
    expect_no_mutations(mutator, c("x <- NA_real_"))
  })
})
