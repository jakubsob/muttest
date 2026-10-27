expect_preset <- function(preset, ...) {
  expect_gt(length(preset), 0)
  for (m in preset) expect_s3_class(m, "Mutator")
}

describe("extra presets", {
  it("all return lists of Mutator objects", {
    expect_preset(function_swaps())
    expect_preset(membership_operators())
    expect_preset(operand_elision())
    expect_preset(branch_mutations())
    expect_preset(numeric_boundaries())
    expect_preset(numeric_type_swaps())
    expect_preset(sequence_mutations())
  })
})

describe("function_swaps", {
  it("swaps in both directions", {
    mutations <- unlist(lapply(function_swaps(), function(m) mutate_codes(m, "any(x)")), recursive = FALSE)
    expect_equal(mutations, list("all(x)"))
    mutations <- unlist(lapply(function_swaps(), function(m) mutate_codes(m, "ncol(x)")), recursive = FALSE)
    expect_equal(mutations, list("nrow(x)"))
  })
})

describe("branch_mutations", {
  it("produces mutants for an if/else", {
    n <- sum(vapply(
      branch_mutations(),
      function(m) length(m$mutate("if (a) b else c")),
      integer(1)
    ))
    # TRUE, FALSE, remove else, empty body
    expect_equal(n, 4)
  })
})

describe("sequence_mutations", {
  it("produces a mutant per applicable mutator for a loop", {
    n <- sum(vapply(
      sequence_mutations(),
      function(m) length(m$mutate("for (i in seq_len(n)) x[i]")),
      integer(1)
    ))
    expect_equal(n, 1)
  })
})
