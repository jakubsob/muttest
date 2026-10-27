describe("all_mutators", {
  it("returns a flat list of Mutator objects", {
    mutators <- all_mutators()
    expect_gt(length(mutators), 50)
    for (m in mutators) expect_s3_class(m, "Mutator")
  })

  it("includes mutators from presets and standalone constructors", {
    froms <- vapply(all_mutators(), function(m) m$from, character(1))
    expect_true(all(c("+", "<n>L", "stop(...)", "x |> f()", "!<expr>") %in% froms))
  })

  it("does not include the condition-scoped negation removal twice", {
    froms <- vapply(all_mutators(), function(m) m$from, character(1))
    expect_false("!<condition>" %in% froms)
  })

  it("every mutator can be applied to code without error", {
    code <- c("f <- function(x, n = 2L) {", "  if (is.null(x)) return(1)", "  for (i in seq_len(n)) x <- -x[i] + 1", "  x", "}")
    for (m in all_mutators()) expect_no_error(m$mutate(code))
  })
})
