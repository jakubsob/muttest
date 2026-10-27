describe("drop_argument", {
  it("drops each argument in turn", {
    expect_mutates_to(drop_argument(), "f(a, b, c)", list("f(b, c)", "f(a, c)", "f(a, b)"))
  })

  it("keeps named arguments intact", {
    expect_mutates_to(drop_argument(), "f(a, n = 1)", list("f(n = 1)", "f(a)"))
  })

  it("ignores single-argument calls and return()", {
    expect_no_mutations(drop_argument(), c("f(a)", "return(a, b)"))
  })

  it("can be scoped to named functions, including namespaced calls", {
    code <- c("c(a, b)", "paste(a, b)", "base::c(a, b)")
    mutations <- mutate_codes(drop_argument("c"), code)
    expect_length(mutations, 4)
    expect_equal(mutations[[1]], c("c(b)", "paste(a, b)", "base::c(a, b)"))
  })

  it("handles multi-line argument lists", {
    code <- c("f(", "  a,", "  b", ")")
    expect_mutates_to(drop_argument(), code, list("f(b)", "f(a)"))
  })
})

describe("reverse_arguments", {
  it("reverses c() by default", {
    expect_mutates_to(reverse_arguments(), "c(a, b, d)", list("c(d, b, a)"))
  })

  it("ignores other functions by default", {
    expect_no_mutations(reverse_arguments(), "paste(a, b)")
  })

  it("skips palindromic argument lists", {
    expect_no_mutations(reverse_arguments(), "c(a, a)")
  })

  it("can target every call", {
    expect_mutates_to(reverse_arguments(NULL), "paste(a, b)", list("paste(b, a)"))
  })
})

describe("remove_validation", {
  it("replaces stop() in an if body with NULL", {
    expect_mutates_to(
      remove_validation(),
      "if (!ok) stop('bad')",
      list("if (!ok) NULL")
    )
  })

  it("covers stopifnot, abort and assert_ calls", {
    code <- c("stopifnot(x)", "rlang::abort('m')", "assert_string(x)")
    expect_mutation_count(remove_validation(), code, 3)
  })

  it("ignores unrelated calls", {
    expect_no_mutations(remove_validation(), "print(x)")
  })

  it("accepts a custom pattern", {
    expect_mutates_to(remove_validation("^check$"), "check(x)", list("NULL"))
  })
})

describe("flip_named_flag", {
  it("flips TRUE and FALSE in named arguments", {
    expect_mutates_to(
      flip_named_flag(),
      "mean(x, na.rm = TRUE)",
      list("mean(x, na.rm = FALSE)")
    )
    expect_mutates_to(flip_named_flag(), "f(a = FALSE)", list("f(a = TRUE)"))
  })

  it("ignores positional flags and function defaults", {
    expect_no_mutations(flip_named_flag(), c("f(TRUE)", "g <- function(x = TRUE) x"))
  })

  it("can be scoped to argument names", {
    code <- "f(na.rm = TRUE, other = FALSE)"
    expect_mutates_to(flip_named_flag("na.rm"), code, list("f(na.rm = FALSE, other = FALSE)"))
  })
})

describe("remove_pipe_stage", {
  it("drops the native pipe stage", {
    expect_mutates_to(remove_pipe_stage(), "x |> f()", list("x"))
  })

  it("drops the magrittr pipe stage", {
    expect_mutates_to(remove_pipe_stage(), "y <- x %>% f()", list("y <- x"))
  })

  it("removes each stage of a chain in turn", {
    expect_mutates_to(remove_pipe_stage(), "x |> f() |> g()", list("x |> f()", "x |> g()"))
  })

  it("ignores other operators", {
    expect_no_mutations(remove_pipe_stage(), "a + b")
  })
})

describe("seq_len_decrement", {
  it("subtracts one from the length", {
    expect_mutates_to(seq_len_decrement(), "for (i in seq_len(n)) f(i)", list("for (i in seq_len(n - 1L)) f(i)"))
  })

  it("ignores other calls", {
    expect_no_mutations(seq_len_decrement(), "seq_along(x)")
  })
})

describe("seq_along_drop_first", {
  it("drops the first index", {
    expect_mutates_to(seq_along_drop_first(), "seq_along(x)", list("seq_along(x)[-1L]"))
  })

  it("ignores other calls", {
    expect_no_mutations(seq_along_drop_first(), "seq_len(x)")
  })
})

describe("colon ranges", {
  it("increments a non-literal start", {
    expect_mutates_to(colon_start_increment(), "a:b", list("(a + 1L):b"))
  })

  it("decrements a non-literal end", {
    expect_mutates_to(colon_end_decrement(), "1:n", list("1:(n - 1L)"))
  })

  it("leaves literal bounds to the numeric mutators", {
    expect_no_mutations(colon_start_increment(), "1:10")
    expect_no_mutations(colon_end_decrement(), "1:10")
  })

  it("ignores namespace operators and other binary operators", {
    expect_no_mutations(colon_end_decrement(), c("a::b", "a + b"))
  })
})
