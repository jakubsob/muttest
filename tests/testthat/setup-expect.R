# mutate() returns records {code, location, replacement}; most tests only care
# about the mutated code, so unwrap to the code variants.
mutate_codes <- function(mutator, input) {
  lapply(mutator$mutate(input), `[[`, "code")
}

expect_mutates_to <- function(mutator, input, expected_list) {
  expect_equal(mutate_codes(mutator, input), expected_list)
}

expect_no_mutations <- function(mutator, input) {
  expect_null(mutator$mutate(input))
}

expect_mutation_count <- function(mutator, input, n) {
  expect_length(mutator$mutate(input), n)
}
