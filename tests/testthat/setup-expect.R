expect_mutates_to <- function(mutator, input, expected_list) {
  mutations <- mutator$mutate(input)
  expect_equal(mutations, expected_list)
}

expect_no_mutations <- function(mutator, input) {
  expect_null(mutator$mutate(input))
}

expect_mutation_count <- function(mutator, input, n) {
  expect_length(mutator$mutate(input), n)
}
