# Package index

## Core

Run mutation tests and define test plans.

- [`muttest()`](https://jakubsob.github.io/muttest/reference/muttest.md)
  : Run a mutation test
- [`muttest_plan()`](https://jakubsob.github.io/muttest/reference/muttest_plan.md)
  : Create a plan for mutation testing

## Mutators

Individual mutators — each defines one kind of code change.

- [`Mutator`](https://jakubsob.github.io/muttest/reference/Mutator.md) :
  Mutator
- [`operator()`](https://jakubsob.github.io/muttest/reference/operator.md)
  : Mutate a binary operator
- [`boolean_literal()`](https://jakubsob.github.io/muttest/reference/boolean_literal.md)
  : Mutate a boolean literal
- [`na_literal()`](https://jakubsob.github.io/muttest/reference/na_literal.md)
  : Mutate an NA or NULL literal
- [`call_name()`](https://jakubsob.github.io/muttest/reference/call_name.md)
  : Mutate a function call name
- [`string_empty()`](https://jakubsob.github.io/muttest/reference/string_empty.md)
  : Mutate non-empty string literals to the empty string
- [`string_fill()`](https://jakubsob.github.io/muttest/reference/string_fill.md)
  : Mutate the empty string literal to a placeholder string
- [`numeric_increment()`](https://jakubsob.github.io/muttest/reference/numeric_increment.md)
  : Increment numeric literals
- [`numeric_decrement()`](https://jakubsob.github.io/muttest/reference/numeric_decrement.md)
  : Decrement numeric literals
- [`index_increment()`](https://jakubsob.github.io/muttest/reference/index_increment.md)
  : Increment subscript indices
- [`index_decrement()`](https://jakubsob.github.io/muttest/reference/index_decrement.md)
  : Decrement subscript indices
- [`negate_condition()`](https://jakubsob.github.io/muttest/reference/negate_condition.md)
  : Negate the condition of if/while statements
- [`remove_condition_negation()`](https://jakubsob.github.io/muttest/reference/remove_condition_negation.md)
  : Remove negation from the condition of if/while statements
- [`remove_negation()`](https://jakubsob.github.io/muttest/reference/remove_negation.md)
  : Remove logical negation
- [`replace_return_value()`](https://jakubsob.github.io/muttest/reference/replace_return_value.md)
  : Replace the value in explicit return() calls
- [`delete_statement()`](https://jakubsob.github.io/muttest/reference/delete_statement.md)
  : Delete statements one at a time

## Mutator presets

Ready-made collections of related mutators for common bug categories.

- [`arithmetic_operators()`](https://jakubsob.github.io/muttest/reference/arithmetic_operators.md)
  : Arithmetic operator mutators
- [`comparison_operators()`](https://jakubsob.github.io/muttest/reference/comparison_operators.md)
  : Comparison operator mutators
- [`logical_operators()`](https://jakubsob.github.io/muttest/reference/logical_operators.md)
  : Logical operator mutators
- [`boolean_literals()`](https://jakubsob.github.io/muttest/reference/boolean_literals.md)
  : Boolean literal mutators
- [`na_literals()`](https://jakubsob.github.io/muttest/reference/na_literals.md)
  : NA and NULL literal mutators
- [`numeric_literals()`](https://jakubsob.github.io/muttest/reference/numeric_literals.md)
  : Numeric literal mutators
- [`index_mutations()`](https://jakubsob.github.io/muttest/reference/index_mutations.md)
  : Index mutation mutators
- [`string_literals()`](https://jakubsob.github.io/muttest/reference/string_literals.md)
  : String literal mutators
- [`condition_mutations()`](https://jakubsob.github.io/muttest/reference/condition_mutations.md)
  : Condition mutation mutators

## Reporters

Control how results are displayed.

- [`MutationReporter`](https://jakubsob.github.io/muttest/reference/MutationReporter.md)
  : Reporter for Mutation Testing
- [`ProgressMutationReporter`](https://jakubsob.github.io/muttest/reference/ProgressMutationReporter.md)
  : Progress Reporter for Mutation Testing
- [`JSONMutationReporter`](https://jakubsob.github.io/muttest/reference/JSONMutationReporter.md)
  : JSON Reporter for Mutation Testing
- [`MultiReporter`](https://jakubsob.github.io/muttest/reference/MultiReporter.md)
  : Combine Several Mutation Reporters
- [`default_reporter()`](https://jakubsob.github.io/muttest/reference/default_reporter.md)
  : Create a default reporter
- [`report()`](https://jakubsob.github.io/muttest/reference/report.md) :
  Build a source-annotated HTML report from a muttest JSON file

## Strategies

Control how tests are run and source files are copied.

- [`TestStrategy`](https://jakubsob.github.io/muttest/reference/TestStrategy.md)
  : TestStrategy interface
- [`FileTestStrategy`](https://jakubsob.github.io/muttest/reference/FileTestStrategy.md)
  : Run tests matching the mutated source file name
- [`FullTestStrategy`](https://jakubsob.github.io/muttest/reference/FullTestStrategy.md)
  : Run all tests for a mutant
- [`CopyStrategy`](https://jakubsob.github.io/muttest/reference/CopyStrategy.md)
  : CopyStrategy interface
- [`PackageCopyStrategy`](https://jakubsob.github.io/muttest/reference/PackageCopyStrategy.md)
  : Package copy strategy
- [`default_test_strategy()`](https://jakubsob.github.io/muttest/reference/default_test_strategy.md)
  : Create a default run strategy
- [`default_copy_strategy()`](https://jakubsob.github.io/muttest/reference/default_copy_strategy.md)
  : Create a default project copy strategy
