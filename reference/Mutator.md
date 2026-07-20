# Mutator

Mutator

Mutator

## Details

Represents a single code mutation — a pattern to find and a replacement
to apply. Every mutator function
([`operator()`](https://jakubsob.github.io/muttest/reference/operator.md),
[`boolean_literal()`](https://jakubsob.github.io/muttest/reference/boolean_literal.md),
etc.) returns an instance of this class.

## Public fields

- `from`:

  The token or operator to replace.

- `to`:

  The replacement token or operator.

- `query`:

  Tree-sitter query used to locate candidate nodes.

- `match_fn`:

  Optional `function(node_text)` returning `logical`; overrides the
  default `node_text == from` equality check.

- `replacement_fn`:

  Optional `function(node_text)` returning a string; overrides the
  static `to` value as the replacement text.

- `mutate_fn`:

  Optional `function(code)` that fully replaces the default mutation
  logic when set.

## Methods

### Public methods

- [`Mutator$new()`](#method-Mutator-new)

- [`Mutator$mutate()`](#method-Mutator-mutate)

- [`Mutator$print()`](#method-Mutator-print)

- [`Mutator$clone()`](#method-Mutator-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Mutator`.

#### Usage

    Mutator$new(
      from,
      to,
      query,
      match_fn = NULL,
      replacement_fn = NULL,
      mutate_fn = NULL
    )

#### Arguments

- `from`:

  Token to replace.

- `to`:

  Replacement token.

- `query`:

  Tree-sitter query string.

- `match_fn`:

  Optional custom match function.

- `replacement_fn`:

  Optional custom replacement function.

- `mutate_fn`:

  Optional `function(code)` that fully overrides the default mutation
  logic.

------------------------------------------------------------------------

### Method `mutate()`

Apply this mutator to a character vector of source lines.

#### Usage

    Mutator$mutate(code)

#### Arguments

- `code`:

  Character vector of source lines.

#### Returns

A list of mutation records (one per match), each a list with `code`
(mutated source lines), `location` (1-based `start`/`end` line/column),
and `replacement` (the inserted text), or `NULL` if the pattern was not
found.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print a short summary of the mutator.

#### Usage

    Mutator$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Mutator$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
