# Build a source-annotated HTML report from a muttest JSON file

Reads a JSON file produced by
[JSONMutationReporter](https://jakubsob.github.io/muttest/reference/JSONMutationReporter.md)
(the
[mutation-testing-elements](https://github.com/stryker-mutator/mutation-testing-elements)
schema) and renders a self-contained HTML report: overall score,
per-file breakdown, and every mutant overlaid on its source line.
Mutated lines expand to show each mutation as a diff. A small inline
script adds status filtering (the report opens focused on survived
mutants) and `n`/`p` keyboard navigation between mutated lines.

## Usage

``` r
report(json = "muttest.json", output = sub("\\.json$", ".html", json))
```

## Arguments

- json:

  Path to the JSON file written by
  [JSONMutationReporter](https://jakubsob.github.io/muttest/reference/JSONMutationReporter.md).

- output:

  Path of the HTML file to write. Defaults to the JSON path with an
  `.html` extension.

## Value

The `output` path, invisibly.

## Details

The report is a single static file. Syntax highlighting is loaded from a
CDN ([shiki](https://shiki.style)) when online and is skipped gracefully
offline.
