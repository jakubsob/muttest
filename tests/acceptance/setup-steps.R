cucumber::before(function(context, scenario_name) {
  dir <- fs::path(tempdir(), "__muttest_tmp___")
  fs::dir_create(dir)
  context$dir <- dir
})

cucumber::given(
  "I have a {string} file with",
  function(filename, code, context) {
    file <- fs::path(context$dir, filename)
    fs::dir_create(fs::path_dir(file))
    writeLines(code, file)
  }
)

cucumber::given("I clone a repository from {string}", function(url, context) {
  path <- get_repo(url)
  context$dir <- path
})

cucumber::when("I run mutation tests with", function(code, context) {
  withr::with_output_sink(new = nullfile(), {
    withr::with_dir(context$dir, {
      context$score <- eval(parse(text = code))
    })
  })
})

cucumber::then("the mutation score should be {float}", function(score, context) {
  expect_equal(context$score, score)
})

cucumber::then("the mutation score should be {word}", function(score, context) {
  if (score == "NA") {
    score <- NA_real_
  }
  expect_equal(context$score, score)
})

cucumber::then("I get a mutation score", function(context) {
  expect_true(!is.na(context$score))
})

cucumber::after(function(context, scenario_name) {
  fs::dir_delete(context$dir)
})
