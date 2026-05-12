source(file.path("..", "..", "R", "shipping.R"))

test_that("heavy packages cost more than light ones", {
  expect_gt(shipping_cost(10), shipping_cost(2))
})

test_that("5kg falls into the lower-cost tier", {
  expect_equal(shipping_cost(5), 5.00)
})
