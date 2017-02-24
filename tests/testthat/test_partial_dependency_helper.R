library(brightbox)
context("Generate Grid Range")

dt <- data.table(a = 1:6, b = LETTERS[1:6])

test_that("generate_grid_range_numeric_ does not modify input data.table", {
  actual <- generate_grid_range_numeric_(dt, "a", num_grid = 2)
  expect_equal(ncol(dt), 2)
  expect_equal(nrow(dt), 6)
})

test_that("generate_grid_range_numeric_ properly assigns grid points along range", {
  actual <- generate_grid_range_numeric_(dt, "a", num_grid = 3)
  expected <- data.table(b = rep(LETTERS[1:6], 3),
                         a = c(rep(1, 6),
                               rep(3.5, 6),
                               rep(6, 6)))
  expect_equal(actual, expected)
})

test_that("generate_grid_range_numeric_ properly assigns grid points along custom range", {
  actual <- generate_grid_range_numeric_(dt, "a", num_grid = 3, custom_range = c(-1, -6))
  expected <- data.table(b = rep(LETTERS[1:6], 3),
                         a = c(rep(-1, 6),
                               rep(-3.5, 6),
                               rep(-6, 6)))
  expect_equal(actual, expected)
})

test_that("generate_grid_range_categorical_ does not modify input data.table", {
  actual <- generate_grid_range_categorical_(dt, "a")
  expect_equal(ncol(dt), 2)
  expect_equal(nrow(dt), 6)
})

test_that("generate_grid_range_categorical_ properly assigns grid points along levels", {
  actual <- generate_grid_range_categorical_(dt, "b")
  expected <- data.table(a = rep(1:6, 6),
                         b = c(rep("A", 6),
                               rep("B", 6),
                               rep("C", 6),
                               rep("D", 6),
                               rep("E", 6),
                               rep("F", 6)))
  expect_equal(actual, expected)
})

test_that("generate_grid_range_categorical_ properly assigns grid points along custom range", {
  actual <- generate_grid_range_categorical_(dt, "b", custom_range = c("B", "A", "D"))
  expected <- data.table(a = rep(1:6, 3),
                         b = c(rep("B", 6),
                               rep("A", 6),
                               rep("D", 6)))
  expect_equal(actual, expected)
})
