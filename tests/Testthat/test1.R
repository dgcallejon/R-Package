context("Correct Output")

test_that("adds the first two elements of a vector", {
  expect_equal(addFirstTwo(c(4,5,6)), 9)
  expect_equal(addFirstTwo(c(1,2,6)), 3)
  expect_equal(addFirstTwo(c(3,3)), 6)
})

