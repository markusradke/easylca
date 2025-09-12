test_that("retrieve local minima from given vector works", {
  expect_equal(is_local_minimum(c(2,1,2)), c(FALSE, TRUE, FALSE))
  expect_equal(is_local_minimum(c(1, 2, 1 ,2)), c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(is_local_minimum(c(1, 2, 3, 2)), c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(is_local_minimum(c(1, 2, 3, 2, 1)),
               c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(is_local_minimum(c(2, 1, 3, 2, 4)),
               c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(is_local_minimum(c(1, NA, 3, 2, 3)),
               c(FALSE, FALSE, FALSE, TRUE, FALSE))
})
