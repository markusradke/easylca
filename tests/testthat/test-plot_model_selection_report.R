test_that("get local minima logical indicator from given vector works", {
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

test_that("get local minima from frame works with different modeltypes works", {
  testdata <- data.frame(Type = c(1,1,1,2,2,2),
                         BIC = c(2, 1, 3, 2, 1, 3))
  expect_equal(get_local_minima(testdata, 'BIC'),
               c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE))
  testdata <- data.frame(Type = c(1,1,1,2,2,2),
                         BIC = c(2, 1, 1, 2, 1, 3))
  expect_equal(get_local_minima(testdata, 'BIC'),
               c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  testdata <- data.frame(Type = c(1,1,1,2,2,2),
                         BIC = c(2, 1, 3, 2, 1, 1))
  expect_equal(get_local_minima(testdata, 'BIC'),
               c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  testdata <- data.frame(Type = c(1,1,1,1,2,2,2,2),
                         BIC = c(2, 1, 2,1, 2, 2, 1, 2))
  expect_equal(get_local_minima(testdata, 'BIC'),
               c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  testdata <- data.frame(Type = c(1,1,1,1,2,2,2,2),
                         BIC = c(2, NA, 2,1, 11, 10, 9, 11))
  expect_equal(get_local_minima(testdata, 'BIC'),
               c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  # if none is TRUE make lowest value TRUE
  testdata <- data.frame(Type = c(1,1,1,1,2,2,2,2),
                         BIC = c(2, NA, 2,1, 11, 10, 9, 8))
  expect_equal(get_local_minima(testdata, 'BIC'),
               c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
})
