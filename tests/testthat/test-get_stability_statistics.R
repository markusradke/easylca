test_that("max BIC retrieval works", {
  cases <- stability_check_testcases
  expect_equal(get_max_bic_diff(cases), 0)
})
