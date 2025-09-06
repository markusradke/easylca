test_that('get_prediction_for_model returns data frame with expected columns', {
  res <- get_prediction_for_model(random_testresults, 3, 2)
  expect_true('data.frame' %in% class(res))
  expect_setequal(colnames(res) %>% tolower(),
                  c(colnames(testdata), 'cprob1', 'cprob2', 'class') %>% tolower())
})
