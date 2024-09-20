test_that('show_model returns data frame', {
  res <- show_model(testresults, 3, 3)
  expect_true('data.frame' %in% class(res))
  expect_setequal(colnames(res),
                  c('item', 'param', 'est', 'se', 'est_se', 'pval', 'segment',
                    'level', 'count', 'upper', 'lower', 'pzero'))
})
