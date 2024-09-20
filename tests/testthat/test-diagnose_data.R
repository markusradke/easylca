test_that('Returns historgrams for each variable and a data frame with missings',{
  sink(tempfile())
  res <- suppressMessages(diagnose_data(testdata, 'id'))

  classes <- c(rep(list('histogram'), 8), list('data.frame'))
  expect_equal(res %>% purrr::map(class), classes)

  expect_equal(colnames(res[[9]]), c('variable', 'NA_relfreq'))

  idcs <- sample(1000, 100)
  missings_data <- testdata %>%
    dplyr::select(id, var1) %>%
    dplyr::mutate(var1 = ifelse(dplyr::row_number() %in% idcs, NA, var1))
  test_diagnosis <- suppressMessages(diagnose_data(missings_data, 'id'))
  expect_equal(test_diagnosis[[2]], data.frame(variable = 'var1', NA_relfreq = 0.1))
  sink()
})

test_that('Assertion all cols except id are numeric', {
  wrong_type <- testdata %>% dplyr::mutate(var1 = as.character(var1))
  expect_error(diagnose_data(wrong_type, 'id'),
               'Please provide only numeric and integer data columns. Make sure categorical variables are coded as integers > 0.')
})

test_that('Assertion data.frame is provided', {
  expect_error(diagnose_data(c(1,2,3), 'id'),
               'Please provide an object of class data.frame for analysis.')
})
