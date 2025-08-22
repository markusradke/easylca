test_that("is data frame type assertion works", {
  expect_error(assert_data_format(1, 'id'),
               'Please provide an object of class data.frame or tibble for analysis.')
})

test_that("is all discrete indicators <= 10 levels works", {
  testdata <- data.frame(var1 = seq(11) %>% as.integer(),
                         id = 'id')
  expect_error(assert_data_format(testdata, 'id'),
               'Please make sure all discrete indicators have <= 10 levels. Make sure that all continuous varialbes are explictly coded as doubles by using as.double.\nProblem caused by: var1')

  only_continuous <- data.frame(var1 = seq(10) %>% as.numeric(),
                         id = 'id')
  expect_no_error(assert_data_format(only_continuous, 'id'))

})

test_that("is all indicators are either double or integer", {
  testdata <- data.frame(var1 = 'character',
                         var2 = 'character',
                         id = 'id')
  expect_error(assert_data_format(testdata, 'id'),
               'Please make sure all indicators are coded as either double or integer.\nProblem caused by: var1, var2')

  testdata <- data.frame(var1 = c(1,2) %>% as.integer(),
                         var2 = c(1., 2.1) %>% as.double(),
                         id = 'id')
  expect_no_condition(assert_data_format(testdata, 'id'))
})

test_that("report generation creates file in the current wd directory",{
  testdata <- data.frame(var1 = seq(10) %>% as.integer(),
                         id = 'id')
  generate_data_diagnosis_report(testdata, 'id', 'testreport')
  is_file <- file.exists('data_diagnosis_testreport.html')
  expect_true(is_file)
  if(is_file){file.remove('data_diagnosis_testreport.html')}
})
