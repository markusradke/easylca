test_that('assert easylca object', {
  expect_error(generate_model_report(1, 1, 1), 'Please provide an object of type "easylca".')
})

test_that('assert number of classes has been modeled', { # TODO only catches the case when initial number of classes has not been surpassed
  expect_error(generate_model_report(random_testresults, modeltype = 1, classes = 4),
               'You are looking for a class solution that has not been modeled.')
})

test_that('assert modeltype has been modeled', { # TODO only catches the case when initial number of classes has not been surpassed
  expect_error(generate_model_report(random_testresults, modeltype = 7, classes = 3),
               'You are looking for a modeltype that has not been modeled.')
})

test_that("report generation creates file in the current wd directory",{
  generate_model_report(random_testresults, modeltype = 1, classes = 3)
  is_file <- file.exists('modeltype-1_3-classes_test_20240920_14-21.html')
  expect_true(is_file)
  if(is_file){file.remove('modeltype-1_3-classes_test_20240920_14-21.html')}
})
