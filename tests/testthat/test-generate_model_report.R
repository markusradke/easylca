perform_test_lca <- function(settings, modeltypes) {
  message('Performing test lca...')
  capture.output(perform_lca(settings, modeltypes = modeltypes))
  results <- readRDS(paste0(
    settings$folder_name,
    '/',
    settings$analysis_name,
    '_lca_results.rds'
  ))
  results
}

test_that('assert easylca object', {
  expect_error(
    generate_model_report(1, 1, 1),
    'Please provide an object of type "easylca".'
  )
})

test_that('assert solution has been modeled', {
  expect_error(
    assert_solution_in_results(random_testresults, modeltype = 1, classes = 4),
    'You are looking for solution that has not been estimated. Please check modeltype and class number.'
  )

  expect_error(
    assert_solution_in_results(random_testresults, modeltype = 7, classes = 3),
    'You are looking for solution that has not been estimated. Please check modeltype and class number.'
  )
})

test_that("report generation creates file in the current wd directory", {
  generate_model_report(random_testresults, modeltype = 1, classes = 3)
  # please change name if changing test results
  is_file <- file.exists('modeltype-1_3-classes_test_20250910_17-17.html')
  expect_true(is_file)
  if (is_file) {
    file.remove('modeltype-1_3-classes_test_20250910_17-17.html')
  }
})

if (is_mplus_installed()) {
  test_that("report genreation works for only discret variables", {
    data <- dplyr::select(titanic_passengers, id, survived, pasclass)
    settings <- define_lca(
      data,
      'test',
      'id',
      nclasses = 2,
      starts = 5,
      categorical = 'survived',
      nominal = 'pasclass'
    )
    res <- perform_test_lca(settings, modeltypes = 1)
    generate_model_report(res, modeltype = 1, classes = 2)
    path <- sprintf('modeltype-1_2-classes_%s.html', settings$folder_name)
    is_file <- file.exists(path)
    expect_true(is_file)
    unlink(settings$folder_name, recursive = T)
    if (is_file) {
      file.remove(path)
    }
  })
}

test_that("report genreation works on wehner data", {
  generate_model_report(wehner_testresults, modeltype = 1, classes = 4)
  path <- sprintf(
    'modeltype-1_4-classes_%s.html',
    wehner_testresults$settings$folder_name
  )
  is_file <- file.exists(path)
  expect_true(is_file)
  if (is_file) {
    file.remove(path)
  }
})
