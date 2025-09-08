test_that('checks if object is a settings_object', {
  settings <- random_testresults$settings
  expect_no_error(assert_is_settings(settings))
  expect_error(assert_is_settings(1),
               'Please provide an easylca_settings_object as input.')
})

test_that('checks if folder exists',{
  settings <- random_testresults$settings
  expect_error(assert_is_folder(settings),
               'Could not find corresponding folder in current subdirectory for the settings you provided.')
  dir.create(settings$folder_name)
  expect_no_error(assert_is_folder(settings))
  unlink(settings$folder_name, recursive = TRUE, force = TRUE)
})

test_that('looks up which model types were estimated', {
  create_modeltype_folders <- function(settings, types){
    for(i in types){
      dir.create(paste0(settings$folder_name,
                        '/', settings$analysis_name,
                        '_model', i, '_lca'))
    }
    return(NULL)
  }

  settings <- random_testresults$settings
  expect_error(get_estimated_modeltypes(settings),
               'No models were found inside the corresponding folder for given settings.')
  dir.create(settings$folder_name)
  create_modeltype_folders(settings, c(1, 3))
  expect_equal(get_estimated_modeltypes(settings), c(1, 3))
  create_modeltype_folders(settings, c(2, 4, 5, 6))
  expect_equal(get_estimated_modeltypes(settings), seq(6))
  unlink(settings$folder_name, recursive = TRUE, force = TRUE)
})

test_that('successfully reads models for model types and creates easylca object', {
  settings <- define_lca(random_testdata, 'read_models_test', id_variable = 'id', use = c('var1', 'var7'),
                         categorical = 'var1', nclasses = 2, starts = 5, cores = 64)
  message('Performing test LCA...')
  capture.output(
    suppressMessages(perform_lca(settings, modeltypes = c(1, 3)))
  )

  message('Reading test LCA folder...')
  lca <- suppressMessages(read_models(settings))
  expect_true(methods::is(lca, 'easylca'))
  expect_equal(names(lca), c('settings', 'models', 'summary'))
  expect_equal(names(lca$models), c('modeltype_1', 'modeltype_3'))

  file.remove(paste0(settings$folder_name,
                     '/', settings$analysis_name,
                     '_model3_lca/02_', settings$analysis_name,
                     '_model3_lca.out'))
  message('Read test LCA with one class solution deleted...')
  lca <- suppressMessages(read_models(settings))
  expect_equal(length(lca$models$modeltype_3), 1)

  unlink(settings$folder_name, recursive = TRUE, force = TRUE)
})

