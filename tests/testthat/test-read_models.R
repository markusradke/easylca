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
  unlink(settings$folder_name)
})
