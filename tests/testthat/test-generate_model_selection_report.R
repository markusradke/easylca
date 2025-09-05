test_that('Assertion easylca object works with show_summary', {
  expect_error(generate_model_selection_report(1),
               'Please provide an object of type "easylca".')
})

test_that("report generation creates file in the current wd directory",{
  generate_model_selection_report(testresults)
  is_file <- file.exists(sprintf('summary_%s.html',
                         testresults$settings$folder_name))
  expect_true(is_file)
  if(is_file){file.remove(sprintf('summary_%s.html',
                          testresults$settings$folder_name))}
})
