test_that('show_summary does not throw error with testresults', {
  expect_silent(show_summary(testresults))
})

test_that('Assertion easylca object works with show_summary', {
  expect_error(show_summary(1),
               'Please provide an object of type "easylca".')
})
