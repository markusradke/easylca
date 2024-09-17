test_that('create correct frame if no input is supplied', {
  lca <- testresults
  models_and_starts <- data.frame(classes = c(3,3,3,3,3,3),
                                  modeltype = c(1,2,3,4,5,6),
                                  starts = c(40,40,40,40,40,40))
  expect_equal(create_models_and_starts_for_rerun(lca) %>% dplyr::arrange(classes, modeltype, starts), models_and_starts)
})

test_that('Assertion models_and_starts frame as well as lca_settings object are correct', {
  expect_error(rerun_lca(testresults, data.frame(test = c(1,2,3))),
                         'models_and_frame parameter does not contain all neccessary columns for the operation. Please include "classes", "modeltype" and "start".')

  expect_error(rerun_lca(testresults, data.frame(classes = c(1,2,3), modeltype = c(1,2,7), starts = c(20, 20, 20))),
                         'Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')

  expect_error(rerun_lca(testresults, data.frame(classes = c(1,2,3), modeltype = c(1,0,6), starts = c(20, 20, 20))),
                         'Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')

  expect_error(rerun_lca(testresults, data.frame(classes = c(-1,2,3), modeltype = c(1,2,6), starts = c(20, 20, 20))),
                         'Please make sure models_and_frame contains only classes with integers > 0.')

  expect_error(rerun_lca(testresults, data.frame(classes = c(1,2.5,3), modeltype = c(1,2,6), starts = c(20, 20, 20))),
                         'Please make sure models_and_frame contains only classes with integers > 0.')

  expect_error(rerun_lca(c(1,2,3)),
               'Please make sure to supply an object of type "easylca" as input to the rerun_lca function. \n"easylca" objects can be obtained through the perform_lca command.')
})


test_that('update settings from starts works', {
  settings <- new.env()
  for (var in ls(testresults$settings)) {
    assign(var, get(var, envir = testresults$settings), envir = settings)
  }
  class(settings) <- 'lca_settings'
  models_and_starts <- data.frame(classes = c(3,3,3,3,3,3),
                                  modeltype = c(1,2,3,4,5,6),
                                  starts = c(40,40,40,40,40,40))
  settings$starts <- list(c(20, 20, 40),
                          c(20, 20, 40),
                          c(20, 20, 40),
                          c(20, 20, 40),
                          c(20, 20, 40),
                          c(20, 20, 40))
  expect_equal(update_starts_in_settings(testresults$settings, models_and_starts),
               settings)
})
