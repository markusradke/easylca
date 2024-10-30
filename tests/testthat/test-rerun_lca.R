test_that('create correct frame if no input is supplied', {
  lca <- testresults
  models_and_starts <- data.frame(classes = c(3,3,3,3,3,3),
                                  modeltype = c(1,2,3,4,5,6),
                                  starts = c(40,40,40,40,40,40))
  expect_equal(create_models_and_starts_for_rerun(lca) %>% dplyr::arrange(classes, modeltype, starts), models_and_starts)
})

test_that('Assertion models_and_starts frame as well as lca_settings object are correct', {
    expect_error(suppressMessages(rerun_lca(testresults, data.frame(test = c(1,2,3)))),
                           'models_and_frame parameter does not contain all neccessary columns for the operation. Please include "classes", "modeltype" and "start".')

    expect_error(suppressMessages(rerun_lca(testresults, data.frame(classes = c(1,2,3), modeltype = c(1,2,7), starts = c(20, 20, 20)))),
                           'Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')

    expect_error(suppressMessages(rerun_lca(testresults, data.frame(classes = c(1,2,3), modeltype = c(1,0,6), starts = c(20, 20, 20)))),
                           'Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')

    expect_error(suppressMessages(rerun_lca(testresults, data.frame(classes = c(-1,2,3), modeltype = c(1,2,6), starts = c(20, 20, 20)))),
                           'Please make sure models_and_frame contains only classes with integers > 0.')

    expect_error(suppressMessages(rerun_lca(testresults, data.frame(classes = c(1,2.5,3), modeltype = c(1,2,6), starts = c(20, 20, 20)))),
                           'Please make sure models_and_frame contains only classes with integers > 0.')

    expect_error(suppressMessages(rerun_lca(c(1,2,3))),
                 'Please make sure to supply an object of type "easylca" as input to the rerun_lca function. \n"easylca" objects can be obtained through the perform_lca command.')
})


test_that('update settings from starts works', {
  settings <- testresults$settings
  models_and_starts <- data.frame(classes = c(3,3,3,3,3,3),
                                  modeltype = c(1,2,3,4,5,6),
                                  starts = c(40,30,30,40,50,60))
  settings$starts <- list(c(20, 20, 40),
                          c(20, 20, 30),
                          c(20, 20, 30),
                          c(20, 20, 40),
                          c(20, 20, 50),
                          c(20, 20, 60))
  expect_equal(update_starts_in_settings(testresults$settings, models_and_starts),
               settings)
})

test_that('creates templates and performs rerun', {
  get_path_from_type <- function(type){
    paste0(settings$folder_name, '/', settings$analysis_name, '_model', type)
  }

  perform_lca(testresults$settings, modeltypes = c(1,2))
  models_and_starts <- data.frame(modeltype = c(1,2,2),
                                  classes = c(3,2,3),
                                  starts = c(40,40,40))
  rerun_lca(testresults, models_and_starts)
  settings <- testresults$settings
  for(i in seq(6)){
    model_path <- paste0(get_path_from_type(i), '_template.txt')
    expect_true(file.exists(model_path),
                info = paste0('Did not write file for model ', i, '...'))
  }

  out_path_13 <- paste0(get_path_from_type(1), '_lca/03_test_model1_lca.out')
  out_path_22 <- paste0(get_path_from_type(2), '_lca/02_test_model2_lca.out')
  out_path_23 <- paste0(get_path_from_type(2), '_lca/03_test_model2_lca.out')
  expect_true(file.exists(out_path_13))
  expect_true(file.exists(out_path_22))
  expect_true(file.exists(out_path_23))
  unlink(settings$folder_name, recursive = T)
})

