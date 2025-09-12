get_path_from_type <- function(settings, type){
  paste0(settings$folder_name, '/modeltype_', type)
}

test_that('create correct frame if no input is supplied', {
  lca <- random_testresults
  models_and_starts <- data.frame(classes = c(3,3,3,3,3,3),
                                  modeltype = c(1,2,3,4,5,6),
                                  starts = c(40,40,40,40,40,40))
  expect_equal(create_models_and_starts_for_rerun(lca) %>%
                 dplyr::arrange(classes, modeltype, starts),
               models_and_starts)
})

test_that('Assertion models_and_starts frame as well as lca_settings object are correct', {
    expect_error(suppressMessages(
      rerun_lca(random_testresults,
                data.frame(test = c(1,2,3)))),
                'models_and_frame parameter does not contain all neccessary columns for the operation. Please include "classes", "modeltype" and "starts".')

    expect_error(suppressMessages(
      rerun_lca(random_testresults,
                data.frame(classes = c(1,2,3),
                modeltype = c(1,2,7),
                starts = c(20, 20, 20)))),
      'Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')

    expect_error(suppressMessages(
      rerun_lca(random_testresults,
                data.frame(classes = c(1,2,3),
                           modeltype = c(1,0,6),
                           starts = c(20, 20, 20)))),
      'Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')

    expect_error(suppressMessages(
      rerun_lca(random_testresults,
                data.frame(classes = c(-1,2,3),
                           modeltype = c(1,2,6),
                           starts = c(20, 20, 20)))),
      'Please make sure models_and_frame contains only classes with integers > 0.')

    expect_error(suppressMessages(
      rerun_lca(random_testresults,
                data.frame(classes = c(1,2.5,3),
                           modeltype = c(1,2,6),
                           starts = c(20, 20, 20)))),
      'Please make sure models_and_frame contains only classes with integers > 0.')

    expect_error(suppressMessages(
      rerun_lca(c(1,2,3))),
      'Please make sure to supply an object of type "easylca" as input to the rerun_lca function. \n"easylca" objects can be obtained through the perform_lca command.')
})


test_that('update settings from starts works', {
  settings <- random_testresults$settings
  models_and_starts <- data.frame(classes = c(3,3,3,3,3,3),
                                  modeltype = c(1,2,3,4,5,6),
                                  starts = c(40,30,30,40,50,60))
  settings$starts <- list(c(20, 20, 40),
                          c(20, 20, 30),
                          c(20, 20, 30),
                          c(20, 20, 40),
                          c(20, 20, 50),
                          c(20, 20, 60))
  expect_equal(update_starts_in_settings(random_testresults$settings, models_and_starts),
               settings)
})

perform_test_lca <- function(settings, modeltypes){
  message('Performing test lca...')
  capture.output(perform_lca(settings, modeltypes = modeltypes))
  results <- readRDS(paste0(settings$folder_name, '/', settings$analysis_name,
                            '_lca_results.rds'))
  results
}

if(is_mplus_installed()){
  test_that('creates templates and performs rerun with new classes', {
    settings <- define_lca(random_testdata, 'test', 'id', nclasses = 2, starts = 5,
                           use = c('var3', 'var4'))
    results <- perform_test_lca(settings, modeltypes = c(1,2))
    models_and_starts <- data.frame(modeltype = c(1,2),
                                    classes = c(2,1),
                                    starts = c(5,5))
    capture.output(rerun_lca(results, models_and_starts))
    for(i in c(1 , 2)){
      model_path <- paste0(get_path_from_type(settings, i))
      template_path <- sprintf('%s/modeltype_%d_template.txt',
                               model_path,i)
      expect_true(file.exists(template_path),
                  info = sprintf('Did not write template file for model %d...',
                                 i))
    }

    out_path_12 <- paste0(get_path_from_type(settings, 1), '/02_classes.out')
    out_path_21 <- paste0(get_path_from_type(settings, 2), '/01_classes.out')
    expect_true(file.exists(out_path_12))
    expect_true(file.exists(out_path_21))
    unlink(settings$folder_name, recursive = T)
  })
}


if(is_mplus_installed()){
  test_that('performs reruns with a higher number of classes', {
    results <- rerun_lca(random_testresults,
                         models_and_starts = data.frame(classes = 4,
                                                        modeltype = 1,
                                                        starts = 5))
    out_path_14 <- paste0(get_path_from_type(random_testresults$settings, 1),
                          '/04_classes.out')
    expect_true('modeltype_1.03_classes.out' %in% names(results$models$modeltype_1))
    expect_true('modeltype_1.04_classes.out' %in% names(results$models$modeltype_1))
    expect_true(file.exists(out_path_14))
    unlink(random_testresults$settings$folder_name, recursive = T)
  })
}
