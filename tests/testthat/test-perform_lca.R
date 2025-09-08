test_that('check for lca_settings object works', {
  expect_error(perform_lca(6),
               'Please provide a settings object of type "lca_settings". It can be generated using the define_lca command.')
})

perform_test_lca <- function(settings, modeltypes){
  message('Performing test lca...')
  capture.output(perform_lca(settings, modeltypes = modeltypes))
  results <- readRDS(paste0(settings$folder_name, '/', settings$analysis_name,
                            '_lca_results.rds'))
  results
}

test_that('mplus analysis creates necessary files and returns result', {
  settings <- define_lca(random_testdata, 'test', 'id', nclasses = 2, starts = 5,
                         categorical = c('var1', 'var2'),
                         censored_below = c('var6', 'var8'),
                         poisson = 'var7',
                         inflated = c('var7', 'var8'))

  modeltypes <- c(2, 4)
  results <- perform_test_lca(settings, modeltypes)


  for(type in modeltypes){
    model_path <- paste0(settings$folder_name, '/test_model', type, '_lca/')
    template <- paste0(model_path, 'test_model', type, '_template.txt')
    data <- paste0(model_path, 'test_model', type, '_lca.dat')
    log <- paste0(model_path, 'test_model', type, '_log.txt')

    expect_true(file.exists(template),
                info = paste0('Model ', type, ': Did not write file for model template ', template, '.'))

    expect_true(file.exists(data),
                info = paste0('Model ', type, ': Did not write file for model data ', data, '.'))

    expect_true(file.exists(log),
                info = paste0('Model ', type, ': Did not write file for model log ', log, '.'))

    for(class in seq(settings$nclasses)){
      input <- paste0(model_path, class, '_test_model', type, '_lca.inp')
      output <- paste0(model_path, class, '_test_model', type, '_lca.out')
    }
  }

  expect_true(file.exists(paste0(settings$folder_name, '/test_lca_results.rds')),
              info = 'Did not write general results file.')

  expect_setequal(results$summary %>% colnames, c('classes', 'Title', 'Parameters', 'LL', 'AIC', 'AICC', 'BIC', 'saBIC',
                                                  'Entropy', 'nmin', 'replicated', 'boundary_values', 'modeltype'))

  expect_equal(results$settings %>% class, c('lca_settings'))
  expect_equal(results %>% class, 'easylca')

  unlink(settings$folder_name, recursive = T)
})


