test_that('mplus analysis for all models creates necessary files and returns', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 2, starts = 20,
                         categorical = c('var1', 'var2'),
                         censored_below = c('var6', 'var8'),
                         poisson = 'var7',
                         inflated = c('var7', 'var8'))

  results <- perform_lca(settings)

  for(type in seq(6)){
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
      portrait <- paste0(model_path, class, '_test_model', type, '_diagnostic_portrait.png')
      landscape <- paste0(model_path, class, '_test_model', type, '_diagnostic_landscape.png')

      expect_true(file.exists(input),
                  info = paste0('Model ', type, ': Did not write input file for model class ', class, ' ', input, '.'))
      expect_true(file.exists(output),
                  info = paste0('Model ', type, ': Did not write output file for model class ', class, ' ', output, '.'))
      expect_true(file.exists(portrait),
                  info = paste0('Model ', type, ': Did not write diagnostic plot portrait file for model class ', class, ' ', portrait, '.'))
      expect_true(file.exists(landscape),
                  info = paste0('Model ', type, ': Did not write diagnostic plot landscape file for model class ', class, ' ', landscape, '.'))
    }
  }

  expect_true(file.exists(paste0(settings$folder_name, '/test_lca_results.rds')),
              info = 'Did not write general results file.')
  expect_true(file.exists(paste0(settings$folder_name, '/test_summary_portrait.png')),
              info = 'Did not write portrait summary plot file.')
  expect_true(file.exists(paste0(settings$folder_name, '/test_summary_landscape.png')),
              info = 'Did not write landscape summary plot file.')

  expect_setequal(results$summary %>% colnames, c('classes', 'Title', 'Parameters', 'LL', 'AIC', 'AICC', 'BIC', 'saBIC',
                                                  'Entropy', 'nmin', 'replicated', 'boundary_values', 'modeltype'))

  expect_equal(results$settings %>% class, c('lca_settings'))
  expect_equal(results$plots[[1]] %>% class, c('gg', 'ggplot'))
  expect_equal(results %>% class, 'easylca')

  # unlink(settings$folder_name, recursive = T)
})
