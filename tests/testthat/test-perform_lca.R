test_that('check for lca_settings object works', {
  expect_error(perform_lca(6),
               'Please provide a settings object of type "lca_settings". It can be generated using the define_lca command.')
})

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
      portrait <- paste0(model_path, 'plots/', class, '_test_model', type, '_diagnostic_portrait.png')
      landscape <- paste0(model_path, 'plots/', class, '_test_model', type, '_diagnostic_landscape.png')

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

  unlink(settings$folder_name, recursive = T)
})

test_that('mplus analysis for only first two models creates necessary files and returns', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 4, starts = 20,
                         use = c('var1', 'var6', 'var7', 'var8'),
                         categorical = c('var1'),
                         censored_below = c('var6', 'var8'),
                         negbin = 'var7',
                         inflated = c('var7', 'var8'))
  modeltypes <- c(1,2)
  results <- perform_lca(settings, modeltypes)

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
      portrait <- paste0(model_path, 'plots/', class, '_test_model', type, '_diagnostic_portrait.png')
      landscape <- paste0(model_path, 'plots/', class, '_test_model', type, '_diagnostic_landscape.png')

      expect_true(file.exists(input),
                  info = paste0('Model ', type, ': Did not write input file for model class ', class, ' ', input, '.'))
      expect_true(file.exists(output),
                  info = paste0('Model ', type, ': Did not write output file for model class ', class, ' ', output, '.'))
      isconverged <- ! is.na(results$summary %>% dplyr::filter(classes == class & modeltype == type) %>% dplyr::pull(Parameters))
      if(isconverged){
        expect_true(file.exists(portrait),
                    info = paste0('Model ', type, ': Did not write diagnostic plot portrait file for model class ', class, ' ', portrait, '.'))
        expect_true(file.exists(landscape),
                    info = paste0('Model ', type, ': Did not write diagnostic plot landscape file for model class ', class, ' ', landscape, '.'))
      }
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

  unlink(settings$folder_name, recursive = T)
})

test_that('prints elpased time for perform_lca', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 2, starts = 10,
                         use = c('var1', 'var2'), categorical = c('var1', 'var2'))
  expect_message(perform_lca(settings, modeltypes = 1),
                 regexp = 'The time needed for the analysis was: (\\d{1,2}|\\d{2,}):([0-5]?\\d):([0-5]?\\d).')
  unlink(settings$folder_name, recursive = T)
})

test_that('works also for only metric',{
  settings <- define_lca(testdata, 'test', 'id', nclasses = 2, starts = 40,
                         use = c('var3', 'var4'))
  expect_error(perform_lca(settings, modeltypes = 1), NA)
  unlink(settings$folder_name, recursive = T)
})
