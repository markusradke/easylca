test_that('mplus analysis for all models creates necessary files and returns', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 2, starts = 20)
  perform_lca(settings)

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

    for(class in seq(2)){
      input <- paste0(model_path, class, '_test_model', type, '_lca.inp')
      output <- paste0(model_path, class, '_test_model', type, '_lca.out')

      expect_true(file.exists(input),
                  info = paste0('Model ', type, ': Did not write input file for model class ', class, ' ', input, '.'))
      expect_true(file.exists(output),
                  info = paste0('Model ', type, ': Did not write output file for model class ', class, ' ', output, '.'))
    }
  }



  # unlink(settings$folder_name, recursive = T)
})

# test_that('returns an object of type easylca', {
#   settings <- define_lca(testdata, 'test', 'id')
#   expect_equal(perform_lca(settings) %>% class, 'easylca')
# })

# test_that('return contains settings, models, and plots', {
#   settings <- define_lca(testdata, 'test', 'id')
#   results <- perform_lca(settings)
#
#   expect_equal(results$settings %>% class, 'lca_settings')
#   expect_equal(results$models %>% class, 'LCA MOdel')
#   expect_equal(results$plots[[1]] %>% class, c('gg', 'ggplot'))
# })
