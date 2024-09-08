test_that('creates necessary files', {
  settings <- define_lca(testdata, 'test', 'id')
  create_templates(settings)

  for(i in seq(6)){
    model_path <- paste0(settings$folder_name, '/', settings$analysis_name, '_model', i, '_template.txt')
    expect_true(file.exists(model_path),
                info = paste0('Did not write file for model ', i, '...'))
  }
  unlink(settings$folder_name, recursive = T)
})


test_that('headers are created the right way from settings', {
  settings <- define_lca(testdata, 'test', 'id')
  headers <- list()
  for(i in seq(6)){
    extended_name <- paste0('test_model', i, '_lca')
    model_header <- c('[[init]]',
                      'iterators = classes;',
                      'classes = 1:4',
                      paste0('filename = \"[[classes]]_', extended_name,'.inp\";'),
                      paste0('outputDirectory = \"', extended_name, '\";'),
                      paste0('[[/init]]'),
                      '',
                      paste0('TITLE: test[[classes]] classes'),
                      paste0('FILE IS ', extended_name, '_[[classes]].dat;'))
    headers <- c(headers, list(model_header))
  }
  expect_equal(create_headers(settings), headers)
})

test_that('variables specs are created the right way from settings',{
  settings <- define_lca(testdata, 'test', 'id', use = c('var1', 'var2', 'var3', 'var4'), categorical = c('var1', 'var2'))
  variable_settings <- c('VARIABLE:',
                         'NAMES = id var1 var2 var3 var4 var5 var6 var7 var8;',
                         'IDVARIABLE = id;',
                         'USEVARIABLES = var1 var2 var3 var4;',
                         'CATEGORICAL = var1 var2;',
                         'AUXILIARY = var5 var6 var7 var8;',
                         'MISSING = .;',
                         'CLASSES = class ([[classes]]);')
  variable_settings <- rep(list(variable_settings), 6) %>% as.list()
  expect_equal(create_variable_specs(settings), variable_settings)

  settings <- define_lca(testdata, 'test', 'id',
                         use = c('var1', 'var2', 'var3', 'var4', 'var5', 'var6', 'var7', 'var8'),
                         censored_above = c('var1', 'var2'),
                         censored_below = c('var3', 'var4'),
                         poisson = c('var5', 'var6'),
                         negbin = c('var7', 'var8'),
                         inflated = c('var2', 'var4', 'var6', 'var8'))
  variable_settings <- c('VARIABLE:',
                         'NAMES = id var1 var2 var3 var4 var5 var6 var7 var8;',
                         'IDVARIABLE = id;',
                         'USEVARIABLES = var1 var2 var3 var4 var5 var6 var7 var8;',
                         'CENSORED = var1 (a) var2 (ai) var3 (b) var4 (bi);',
                         'COUNT = var5 (p) var6 (pi) var7 (nb) var8 (nbi);',
                         'MISSING = .;',
                         'CLASSES = class ([[classes]]);')
  variable_settings <- rep(list(variable_settings), 6) %>% as.list()
  expect_equal(create_variable_specs(settings), variable_settings)

  settings <- define_lca(testdata, 'test', 'id',
                         use = c('var1', 'var2', 'var3', 'var5', 'var7', 'var8'),
                         censored_above = c('var1', 'var2'),
                         negbin = c('var7', 'var8'),
                         inflated = c('var2', 'var8'))
  variable_settings <- c('VARIABLE:',
                         'NAMES = id var1 var2 var3 var4 var5 var6 var7 var8;',
                         'IDVARIABLE = id;',
                         'USEVARIABLES = var1 var2 var3 var5 var7 var8;',
                         'CENSORED = var1 (a) var2 (ai);',
                         'COUNT = var7 (nb) var8 (nbi);',
                         'AUXILIARY = var4 var6;',
                         'MISSING = .;',
                         'CLASSES = class ([[classes]]);')
  variable_settings <- rep(list(variable_settings), 6) %>% as.list()
  expect_equal(create_variable_specs(settings), variable_settings)
})


test_that('model specifications are created correctly or all model types',{
  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2', 'var3'))
  model1 <- character()
  model2 <- c('MODEL:',
              '%CLASS1%',
              'var1;',
              'var2;',
              'var3;',
              '[[classes > 1]]',
              '%CLASS2%',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS3%',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 2]]')
  model3 <- c('MODEL:',
              '%OVERALL%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;')
  model4 <- c('MODEL:',
              '%OVERALL%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '%CLASS1%',
              'var1;',
              'var2;',
              'var3;',
              '[[classes > 1]]',
              '%CLASS2%',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS3%',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 2]]')
  model5 <- c('MODEL:',
              '%CLASS1%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[classes > 1]]',
              '%CLASS2%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS3%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[/classes > 2]]')
  model6 <- c('MODEL:',
              '%CLASS1%',
              'var1;',
              'var2;',
              'var3;',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[classes > 1]]',
              '%CLASS2%',
              'var1;',
              'var2;',
              'var3;',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS3%',
              'var1;',
              'var2;',
              'var3;',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[/classes > 2]]')
  models <- list(model1, model2, model3, model4, model5, model6)
  expect_equal(create_models(settings), models)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 2, use = c('var1', 'var2', 'var3', 'var4', 'var5'), categorical = 'var1',
                         censored_above = 'var2', negbin = 'var3', inflated = c('var2', 'var3')) # correlations only var4 and var5, inflated always
  model1 <- c('MODEL:',
              '%CLASS1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              '[[classes > 1]]',
              '%CLASS2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              '[[/classes > 1]]')
  model2 <- c('MODEL:',
              '%CLASS1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              '[[classes > 1]]',
              '%CLASS2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              '[[/classes > 1]]')
  model3 <- c('MODEL:',
              '%OVERALL%',
              'var4 WITH var5;',
              '%CLASS1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              '[[classes > 1]]',
              '%CLASS2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              '[[/classes > 1]]')
  model4 <- c('MODEL:',
              '%OVERALL%',
              'var4 WITH var5;',
              '%CLASS1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              '[[classes > 1]]',
              '%CLASS2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              '[[/classes > 1]]')
  model5 <- c('MODEL:',
              '%CLASS1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var4 WITH var5;',
              '[[classes > 1]]',
              '%CLASS2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var4 WITH var5;',
              '[[/classes > 1]]')
  model6 <- c('MODEL:',
              '%CLASS1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              'var4 WITH var5;',
              '[[classes > 1]]',
              '%CLASS2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              'var4 WITH var5;',
              '[[/classes > 1]]')
  models <- list(model1, model2, model3, model4, model5, model6)
  expect_equal(create_models(settings), models)
})

