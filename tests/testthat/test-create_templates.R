test_that('creates necessary files', {
  settings <- define_lca(random_testdata, 'test', 'id')
  settings$vlmrt_last_run <- FALSE
  create_templates(settings)
  for(i in seq(6)){
    model_path <- paste0(settings$folder_name, '/modeltype_',  i, '_template.txt')
    expect_true(file.exists(model_path),
                info = paste0('Did not write file for model ', i, '...'))
  }
  unlink(settings$folder_name, recursive = T)
})


test_that('headers are created the right way from settings', {
  settings <- define_lca(random_testdata, 'test', 'id')
  headers <- list()
  for(i in seq(6)){
    modeltype_folder <- paste0('modeltype_', i)
    model_header <- c('[[init]]',
                      'iterators = classes;',
                      'classes = 1:4;',
                      'classname#classes = 01 02 03 04;',
                      paste0('filename = \"[[classname#classes]]_classes.inp\";'),
                      paste0('outputDirectory = \"', modeltype_folder, '\";'),
                      paste0('[[/init]]'),
                      '',
                      paste0('TITLE: test[[classes]] classes'),
                      paste0('DATA: FILE IS ', modeltype_folder, '.dat;'))
    headers <- c(headers, list(model_header))
  }
  expect_equal(create_headers(settings), headers)
})

test_that('variables specs are created the right way from settings',{
  settings <- define_lca(random_testdata, 'test', 'id', use = c('var1', 'var2', 'var3', 'var4'), categorical = c('var1', 'var2'))
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

  data <- random_testdata
  data[1:10, 'var1'] <- 3
  settings <- define_lca(data, 'test', 'id', use = c('var1', 'var2', 'var3', 'var4'),
                         nominal = 'var1',
                         categorical = 'var2')
  variable_settings <- c('VARIABLE:',
                         'NAMES = id var1 var2 var3 var4 var5 var6 var7 var8;',
                         'IDVARIABLE = id;',
                         'USEVARIABLES = var1 var2 var3 var4;',
                         'NOMINAL = var1;',
                         'CATEGORICAL = var2;',
                         'AUXILIARY = var5 var6 var7 var8;',
                         'MISSING = .;',
                         'CLASSES = class ([[classes]]);')
  variable_settings <- rep(list(variable_settings), 6) %>% as.list()
  expect_equal(create_variable_specs(settings), variable_settings)

  settings <- define_lca(random_testdata %>%
                           dplyr::mutate(var5 = floor(var5),
                                         var6 = floor(var6)),
                         'test', 'id',
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

  settings <- define_lca(random_testdata, 'test', 'id',
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

  settings <- define_lca(random_testdata_weights, 'test', 'id',
                         weight_variable = 'weights')
  variable_settings <- c('VARIABLE:',
                         'NAMES = id var1 var2 var3 var4 var5 var6 var7 var8 weights;',
                         'IDVARIABLE = id;',
                         'USEVARIABLES = var1 var2 var3 var4 var5 var6 var7 var8;',
                         'WEIGHT IS weights;',
                         'MISSING = .;',
                         'CLASSES = class ([[classes]]);')
  variable_settings <- rep(list(variable_settings), 6) %>% as.list()
  expect_equal(create_variable_specs(settings), variable_settings)
})


test_that('create plot save is working correctly', {
  settings <- define_lca(random_testdata, 'test', 'id')
  settings$vlmrt_last_run <- FALSE

  plot_save <- list()
  for(i in seq(6)){
    modeltype_folder <- paste0('modeltype_', i)
    model_plot_save <- c('OUTPUT:',
                         'SVALUES ENTROPY TECH1 TECH4 TECH10;',
                         'PLOT: TYPE=PLOT1 PLOT2 PLOT3;',
                         'SAVEDATA:',
                         paste0('FILE IS [[classname#classes]]_classes.dat'),
                         'SAVE = cprobabilites bchweights;')
    plot_save <- c(plot_save, list(model_plot_save))
  }
  expect_equal(create_plot_save(settings), plot_save)
})


test_that('create analysis is working correctly', {
  settings <- define_lca(random_testdata, 'test', 'id', nclasses = 3, cores = 64, starts = 160L)
  analysis_all_types <- c('ANALYSIS:',
                          'TYPE = MIXTURE;',
                          'PROCESSORS = 64;',
                          '[[classes = 1]]',
                          'STITERATIONS = 32;',
                          'STARTS = 160 32;',
                          '[[/classes = 1]]',
                          '[[classes = 2]]',
                          'STITERATIONS = 32;',
                          'STARTS = 160 32;',
                          '[[/classes = 2]]',
                          '[[classes = 3]]',
                          'STITERATIONS = 32;',
                          'STARTS = 160 32;',
                          '[[/classes = 3]]')
  analysis <- list(analysis_all_types, analysis_all_types, analysis_all_types,
                   analysis_all_types, analysis_all_types, analysis_all_types)
  expect_equal(create_analysis(settings), analysis)

  settings <- define_lca(random_testdata, 'test', 'id', nclasses = 2, cores = 64, starts = list(c(160, 320),
                                                                                         c(160, 320),
                                                                                         c(160, 320),
                                                                                         c(320, 640),
                                                                                         c(320, 640),
                                                                                         c(320, 640)))
  analysis <- list(c('ANALYSIS:',
                     'TYPE = MIXTURE;',
                     'PROCESSORS = 64;',
                     '[[classes = 1]]',
                     'STITERATIONS = 32;',
                     'STARTS = 160 32;',
                     '[[/classes = 1]]',
                     '[[classes = 2]]',
                     'STITERATIONS = 64;',
                     'STARTS = 320 64;',
                     '[[/classes = 2]]'),
                   c('ANALYSIS:',
                     'TYPE = MIXTURE;',
                     'PROCESSORS = 64;',
                     '[[classes = 1]]',
                     'STITERATIONS = 32;',
                     'STARTS = 160 32;',
                     '[[/classes = 1]]',
                     '[[classes = 2]]',
                     'STITERATIONS = 64;',
                     'STARTS = 320 64;',
                     '[[/classes = 2]]'),
                   c('ANALYSIS:',
                     'TYPE = MIXTURE;',
                     'PROCESSORS = 64;',
                     '[[classes = 1]]',
                     'STITERATIONS = 32;',
                     'STARTS = 160 32;',
                     '[[/classes = 1]]',
                     '[[classes = 2]]',
                     'STITERATIONS = 64;',
                     'STARTS = 320 64;',
                     '[[/classes = 2]]'),
                   c('ANALYSIS:',
                     'TYPE = MIXTURE;',
                     'PROCESSORS = 64;',
                     '[[classes = 1]]',
                     'STITERATIONS = 64;',
                     'STARTS = 320 64;',
                     '[[/classes = 1]]',
                     '[[classes = 2]]',
                     'STITERATIONS = 128;',
                     'STARTS = 640 128;',
                     '[[/classes = 2]]'),
                   c('ANALYSIS:',
                     'TYPE = MIXTURE;',
                     'PROCESSORS = 64;',
                     '[[classes = 1]]',
                     'STITERATIONS = 64;',
                     'STARTS = 320 64;',
                     '[[/classes = 1]]',
                     '[[classes = 2]]',
                     'STITERATIONS = 128;',
                     'STARTS = 640 128;',
                     '[[/classes = 2]]'),
                   c('ANALYSIS:',
                     'TYPE = MIXTURE;',
                     'PROCESSORS = 64;',
                     '[[classes = 1]]',
                     'STITERATIONS = 64;',
                     'STARTS = 320 64;',
                     '[[/classes = 1]]',
                     '[[classes = 2]]',
                     'STITERATIONS = 128;',
                     'STARTS = 640 128;',
                     '[[/classes = 2]]'))
  expect_equal(create_analysis(settings), analysis)
})

test_that('LMRLRT is added on wish', {
  settings <- define_lca(random_testdata, 'test', 'id')
  settings$vlmrt_last_run <- TRUE
  plot_save <- list()
  for(i in seq(6)){
    modeltype_folder <- paste0('modeltype_',  i)
    model_plot_save <- c('OUTPUT:',
                         'SVALUES ENTROPY TECH1 TECH4 TECH10 TECH11;',
                         'PLOT: TYPE=PLOT1 PLOT2 PLOT3;',
                         'SAVEDATA:',
                         paste0('FILE IS [[classname#classes]]_classes.dat'),
                         'SAVE = cprobabilites bchweights;')
    plot_save <- c(plot_save, list(model_plot_save))
  }

  expect_equal(create_plot_save(settings), plot_save)
})
