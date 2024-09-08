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
                         'CATEGORICAL: var1 var2;',
                         'AUXILLIARY: var5 var6 var7 var8;',
                         'MISSING = .;',
                         'CLASSES = class ([[classes]]);')
  variable_settings <- rep(list(variable_settings), 6) %>% as.list()
  expect_equal(create_variable_specs(settings), variable_settings)

  settings <- define_lca(testdata, 'test', 'id',
                         use = c('var1', 'var2', 'var3', 'var4', 'var5', 'var6', 'var7'),
                         censored_above = c('var1', 'var2'),
                         censored_below = c('var3', 'var4'),
                         poisson = c('var5', 'var6'),
                         negbin = c('var7', 'var8'),
                         inflated = c('var2', 'var4', 'var6', 'var8'))
  variable_settings <- c('VARIABLE:',
                         'NAMES = id var1 var2 var3 var4 var5 var6 var7 var8;',
                         'IDVARIABLE = id;',
                         'USEVARIABLES =  var1 var2 var3 var4 var5 var6 var7 var8;',
                         'CENSORED: var1 (a) var2 (ai) var3 (b) var4 (bi);',
                         'COUNT: var5 (p) var6 (pi) var7 (nb) var8 (nbi);',
                         'MISSING = .;',
                         'CLASSES = class ([[classes]]);')
  variable_settings <- rep(list(variable_settings), 6) %>% as.list()
  expect_equal(create_variable_specs(settings), variable_settings)
})

# test_that("creates basemodel templates the right way", {
#   create_templates(frame = testdata,
#                    'base_lca',
#                    id = 'id',
#                    nclasses = 4,
#                    starts = list(c(320, 160, 160, 320),
#                                  c(160, 160, 160, 320),
#                                  c(160, 160, 160, 320),
#                                  c(160, 160, 160, 320),
#                                  c(160, 160, 160, 320),
#                                  c(160, 160, 160, 320)),
#                    cores = 16,
#                    categorical = c('cat1', 'cat2'),
#                    aux = c('p', 'pi'))
#
#   created_templates <- list(readLines('base_lca_model1_template.txt'),
#                             readLines('base_lca_model2_template.txt'),
#                             readLines('base_lca_model3_template.txt'),
#                             readLines('base_lca_model4_template.txt'),
#                             readLines('base_lca_model5_template.txt'),
#                             readLines('base_lca_model6_template.txt'))
#
#   # file.remove('base_lca_model1_template.txt') # clean up
#   # file.remove('base_lca_model2_template.txt')
#   # file.remove('base_lca_model3_template.txt')
#   # file.remove('base_lca_model4_template.txt')
#   # file.remove('base_lca_model5_template.txt')
#   # file.remove('base_lca_model6_template.txt')
#
#   expect_equal(basemodel_templates, created_templates)
# })


