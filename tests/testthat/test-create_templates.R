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
  settings <- define_lca(testdata, 'test', 'id', use = c('cat1', 'cat2', 'nd', 'ndc'), categorical = c('cat1', 'cat2'))
  variable_settings <- c('VARIABLE:',
                         'NAMES = id cat1 cat2 nd ndc ndi ndci p pi;',
                         'IDVARIABLE = id;',
                         'USEVARIABLES = cat1 cat2 nd ndc;',
                         'CATEGORICAL: cat1 cat;',
                         'AUXILLIARY: ndi ndci p pi;',
                         'MISSING = .;',
                         'CLASSES = class ([[classes]]);')
  variable_settings <- rep(list(variable_settings), 6) %>% as.list()
  expect_equal(create_variable_specs(settings), variable_settings)

  settings <- define_lca(testdata, 'test', 'id',
                         use = c('cat1', 'nd', 'ndi', 'ndc', 'ndci', 'p', 'pi'),
                         categorical = 'cat1',
                         poisson = c('p', 'pi'),
                         negbin = c('ndci'),
                         censored = c('ndc', 'ndi', 'ndci'),
                         inflated = c('ndi', 'ndci', 'pi', 'ndci'))
  variable_settings <- c('VARIABLE:',
                         'NAMES = id cat1 cat2 nd ndc ndi ndci p pi;',
                         'IDVARIABLE = id;',
                         'USEVARIABLES = cat1 nd ndi ndc ndci p pi;',
                         'CATEGORICAL: cat1;',
                         'CENSORED: ndc ndi(i) ndci(i);',
                         'COUNT: p pi(i) ndci(nbi);',
                         'AUXILLIARY: ndi;',
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


