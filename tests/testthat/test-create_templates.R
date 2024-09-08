test_that('creates necessary files', {
  settings <- define_lca(testdata, 'test', 'id')
  create_templates(settings)
  currenttime <- format(Sys.time(), "%Y%m%d_%H-%M")
  analysis_name <- paste('test', currenttime, sep = '_')

  for(i in seq(6)){
    model_path <- paste0(analysis_name, '/', analysis_name, '_model_', i, '_template.txt')
    print(model_path)
    expect_true(file.exists(model_path),
                info = paste0('Did not write file for model ', i, '...'))
  }
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


