test_that("creates basemodel templates the right way", {
  create_templates(frame = testdata,
                   'base_lca',
                   nclasses = 4,
                   starts = c(160, 160, 160, 320),
                   cores = 16,
                   categoricals = c('cat1', 'cat2'),
                   aux = c('p', 'pi'))

  created_templates <- list(readLines('base_lca_model1_template.txt'),
                            readLines('base_lca_model2_template.txt'),
                            readLines('base_lca_model3_template.txt'),
                            readLines('base_lca_model4_template.txt'),
                            readLines('base_lca_model5_template.txt'),
                            readLines('base_lca_model6_template.txt'))

  file.remove('base_lca_model1_template.txt') # clean up
  file.remove('base_lca_model2_template.txt')
  file.remove('base_lca_model3_template.txt')
  file.remove('base_lca_model4_template.txt')
  file.remove('base_lca_model5_template.txt')
  file.remove('base_lca_model6_template.txt')

  expect_equal(basemodel_templates, created_templates)
})


