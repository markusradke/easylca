test_that('extraction of profiles works', {
  model <- testresults$models[[2]][[2]]
  settings <- testresults$settings
  profile_variables <- c('param', 'item', 'est', 'se', 'est_se', 'pval', 'segment', 'level')
  expect_setequal(extract_profile(model, settings) %>% colnames(), profile_variables)

  profile_plot_variables <- c(profile_variables, 'sd', 'upper', 'lower', 'pzero', 'yposinflation')
  expect_setequal(extract_profile_for_plotting(model, settings) %>% colnames(), profile_variables)

  params <- c('Probability', 'Means')
  expect_setequal(extract_profile_for_plotting(model, settings) %>% dplyr::pull('param') %>% unique(),
                  params)
})

