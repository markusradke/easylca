test_that('extraction of profiles works', {
  model <- testresults$models[[2]][[3]]
  settings <- testresults$settings
  profile_variables <- c('param', 'item', 'est', 'se', 'est_se', 'pval', 'segment', 'level')
  expect_setequal(extract_profile(model, settings) %>% colnames(), profile_variables)

  profile_plot_variables <- c(profile_variables, 'upper', 'lower', 'pzero', 'yposinflation')
  expect_setequal(extract_profile_for_plotting(model, settings) %>% colnames(), profile_plot_variables)


  params <- c('Probabilities', 'Means')
  expect_setequal(extract_profile_for_plotting(model, settings) %>% dplyr::pull('param') %>% unique(),
                  params)
})

test_that('error bars and means for count variabels are correct', {
  model <- testresults$models[[2]][[2]]
  settings <- testresults$settings
  profiles <- extract_profile(model, settings)
  means <- get_means_from_profiles(profiles)
  errors <- get_errors_from_profiles(profiles)

  negbinmeans <- correct_negbin_means(means, settings)
  expect_equal(dplyr::filter(negbinmeans, item =='var7') %>% dplyr::pull(est),
               dplyr::filter(means, item == 'var7') %>% dplyr::pull(est) %>% exp)

  counterorrs <- correct_negbin_poisson_errors(negbinmeans, errors, profiles, settings)

  expect_equal(dplyr::filter(counterorrs, item == 'var8') %>% dplyr::pull(upper),
               dplyr::filter(means, item == 'var8') %>% dplyr::pull(est) + dplyr::filter(means, item == 'var8') %>% dplyr::pull(est) %>% sqrt)
  expect_equal(dplyr::filter(counterorrs, item == 'var8') %>% dplyr::pull(lower),
               dplyr::filter(means, item == 'var8') %>% dplyr::pull(est) - dplyr::filter(means, item == 'var8') %>% dplyr::pull(est) %>% sqrt)

  meanvar7 <- dplyr::filter(means, item == 'var7') %>% dplyr::pull(est) %>% exp
  divvar7 <- dplyr::filter(profiles, param == 'Dispersion' & item == 'var7') %>% dplyr::pull(est)

  expect_equal(dplyr::filter(counterorrs, item == 'var7') %>% dplyr::pull(upper),
               meanvar7 + sqrt(meanvar7 + exp(divvar7) * meanvar7**2))
  expect_equal(dplyr::filter(counterorrs, item == 'var7') %>% dplyr::pull(lower),
               meanvar7 - sqrt(meanvar7 + exp(divvar7) * meanvar7**2))
})
