test_that('extraction of profiles works', {
  model <- testresults$models[[2]][[3]]
  settings <- testresults$settings
  profile_variables <- c('param', 'item', 'est', 'se', 'est_se', 'pval', 'segment', 'level', 'count')
  expect_setequal(extract_profile(model, settings) %>% colnames(), profile_variables)

  profile_plot_variables <- c(profile_variables, 'upper', 'lower', 'pzero', 'yposinflation')
  expect_setequal(extract_profile_for_plotting(model, settings) %>% colnames(), profile_plot_variables)


  params <- c('Probabilities', 'Means')
  expect_setequal(extract_profile_for_plotting(model, settings) %>% dplyr::pull('param') %>% unique(),
                  params)
})

test_that('error bars and means for count variabels are correct', {
  model <- testresults$models[[2]][[3]]
  settings <- testresults$settings
  profiles <- extract_profile(model, settings)
  means <- get_means_from_profiles(profiles)
  errors <- get_errors_from_profiles(profiles, means)

  countmeans <- correct_negbin_poisson_means(means, settings)
  expect_equal(dplyr::filter(countmeans, item =='var7') %>% dplyr::pull(est),
               dplyr::filter(means, item == 'var7') %>% dplyr::pull(est) %>% exp)

  counterorrs <- correct_negbin_poisson_errors(countmeans, errors, profiles, settings)

  poisson_mean <- dplyr::filter(means, item == 'var8') %>%
    dplyr::mutate(est = ifelse(est < 0, 0, est)) %>%
    dplyr::pull(est)
  expect_equal(dplyr::filter(counterorrs, item == 'var8') %>% dplyr::pull(upper), poisson_mean + sqrt(poisson_mean))
  expect_equal(dplyr::filter(counterorrs, item == 'var8') %>% dplyr::pull(lower), poisson_mean - sqrt(poisson_mean))

  meanvar7 <- dplyr::filter(means, item == 'var7') %>% dplyr::pull(est) %>% exp
  divvar7 <- dplyr::filter(profiles, param == 'Dispersion' & item == 'var7') %>% dplyr::pull(est)

  expect_equal(dplyr::filter(counterorrs, item == 'var7') %>% dplyr::pull(upper),
               meanvar7 + sqrt(meanvar7 + exp(divvar7) * meanvar7**2))
  expect_equal(dplyr::filter(counterorrs, item == 'var7') %>% dplyr::pull(lower),
               meanvar7 - sqrt(meanvar7 + exp(divvar7) * meanvar7**2))
})


test_that('prevalences are extracted correctly', {
  model <- testresults$models$modeltype_1$test_model1_lca.3_test_model1_lca.out
  profiles <- extract_profile_for_plotting(model, testresults$settings)
  summed_counts <- model$class_counts$modelEstimated$count %>% round() %>% sum
  sum_var3 <- profiles %>% dplyr::filter(item == 'var3') %>%
    dplyr::mutate(count = as.integer(count)) %>%
    dplyr::summarize(sum(count)) %>% dplyr::pull()
  expect_equal(sum_var3, summed_counts)

  n_unique_class1_counts <- profiles %>%
    dplyr::filter(segment %>% stringr::str_detect('class 1')) %>%
    dplyr::pull(count) %>%
    unique() %>%
    length()
  expect_equal(n_unique_class1_counts, 1L)
})
