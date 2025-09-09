test_that('indicator classes get identified correctly', {
  res <- get_profile_types(titanic_settings)
  expected <- list(continuous = c('age', 'fare'),
                   categorical = c('survived', 'isfem', 'nsibsp', 'nparchi'),
                   nominal = c('port', 'pasclass'))
  expect_equal(res, expected)

  res <- get_profile_types(random_testresults$settings22)
})



# test_that('get binary indicators returns correct indicators', {
#   temp_data <- random_testdata[3:5,]
#   settings <- define_lca(frame = temp_data,
#                          analysis_name = 'test', id_variable = 'id',
#                          categorical = 'var1',
#                          nominal = 'var2', 'var7')
#   res <- get_binary_indicators(settings)
#   expect_setequal(res, c('var1', 'var2'))
# })
#
# test_that('nominal indicators are in plot group "discrete"', {
#   profiles <- get_profiles_for_plotting(
#     model = titanic_lca_results$models$modeltype_2$titanic_model2_lca.06_titanic_model2_lca.out,
#     settings = titanic_settings)
#   plot_group_nominals <- profiles %>%
#     dplyr::filter(.data$item %in% c('port', 'pasclass')) %>%
#     dplyr::distinct(.data$plotgroup)
#   expect_setequal(plot_group_nominals, 'discrete')
# })
#
# test_that('extraction of profiles for plotting retrieves correct variables', {
#   test_for_columnnames_completeness <- function(model, settings){
#     profile_variables <- c('param', 'item', 'est', 'se', 'est_se', 'pval', 'segment', 'level', 'count',
#                            'significance', 'upper', 'lower', 'pzero', 'yposinflation', 'plotgroup')
#     res <- get_profiles_for_plotting(model, settings)
#     expect_setequal(res %>% colnames(), profile_variables)
#   }
#
#   model <- titanic_lca_results$models[[2]][[5]]
#   settings <- titanic_lca_results$settings
#   test_for_columnnames_completeness(model, settings)
#
#   model <- random_testresults$models[[1]][[2]]
#   settings <- random_testresults$settings
#   test_for_columnnames_completeness(model, settings)
# })
#
# test_that('prevalences are extracted correctly', {
#   model <- random_testresults$models$modeltype_1$test_model1_lca.3_test_model1_lca.out
#   profiles <- get_profiles_for_plotting(model, random_testresults$settings)
#
#   summed_counts <- model$class_counts$modelEstimated$count %>% round() %>% sum
#   sum_var3 <- profiles %>% dplyr::filter(item == 'var3') %>%
#     dplyr::mutate(count = as.integer(count)) %>%
#     dplyr::summarize(sum(count)) %>% dplyr::pull()
#   expect_equal(sum_var3, summed_counts)
#
#   n_unique_class1_counts <- profiles %>%
#     dplyr::filter(segment %>% stringr::str_detect('class 1')) %>%
#     dplyr::pull(count) %>%
#     unique() %>%
#     length()
#   expect_equal(n_unique_class1_counts, 1L)
# })

# test_that('error bars and means for count variables are correct', {
#   model <- random_testresults$models[[2]][[3]]
#   settings <- random_testresults$settings
#   profiles <- get_profiles_for_plotting(model, settings)
#
#   means <- get_means_from_profiles(profiles)
#   errors <- get_errors_from_profiles(profiles, means)
#
#   countmeans <- correct_negbin_poisson_means(means, settings)
#   expect_equal(dplyr::filter(countmeans, item =='var7') %>% dplyr::pull(est),
#                dplyr::filter(means, item == 'var7') %>% dplyr::pull(est) %>% exp)
#
#   counterorrs <- correct_negbin_poisson_errors(countmeans, errors, profiles, settings)
#
#   poisson_mean <- dplyr::filter(means, item == 'var8') %>%
#     dplyr::mutate(est = ifelse(est < 0, 0, est)) %>%
#     dplyr::pull(est)
#   expect_equal(dplyr::filter(counterorrs, item == 'var8') %>% dplyr::pull(upper), poisson_mean + sqrt(poisson_mean))
#   expect_equal(dplyr::filter(counterorrs, item == 'var8') %>% dplyr::pull(lower), poisson_mean - sqrt(poisson_mean))
#
#   meanvar7 <- dplyr::filter(means, item == 'var7') %>% dplyr::pull(est) %>% exp
#   divvar7 <- dplyr::filter(profiles, param == 'Dispersion' & item == 'var7') %>% dplyr::pull(est)
#
#   expect_equal(dplyr::filter(counterorrs, item == 'var7') %>% dplyr::pull(upper),
#                meanvar7 + sqrt(meanvar7 + exp(divvar7) * meanvar7**2))
#   expect_equal(dplyr::filter(counterorrs, item == 'var7') %>% dplyr::pull(lower),
#                meanvar7 - sqrt(meanvar7 + exp(divvar7) * meanvar7**2))
# })
