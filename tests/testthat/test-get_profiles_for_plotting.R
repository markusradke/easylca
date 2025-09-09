test_that('indicator classes get identified correctly', {
  res <- get_profile_types(titanic_settings)
  expected <- list(continuous = c('age', 'fare'),
                   categorical = c('survived', 'isfem', 'nsibsp', 'nparchi'),
                   binary = c('survived', 'isfem'),
                   nominal = c('port', 'pasclass'),
                   negbin = character(),
                   poisson = character())
  expect_equal(res, expected)

  res <- get_profile_types(random_testresults$settings)
  expected <- list(continuous = c('var3', 'var4', 'var5', 'var6', 'var7', 'var8'),
                   categorical = c('var1'),
                   binary = c('var1'),
                   nominal = character(),
                   negbin = c('var7'),
                   poisson = c('var8'))
  expect_equal(res, expected)
})

test_that('get binary indicators returns correct indicators', {
  temp_data <- random_testdata[3:5,]
  settings <- define_lca(frame = temp_data,
                         analysis_name = 'test', id_variable = 'id',
                         categorical = 'var1',
                         nominal = 'var2', 'var7')
  res <- get_binary_indicators(settings)
  expect_setequal(res, c('var1', 'var2'))
})

test_that('profile retrieval for categorical variables works', {
  res <- get_categorical_profiles(titanic_lca_results$models[[1]][[2]],
                                  c('survived', 'nsibsp', 'nparchi', 'isfem'),
                                  c('survived', 'isfem'))
  expect_equal(nrow(res), 20)
  expect_setequal(colnames(res), c('item', 'param', 'est', 'se', 'est_se', 'pval',
                                   'level', 'class', 'significance', 'plotgroup'))
  expect_setequal(dplyr::distinct(res, class) %>% dplyr::pull(class), c(1,2))
  expect_setequal(dplyr::distinct(res, level) %>% dplyr::pull(level), c(1,2,3))
  expect_true(all(res$est <= 1) & all(res$est >= 0))
  expect_true(all(res$param == 'probability'))
  expect_setequal(dplyr::distinct(res, significance) %>% dplyr::pull(significance), c('*', '**', '***'))
  expect_setequal(dplyr::distinct(res, plotgroup) %>% dplyr::pull(plotgroup), c('binary', 'discrete'))
})

test_that('profile retrieval for nominal variables works', {
  res <- get_nominal_profiles(titanic_lca_results$models[[3]][[4]],
                              c('pasclass', 'port'))
  expect_equal(nrow(res), 4 * 2 * 3)
  expect_setequal(colnames(res), c('item', 'param', 'est', 'pval',
                                   'level', 'class', 'significance', 'plotgroup'))
  expect_setequal(dplyr::distinct(res, class) %>% dplyr::pull(class), c(1,2,3,4))
  expect_setequal(dplyr::distinct(res, level) %>% dplyr::pull(level), c(1,2,3))
  expect_true(all(res$est <= 1) & all(res$est >= 0))
  expect_true(all(res$param == 'probability'))
  expect_true(all(res %>% dplyr::group_by(.data$item, .data$class) %>% # probabilites sum to 1
                    dplyr::summarize(sum_prob = sum(est)) %>%
                    dplyr::pull(sum_prob) == 1))
  expect_setequal(dplyr::distinct(res, significance) %>% dplyr::pull(significance), c('*', '***', '', ' (ref)'))
  expect_setequal(dplyr::distinct(res, plotgroup) %>% dplyr::pull(plotgroup), c('discrete'))
})

test_that('profile retrieval for continuous variables works', {
  res <- get_continuous_profiles(random_testresults$models[[4]][[2]],
                                 get_profile_types(random_testresults$settings))
  expect_equal(nrow(res), 12)
  expect_setequal(colnames(res), c('item', 'param', 'est', 'se', 'est_se', 'pval',
                                   'significance', 'class', 'plotgroup', 'upper', 'lower',
                                   'pzero', 'yposinflation'))
  expect_setequal(dplyr::distinct(res, class) %>% dplyr::pull(class), c(1,2))
  expect_setequal(dplyr::distinct(res, plotgroup) %>% dplyr::pull(plotgroup), c('continuous'))
  expect_setequal(dplyr::distinct(res, significance) %>% dplyr::pull(significance), c('***'))
})

test_that('extraction of profiles for plotting retrieves correct variables', {
  test_for_columnnames_completeness <- function(model, settings){
    profile_variables <- c('param', 'item', 'est', 'se', 'est_se', 'pval', 'class', 'level', 'count',
                           'significance', 'upper', 'lower', 'pzero', 'yposinflation', 'plotgroup')
    res <- get_profiles_for_plotting(model, settings)
    expect_setequal(res %>% colnames(), profile_variables)
  }

  model <- titanic_lca_results$models[[2]][[5]]
  settings <- titanic_lca_results$settings
  test_for_columnnames_completeness(model, settings)

  model <- random_testresults$models[[1]][[2]]
  settings <- random_testresults$settings
  test_for_columnnames_completeness(model, settings)

  model <- titanic_lca_results$models[[1]][[4]]
  settings <- titanic_lca_results$settings
  test_for_columnnames_completeness(model, settings)
})

test_that('prevalences are extracted correctly', {
  model <- random_testresults$models[[1]][[3]]
  profiles <- get_profiles_for_plotting(model, random_testresults$settings)

  expect_setequal(unique(profiles$item),
                  c('var1', 'var3', 'var4', 'var5', 'var6',
                    'var7\n(negbin)', 'var8\n(poisson)'))

  summed_counts <- model$class_counts$modelEstimated$count %>% round() %>% sum
  sum_var3 <- profiles %>% dplyr::filter(item == 'var3') %>%
    dplyr::summarize(sum(count)) %>% dplyr::pull()
  expect_equal(sum_var3, summed_counts)

  n_unique_class1_counts <- profiles %>%
    dplyr::filter(class %>% stringr::str_detect('class 1')) %>%
    dplyr::pull(count) %>%
    unique() %>%
    length()
  expect_equal(n_unique_class1_counts, 1L)
})
