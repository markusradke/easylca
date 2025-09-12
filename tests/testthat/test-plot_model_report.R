test_that('plot_metric_profiles returns a ggplot', {
  profiles <- get_profiles_for_plotting(random_testresults$models[[3]][[3]], random_testresults$settings)
  plot <- plot_continuous_profiles(profiles)
  expect_true(methods::is(plot, 'ggplot'))
})


test_that('plot_binary_profiles returns a ggplot', {
  profiles <- get_profiles_for_plotting(random_testresults$models[[3]][[3]], random_testresults$settings)
  plot <- plot_binary_profiles(profiles)
  expect_true(methods::is(plot, 'ggplot'))
})

test_that('plot_kruskal returns a ggplot', {
  plot <- plot_kruskal_profiles(random_testresults$models[[3]][[2]])
  expect_true(methods::is(plot, 'ggplot'))
})


test_that('create_mean_assignement_probabilities_table returns flextable', {
  table <- create_mean_assignement_probabilities_table(random_testresults$models[[1]][[2]])
  expect_true(methods::is(table, 'flextable'))
})
