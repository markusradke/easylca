test_that('plot_metric_profiles returns a ggplot', {
  profiles <- extract_profile_for_plotting(testresults$models[[3]][[3]], testresults$settings)
  plot <- plot_metric_profiles(profiles)
  expect_equal(class(plot), c('gg', 'ggplot'))
})


test_that('plot_binary_profiles returns a ggplot', {
  profiles <- extract_profile_for_plotting(testresults$models[[3]][[3]], testresults$settings)
  plot <- plot_binary_profiles(profiles)
  expect_equal(class(plot), c('gg', 'ggplot'))
})

test_that('plot_kruskal returns a ggplot', {
  plot <- plot_kruskal_profiles(testresults$models[[3]][[2]])
  expect_equal(class(plot), c('gg', 'ggplot'))
})
