test_that("test seed retrieval for best solutions for model", {
  model <- titanic_lca_results$models$modeltype_1$modeltype_1.06_classes.out
  expected <- c('565819', '752769', '520177', '30098', '957392')
  expect_equal(get_top_n_solution_seeds(model, 5), expected)

  model <- random_testresults$models$modeltype_6$modeltype_6.02_classes.out
  expect_warning(
    get_top_n_solution_seeds(model, 5),
    'For this model less than 5 runs converged. Not performing replication check. Please increase number of random starts.'
  )
  expect_warning(
    get_top_n_solution_seeds(model, 6),
    'For this model less than 6 runs converged. Not performing replication check. Please increase number of random starts.'
  )
  expect_equal(suppressWarnings(get_top_n_solution_seeds(model, 5)), NULL)
  expected <- c('415931', '68985')
  expect_equal(get_top_n_solution_seeds(model, 2), expected)
})
