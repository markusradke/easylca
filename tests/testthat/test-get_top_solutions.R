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

if (is_mplus_installed()) {
  test_that("test writes necessary input files", {
    settings <- define_lca(
      titanic_passengers,
      'test',
      'id',
      nclasses = 2,
      use = c('pasclass', 'survived'),
      categorical = 'survived',
      nominal = 'pasclass',
      starts = 10
    )
    lca <- perform_lca(settings, 1)
    folder <- sprintf(
      '%s/modeltype_1/check_replication_02_classes',
      settings$folder_name
    )
    seeds <- get_top_n_solution_seeds(lca$models[[1]][[2]], n = 2)
    create_input_files_for_replication(
      settings,
      modeltype = 1,
      nclasses = 2,
      seeds
    )
    expect_true(file.exists(sprintf('%s/modeltype_1.dat', folder)))
    expect_true(file.exists(sprintf('%s/01_seed_%s', folder, seeds[1])))
    expect_true(file.exists(sprintf('%s/02_seed_%s', folder, seeds[2])))

    expected_analysis_seed1 <- c(
      'TITLE: test2 classes',
      'DATA: FILE IS modeltype_1.dat;',
      '',
      'VARIABLE:',
      'NAMES = id survived pasclass age fare nsibsp nparchi isfem port;',
      'IDVARIABLE = id;',
      'USEVARIABLES = pasclass survived;',
      'NOMINAL = pasclass;',
      'CATEGORICAL = survived',
      'AUXILIARY = age fare nsibsp nparchi isfem port;',
      'MISSING = .;',
      'CLASSES = class (2);',
      '',
      '',
      'ANALYSIS:',
      sprintf('OPTSEED = %s;', seeds[1]),
      'TYPE = MIXTURE;',
      'PROCESSORS = 16;',
      'STITERATIONS = 1;',
      'STARTS = 5 1;',
      '',
      'OUTPUT:',
      'SVALUES ENTROPY TECH1 TECH4 TECH10;',
      'SAVEDATA:',
      'FILE IS 02_classes.dat;',
      'SAVE = cprobabilities bchweights loglikelihood;'
    )
    actual <- readLines(sprintf('%s/01_seed_%s', folder, seeds[1]))
    expect_equal(actual, expected)
    unlink(settings$folder, recursive = TRUE)
  })
}
