test_that("seed retrieval for best solutions for model", {
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
  test_that("writes necessary input files, runs them correctly and reads out the values", {
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
    expect_true(file.exists(sprintf('%s/01_seed_%s.inp', folder, seeds[1])))
    expect_true(file.exists(sprintf('%s/02_seed_%s.inp', folder, seeds[2])))

    expected_inp_seed1 <- c(
      '',
      'TITLE: test2 classes',
      'DATA: FILE IS modeltype_1.dat;',
      '',
      'VARIABLE:',
      'NAMES = id survived pasclass age fare nsibsp nparchi isfem port;',
      'IDVARIABLE = id;',
      'USEVARIABLES = pasclass survived;',
      'NOMINAL = pasclass;',
      'CATEGORICAL = survived;',
      'AUXILIARY = age fare nsibsp nparchi isfem port;',
      'MISSING = .;',
      'CLASSES = class (2);',
      '',
      '',
      'ANALYSIS:',
      sprintf('OPTSEED = %s;', seeds[1]),
      'TYPE = MIXTURE;',
      'PROCESSORS = 16;',
      'STITERATIONS = 2;',
      'STARTS = 10 2;',
      '',
      'OUTPUT:',
      'SVALUES ENTROPY TECH1 TECH4 TECH10;',
      'SAVEDATA:',
      'FILE IS 02_classes.dat;',
      'SAVE = cprobabilities bchweights loglikelihood;'
    )
    actual <- readLines(sprintf('%s/01_seed_%s.inp', folder, seeds[1]))
    expect_equal(actual, expected_inp_seed1)

    replication_models <- run_replication_models(settings, 1, 2)
    expect_true(file.exists(sprintf('%s/01_seed_%s.out', folder, seeds[1])))
    expect_true(file.exists(sprintf('%s/02_seed_%s.out', folder, seeds[2])))
    expect_equal(length(replication_models), 2)
    expect_equal(ncol(replication_models[[1]]$savedata), 15)
    expect_true('CPROB1' %in% colnames(replication_models[[1]]$savedata))
    expect_true('CPROB2' %in% colnames(replication_models[[1]]$savedata))
    expect_true('ID' %in% colnames(replication_models[[1]]$savedata))
    expect_true('OUTLOGL' %in% colnames(replication_models[[1]]$savedata))
    unlink(settings$folder, recursive = TRUE)

    case_statistics <- get_case_cprob_and_ll(replication_models, settings)
    expect_true(methods::is(case_statistics, 'data.frame'))
    expect_equal(nrow(case_statistics), 2 * nrow(titanic_passengers))
    expect_setequal(
      colnames(case_statistics),
      c('id', 'top_n', 'cprob1', 'cprob2', 'll_case', 'll_model')
    )
  })
}

test_that('get replication check works', {
  settings <- define_lca(
    titanic_passengers,
    'test',
    'id',
    nclasses = 2,
    use = c('pasclass', 'survived'),
    categorical = 'survived',
    nominal = 'pasclass',
    starts = 15
  )
  lca <- perform_lca(settings, 1)
  testmodel <- lca$models[[1]][[2]]
  expect_null(suppressWarnings(get_stability_check(
    testmodel,
    settings,
    1,
    5
  )))
  browser()
  res <- get_stability_check(testmodel, settings, 1, 3)
  expect_true(methods::is(res, 'list'))
  expect_setequal(
    names(list),
    c(
      'ntop',
      'max_diff_BIC',
      'test_param_stability',
      'sd_param_stability',
      'test_cprob_stability',
      'sd_cprob_stability'
    )
  )
})
