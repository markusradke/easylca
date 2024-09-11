test_that('inflation block is created correctly',{
  settings <- define_lca(testdata, 'test', 'id', censored_above = c('var1', 'var2'), inflated = c('var1', 'var2'))
  inflation_block <- c('[ var1#1 ];',
                       '[ var2#1 ];')
  expect_equal(create_inflation_block(settings), inflation_block)

  settings <- define_lca(testdata, 'test', 'id')
  expect_equal(create_inflation_block(settings), character())
})


test_that('free variance block is created correctly', {
  settings <- define_lca(testdata, 'test', 'id', use = c('var1', 'var2', 'var3'), poisson = 'var3')
  freevariance_block <- c('var1;',
                          'var2;')
  expect_equal(create_freevariance_block(settings), freevariance_block)

  settings <- define_lca(testdata, 'test', 'id', use = c('var1', 'var2'), categorical = c('var1', 'var2'))
  expect_equal(create_freevariance_block(settings), character())
})

test_that('correlation block is created correctly', {
  settings <- define_lca(testdata, 'test', 'id', use = c('var1', 'var2', 'var3'))
  correlation_block <- c('var1 WITH var2;',
                         'var1 WITH var3;',
                         'var2 WITH var3;')
  expect_equal(create_correlation_block(settings), correlation_block)

  settings <- define_lca(testdata, 'test', 'id', use = c('var1', 'var2', 'var3', 'var4'),
                         censored_above = 'var1', censored_below = 'var2', poisson = 'var3', negbin = 'var4')
  expect_equal(create_correlation_block(settings), character())
})

test_that('model 1 is created correctly', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'),
                         censored_above = c('var1', 'var2'), inflated = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  model1 <- c('MODEL:',
            '%CLASS#1%',
            '[ var1#1 ];',
            '[ var2#1 ];',
            '[[classes > 1]]',
            '%CLASS#2%',
            '[ var1#1 ];',
            '[ var2#1 ];',
            '[[/classes > 1]]',
            '[[classes > 2]]',
            '%CLASS#3%',
            '[ var1#1 ];',
            '[ var2#1 ];',
            '[[/classes > 2]]')
  expect_equal(create_model1(settings), model1)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = c('var1'))
  settings$inflation_block <- create_inflation_block(settings)
  expect_equal(create_model1(settings), character())
})

test_that('model 2 is created correctly', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  expect_equal(create_model2(settings), character())

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = 'var1',
                         poisson = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model2 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              '[[/classes > 2]]')
  expect_equal(create_model2(settings), model2)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model2 <- c('MODEL:',
              '%CLASS#1%',
              'var1;',
              'var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var1;',
              'var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var1;',
              'var2;',
              '[[/classes > 2]]')
  expect_equal(create_model2(settings), model2)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), censored_below = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model2 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[/classes > 2]]')
  expect_equal(create_model2(settings), model2)
})

test_that('model 3 is created correctly', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  expect_equal(create_model3(settings), character())

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  model3 <- c('MODEL:',
              '%OVERALL%',
              'var1 WITH var2;')
  expect_equal(create_model3(settings), model3)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'),
                         censored_above = c('var1', 'var2'), inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  model3 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              '[[/classes > 2]]')
  expect_equal(create_model3(settings), model3)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2', 'var3'),
                         censored_above = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  model3 <- c('MODEL:',
              '%OVERALL%',
              'var1 WITH var3;',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              '[[/classes > 2]]')
  expect_equal(create_model3(settings), model3)
})

test_that('model 4 is created correctly', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  expect_equal(create_model4(settings), character())

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = c('var1'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model4 <- c('MODEL:',
              '%CLASS#1%',
              'var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var2;',
              '[[/classes > 2]]')
  expect_equal(create_model4(settings), model4)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = 'var1',
                         poisson = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model4 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              '[[/classes > 2]]')
  expect_equal(create_model4(settings), model4)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model4 <- c('MODEL:',
              '%OVERALL%',
              'var1 WITH var2;',
              '%CLASS#1%',
              'var1;',
              'var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var1;',
              'var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var1;',
              'var2;',
              '[[/classes > 2]]')
  expect_equal(create_model4(settings), model4)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'),
                         censored_below = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model4 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[/classes > 2]]')
  expect_equal(create_model4(settings), model4)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2', 'var3'),
                         censored_below = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model4 <- c('MODEL:',
              '%OVERALL%',
              'var1 WITH var3;',
              '%CLASS#1%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              'var3;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 2]]')
  expect_equal(create_model4(settings), model4)
})

test_that('model 5 is created correctly', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  expect_equal(create_model5(settings), character())

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  model5 <- c('MODEL:',
              '%CLASS#1%',
              'var1 WITH var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var1 WITH var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var1 WITH var2;',
              '[[/classes > 2]]')
  expect_equal(create_model5(settings), model5)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'),
                         censored_above = c('var1', 'var2'), inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  model5 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              '[[/classes > 2]]')
  expect_equal(create_model5(settings), model5)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2', 'var3'),
                         censored_above = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  model5 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              'var1 WITH var3;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              'var1 WITH var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              'var1 WITH var3;',
              '[[/classes > 2]]')
  expect_equal(create_model5(settings), model5)
})

test_that('model 6 is created correctly', {
  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  expect_equal(create_model6(settings), character())

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = c('var1'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model6 <- c('MODEL:',
              '%CLASS#1%',
              'var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var2;',
              '[[/classes > 2]]')
  expect_equal(create_model6(settings), model6)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'), categorical = 'var1',
                         poisson = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model6 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              '[[/classes > 2]]')
  expect_equal(create_model6(settings), model6)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'))
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model6 <- c('MODEL:',
              '%CLASS#1%',
              'var1 WITH var2;',
              'var1;',
              'var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var1 WITH var2;',
              'var1;',
              'var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var1 WITH var2;',
              'var1;',
              'var2;',
              '[[/classes > 2]]')
  expect_equal(create_model6(settings), model6)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2'),
                         censored_below = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model6 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              'var1;',
              'var2;',
              '[[/classes > 2]]')
  expect_equal(create_model6(settings), model6)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2', 'var3'),
                         censored_below = 'var2', inflated = 'var2')
  settings$inflation_block <- create_inflation_block(settings)
  settings$correlation_block <- create_correlation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  model6 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              'var1 WITH var3;',
              'var1;',
              'var2;',
              'var3;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              'var1 WITH var3;',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              '[ var2#1 ];',
              'var1 WITH var3;',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 2]]')
  expect_equal(create_model6(settings), model6)
})


test_that('model specifications are created correctly or all model types',{
  settings <- define_lca(testdata, 'test', 'id', nclasses = 3, use = c('var1', 'var2', 'var3'))
  model1 <- character()
  model2 <- c('MODEL:',
              '%CLASS#1%',
              'var1;',
              'var2;',
              'var3;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 2]]')
  model3 <- c('MODEL:',
              '%OVERALL%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;')
  model4 <- c('MODEL:',
              '%OVERALL%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '%CLASS#1%',
              'var1;',
              'var2;',
              'var3;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 2]]')
  model5 <- c('MODEL:',
              '%CLASS#1%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              '[[/classes > 2]]')
  model6 <- c('MODEL:',
              '%CLASS#1%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              'var1;',
              'var2;',
              'var3;',
              '[[classes > 1]]',
              '%CLASS#2%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 1]]',
              '[[classes > 2]]',
              '%CLASS#3%',
              'var1 WITH var2;',
              'var1 WITH var3;',
              'var2 WITH var3;',
              'var1;',
              'var2;',
              'var3;',
              '[[/classes > 2]]')
  models <- list(model1, model2, model3, model4, model5, model6)
  expect_equal(create_models(settings), models)

  settings <- define_lca(testdata, 'test', 'id', nclasses = 2, use = c('var1', 'var2', 'var3', 'var4', 'var5'), categorical = 'var1',
                         censored_above = 'var2', poisson = 'var3', inflated = c('var2', 'var3'))
  model1 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              '[[/classes > 1]]')
  model2 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              '[[/classes > 1]]')
  model3 <- c('MODEL:',
              '%OVERALL%',
              'var4 WITH var5;',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              '[[/classes > 1]]')
  model4 <- c('MODEL:',
              '%OVERALL%',
              'var4 WITH var5;',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var2;',
              'var4;',
              'var5;',
              '[[/classes > 1]]')
  model5 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var4 WITH var5;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var4 WITH var5;',
              '[[/classes > 1]]')
  model6 <- c('MODEL:',
              '%CLASS#1%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var4 WITH var5;',
              'var2;',
              'var4;',
              'var5;',
              '[[classes > 1]]',
              '%CLASS#2%',
              '[ var2#1 ];',
              '[ var3#1 ];',
              'var4 WITH var5;',
              'var2;',
              'var4;',
              'var5;',
              '[[/classes > 1]]')
  models <- list(model1, model2, model3, model4, model5, model6)
  expect_equal(create_models(settings), models)


  settings <- define_lca(testdata, 'test', 'id', use = c('var1', 'var2'), categorical = c('var1', 'var2'))
  expect_equal(create_models(settings), list(character(), character(), character(), character(), character(), character()))
})
