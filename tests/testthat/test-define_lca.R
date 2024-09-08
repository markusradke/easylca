test_that('definition returns object of class lca_settings', {
  lcasettings <- define_lca(testdata, 'test', 'id')
  expect_equal(class(lcasettings), 'lca_settings')
})

test_that('variable names in settings environment are correct', {
  lcasettings <- define_lca(testdata, 'test', 'id')
  varnames <- c('analysis_name','folder_name', 'use', 'categorical', 'censored_above', 'censored_below', 'cores', 'correlate', 'frame', 'freevariance', 'id',
                'inflated', 'lmrlrt', 'names', 'nclasses', 'negbin', 'poisson', 'starts', 'auxvariables')
  expect_setequal(ls(lcasettings), varnames)
})

test_that('variables classes in settings environment are correct',{
  lcasettings <- define_lca(testdata, 'test', 'id')
  expected_classes <- c('analysis_name' = 'character',
                        'auxvariables' = 'character',
                        'categorical' = 'character',
                        'censored_above' = 'character',
                        'censored_below' = 'character',
                        'cores' = 'integer',
                        'correlate' = 'character',
                        'folder_name' = 'character',
                        'frame' = 'data.frame',
                        'freevariance' = 'character',
                        'id' = 'character',
                        'inflated' = 'character',
                        'lmrlrt' = 'logical',
                        'names' = 'character',
                        'nclasses' = 'integer',
                        'negbin' = 'character',
                        'poisson' = 'character',
                        'starts' = 'list',
                        'use' = 'character')
  actual_variables <- ls(lcasettings)
  actual_classes <- sapply(actual_variables, function(var) class(get(var, envir = lcasettings)))
  expect_equal(actual_classes %>% sort, expected_classes %>% sort)
})

test_that('names contains all colnames of the dataframe', {
  lcasettings <- define_lca(testdata, 'test', 'id')
  expect_setequal(lcasettings$names, colnames(testdata))
})

test_that('folder_name contains current time in minutes', {
  lcasettings <- define_lca(testdata, 'test', 'id')
  currenttime <- format(Sys.time(), "%Y%m%d_%H-%M")
  expect_match(lcasettings$folder_name, currenttime)
})

test_that('usevariables contains all variables of data frame that are not in aux and id', {
  lcasettings <- define_lca(testdata, 'test', 'id', use = c('var1', 'var2'))
  auxvariables <- c('var3', 'var4', 'var5', 'var6', 'var7', 'var8')
  expect_setequal(lcasettings$auxvariables, auxvariables)
})

test_that('correlate contains all variables of usevariables not count, censored (above and  below), and categorical', {
  lcasettings <- define_lca(testdata, 'test', 'id',
                            use = c('var1', 'var2', 'var3', 'var4', 'var5', 'var6'),
                            categorical = c('var1', 'var2'),
                            poisson = 'var6',
                            censored_below = c('var4'),
                            censored_above = c('var5'))
  correlate <- c('var3')
  expect_setequal(lcasettings$correlate, correlate)
})

test_that('freevariance contains all variables of usevariables not poisson or categorical',{
  lcasettings <- define_lca(testdata, 'test', 'id',
                            use = c('var1', 'var2', 'var3', 'var4', 'var5', 'var6', 'var7'),
                            categorical = c('var1', 'var2'),
                            poisson = 'var6',
                            negbin = 'var7',
                            censored_below = 'var4',
                            censored_above = 'var5')
  freevariance <- c('var3', 'var4', 'var5', 'var7')
  expect_setequal(lcasettings$freevariance, freevariance)
})

test_that('Providing less than 1 class throws an error.', {
  expect_error(define_lca(testdata, 'test', 'id', nclasses = 0),
               'Please enter a positive integer value for the number of classes "nclasses".')
})

test_that('Providing wrong format of starts throws an error.', {
  expect_error(define_lca(testdata, 'test', 'id', start=c(160, 160)),
               'Please supply start values for either all model types as a list with dimensions 6 x nclasses or a single integer vaule for all types and classes.')
})

test_that('Providing less than 1 cores throws an error.', {
  expect_error(define_lca(testdata, 'test', 'id', cores = 0),
               'Please enter a positive integer value for the number of cores "cores".')
})

test_that('Providing id not in frame throws an error. ', {
  expect_error(define_lca(testdata, 'test', 'unknown'),
               'Please make sure the id variable is a variable in your data frame.')
})

test_that('Assertion ID not in use.', {
  expect_error(define_lca(testdata, 'test', 'id', use = 'id'),
               'Please make sure the id variable is not listed in use.')
})

test_that('Assertion all variables of use are in colnames of dataframe', {
  expect_error(define_lca(testdata, 'test', 'id', use = 'unknown'),
               'Please make sure all variables listed in use are columns in the data frame provided for analysis.')
})

test_that('Assertion all variables of use are in colnames of dataframe', {
  expect_error(define_lca(testdata, 'test', 'id', use = 'unknown'),
               'Please make sure all variables listed in use are columns in the data frame provided for analysis.')
})

test_that('Assertion all variables of categorical, negbin, poission, censored, inflated are in use.', {
  expect_error(define_lca(testdata, 'test', 'id', categorical = 'var1', use = 'var2'),
               'Please make sure all variables listed in categorical, censored, inflated, poisson, and negbin are also listed in use.')
})

test_that('Assertion none of the categorical variables are listed in negbin, poisson, censored, or inflated.', {
  expect_error(define_lca(testdata, 'test', 'id', categorical = 'var1', inflated = 'var1', use = 'var1'),
               'Please make sure none of the variables listed in categorical are listed in censored, inflated, poisson, or negbin.')
})

test_that('If no use variables are provided, all variables become usevariables by default.', {
  lcasettings <- define_lca(testdata, 'test', 'id')
  expect_setequal(lcasettings$use, colnames(testdata)[colnames(testdata) != 'id'])
})

test_that('Providing an integer for start values and providing a list for start values both works and returns objects of class lca_settings',{   lca_settings_int <- define_lca(testdata, 'test_lca', 'id', starts = 320L)
  lca_settings_list <- define_lca(testdata, 'test_lca', 'id', nclasses = 2, starts = list(c(160, 160),
                                                                                          c(160, 160),
                                                                                          c(160, 160),
                                                                                          c(160, 160),
                                                                                          c(160, 160),
                                                                                          c(160, 160)))
  actual_classes <- c(class(lca_settings_int), class(lca_settings_list))
  expect_equal(actual_classes, c('lca_settings', 'lca_settings'))
})

test_that('Assertion inflated can only be count or censored.', {
  expect_error(define_lca(testdata, 'test', 'id', inflated = 'var1'),
               'Please make sure inflated variables are also either censored, poisson or negbin variables.')
})


test_that('Assertion categorical variables must contain only integers > 1', {
  expect_error(define_lca(testdata, 'test', 'id', categorical = 'var3'),
               'Please make sure categorical variables only contain integers >= 1.')
})

test_that('Aux variables are empty when no aux is specified', {
  settings <- define_lca(testdata, 'test', 'id')
  expect_equal(settings$auxvariables, character())
})
