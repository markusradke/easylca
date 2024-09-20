library(devtools)

testdata <- readRDS('data-raw/testdata.rds')


set.seed(123)
random_weights <- runif(n = nrow(testdata), min = 0.5, max = 2)
testdata_weights <- testdata
testdata_weights$weights <- random_weights

testresults <- readRDS('data-raw/testresults.rds')

# generate testresults
testsettings <- define_lca(testdata, 'test','id', nclasses = 3, starts = 20L, cores = 16,
                       use = c('var1', 'var3', 'var4', 'var5', 'var6', 'var7', 'var8'),
                       categorical = c('var1'),
                       censored_above = c('var6'),
                       censored_below = c('var3', 'var4'),
                       poisson = 'var8',
                       negbin = 'var7',
                       inflated = c('var7', 'var4'))
testresults <- perform_lca(testsettings)

use_data(testdata,
         internal = F,
         overwrite = T)
use_data(testresults,
         testdata_weights,
         internal = T,
         overwrite = T)
