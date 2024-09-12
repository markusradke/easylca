library(devtools)

testdata <- readRDS('data-raw/testdata.rds')
testresults <- readRDS('data-raw/testresults.rds')

use_data(testdata,
         internal = F,
         overwrite = T)
use_data(testresults,
         internal = T,
         overwrite = T)
