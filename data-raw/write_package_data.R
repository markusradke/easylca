library(devtools)

testdata <- readRDS('data-raw/testdata.rds')

use_data(testdata,
         internal = F,
         overwrite = T)
