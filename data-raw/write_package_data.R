library(devtools)

testdata <- readRDS('data-raw/testdata.rds')

# basemodel_templates <- list(readLines('data-raw/base_lca_model1_template.txt'),
#                             readLines('data-raw/base_lca_model2_template.txt'),
#                             readLines('data-raw/base_lca_model3_template.txt'),
#                             readLines('data-raw/base_lca_model4_template.txt'),
#                             readLines('data-raw/base_lca_model5_template.txt'),
#                             readLines('data-raw/base_lca_model6_template.txt'))

use_data(testdata,
         internal = F,
         overwrite = T)
