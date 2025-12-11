rm(list = ls())
library(devtools)
load_all()

# titanic passengers example data set ----
ports_lookup <- c('S' = 1, 'C' = 2, 'Q' = 3) # Southhampton, Cherbourg, Queenstown
# reocde SibSp and Parch:
# # of siblings or spouses aboard (ordinal), levels: 0, 1, 2 or more
# # of parents or children aboard (ordinal), levels: 0, 1, 2 or more
titanic_passengers <- titanic::titanic_train %>%
  dplyr::mutate(
    survived = (Survived + 1) %>% as.integer(),
    isfem = ifelse(Sex == 'female', 2, 1) %>% as.integer(),
    port = ports_lookup[Embarked] %>% as.integer(),
    nsibsp = (ifelse(SibSp < 2, SibSp, 2) + 1) %>% as.integer(),
    nparchi = (ifelse(Parch < 2, Parch, 2) + 1) %>% as.integer()
  ) %>%
  dplyr::select(
    id = PassengerId,
    survived,
    pasclass = Pclass,
    age = Age,
    fare = Fare,
    nsibsp,
    nparchi,
    isfem,
    port
  )


titanic_settings <- define_lca(
  frame = titanic_passengers,
  analysis_name = 'titanic',
  id_variable = 'id',
  nclasses = 3,
  nominal = c('port', 'pasclass'),
  categorical = c('survived', 'isfem', 'nsibsp', 'nparchi'),
  starts = 80,
  cores = 16
)

titanic_lca_results <- readRDS('data-raw/titanic_lca_results.rds')


wehner_testdata <- read.csv(
  "wehner_data.csv",
  sep = ";",
  dec = ",",
  fileEncoding = "UTF-8"
)
wehner_testdata$BIRTH <- as.double(wehner_testdata$BIRTH)
wehner_testdata$SCYEARS <- as.double(wehner_testdata$SCYEARS)
wehner_testdata$FESTUSE <- as.double(wehner_testdata$FESTUSE)

# wehner_testresults were generated like this:
# lca_settings <- easylca::define_lca(
#   frame = wehner_testdata,
#   analysis_name = "LCA_Max",
#   id_variable = "ID",
#   nclasses = 4,
#   starts = 50,
#   cores = 4,
#   use = c(
#     "CLUBUSE",
#     "HOWMUSIC",
#     "HOWSCENE",
#     "HOWPEOPL",
#     "HOWORG",
#     "HOWCLOTH",
#     "HOWMONEY",
#     "HOWSOCM",
#     "HOWDRUG",
#     "HOWVOCAL",
#     "HOWDAYNI",
#     "HOWOPAIR",
#     "HOWPSYCH",
#     "HOWSCINT",
#     "MUSK",
#     "SCEARS"
#   ),

#   categorical = c(
#     "CLUBUSE",
#     "HOWMUSIC",
#     "HOWSCENE",
#     "HOWPEOPL",
#     "HOWORG",
#     "HOWCLOTH",
#     "HOWMONEY",
#     "HOWSOCM",
#     "HOWDRUG",
#     "HOWVOCAL",
#     "HOWDAYNI",
#     "HOWOPAIR",
#     "HOWPSYCH",
#     "HOWSCINT",
#     "MUSK",
#     "SCEARS"
#   )
# )

# lca_results <- perform_lca(lca_settings, modeltypes = 1, vlmrt = FALSE)
wehner_testresults <- readRDS('data-raw/wehner_testresults.rds')

# small random test data for testing ----
random_testdata <- readRDS('data-raw/testdata.rds')
# generate testdata
set.seed(123)
random_weights <- runif(n = nrow(random_testdata), min = 0.5, max = 2)
random_testdata_weights <- random_testdata
random_testdata_weights$weights <- random_weights

random_testresults <- readRDS('data-raw/testresults.rds')
# testresults were generated like this:
# testsettings <- define_lca(random_testdata, 'test','id', nclasses = 3, starts = 20L, cores = 16,
#                        use = c('var1', 'var3', 'var4', 'var5', 'var6', 'var7', 'var8'),
#                        categorical = c('var1'),
#                        censored_above = c('var6'),
#                        censored_below = c('var3', 'var4'),
#                        poisson = 'var8',
#                        negbin = 'var7',
#                        inflated = c('var7', 'var4'))
# random_testresults <- perform_lca(testsettings)

# other utility data for functions ----
discrete_colors_for_classes <- Polychrome::dark.colors() # 24 distinct colors
names(discrete_colors_for_classes) <- NULL

use_data(
  titanic_passengers,
  titanic_settings,
  titanic_lca_results,
  internal = F,
  overwrite = T
)
use_data(
  random_testdata,
  random_testdata_weights,
  random_testresults,
  discrete_colors_for_classes,
  wehner_testdata,
  wehner_testresults,
  internal = T,
  overwrite = T
)
rm(list = ls())
