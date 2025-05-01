#' Define LCA
#'
#' This command creates a settings environment for performing the easy LCA. It also performs basic checks to see if the information provided can be used to successfully perform an LCA.
#'
#' @param frame
#' This dataframe contains the data you want to perform the LCA on.
#' @param analysis_name
#' A name for the analysis. Please choose a meaningful short name that will also be used for constructing subfolders needed for the files. Date and time will be added to the name automatically.
#' @param id_variable Variable in data frame that contains the unique ids for samples. Will not be included in the analysis.
#' @param weight_variable Variable that contains weightings for all samples in the data set (optional).
#' @param nclasses The maximum number of classes the LCA should be performed for.
#' @param starts Number of random starts for each class and model type. It is advisable to use numbers of the form 2^X*10. Must be provided either as single integer or in the form of a List with six entries for the 6 model types, each entry comprising an integer vector of length nclasses.
#' @param cores The number of cores to use when performing the LCA.
#' @param use Character vector with all variables in the data frame used for the LCA. All other variables will be saved as auxiliary variables in the LCA and not used for computing.
#' @param categoricals Character vector with all ordinal variables. Categorical variables must be stored as integers > 0. (Nominal to be inplemented, categorical works for binary).
#' @param censored_above Character vector with all censored above variables.
#' @param censored_below Character vector with all censored below variables.
#' @param inflated Character vector with all zero-inflated variables (can only be censored or poisson, negbin). Inflation is always assumed to vary between classes.
#' @param poisson Character vector with all poisson-distributed variables. Poisson-distributed variables must be positive integers.
#' @param negbin Character vector with all negative-binomial-distributed variables. Negative-binomial-distributed variables must be positive integers.
#' @param LMRLRT Logical indicating wether to perform the Lo-Mendell-Rubin Likelihood Ratio Test. Attention: Takes a lot of time to perform.
#'
#' @return Environment with settings for the LCA that can be passed to the [perform_lca()] command.
#' @export
#' @examples
#' lca_settings <- define_lca(testdata,
#'  'test_lca',
#'  'id',
#'  nclasses = 2,
#'  starts = list(c(160, 320),
#'                c(160, 320),
#'                c(160, 320),
#'                c(320, 640),
#'                c(320, 640),
#'                c(320, 640)),
#'  use = c('var1', 'var2'),
#'  categorical = 'var2')

define_lca <- function(frame,
                       analysis_name,
                       id_variable,
                       weight_variable = character(),
                       nclasses = 4,
                       starts = 160,
                       cores = 16,
                       use = character(),
                       categorical = character(),
                       censored_above = character(),
                       censored_below = character(),
                       inflated = character(),
                       poisson = character(),
                       negbin = character(),
                       lmrlrt = FALSE){

  create_lca_environment <- function(lca){
    lca$names <- colnames(lca$frame)
    if(length(lca$starts) == 1){
      lca <- create_starts_list(lca)
    }
    lca <- write_time_to_analysis_name(lca)
    lca <- create_use_and_aux_variables(lca)
    lca <- create_free_variance(lca)
    lca <- create_correlate(lca)
    lca
  }

  create_use_and_aux_variables <- function(lca){
    if(length(use) == 0){
      lca$use <- colnames(frame)[! colnames(frame) %in% c(lca$id, lca$weights)]
      lca$auxvariables <- character()
    }
    else {
      lca$auxvariables <- colnames(lca$frame)[! colnames(lca$frame) %in% lca$use]
      lca$auxvariables <- lca$auxvariables[! lca$auxvariables == lca$id]
      if(length(lca$weight) != 0){
        lca$auxvariables <- lca$auxvariables[! lca$auxvariables == lca$weights]
      }
    }
    lca
  }

  write_time_to_analysis_name <- function(lca){
    currenttime <- format(Sys.time(), "%Y%m%d_%H-%M")
    lca$folder_name <- paste(lca$analysis_name, currenttime, sep = '_')
    lca
  }

  create_starts_list <- function(lca){
    new_starts <- list()
    for (i in seq(6)){
      new_starts <- c(new_starts, list(rep(lca$starts, lca$nclasses)))
    }
    lca$starts <- new_starts
    lca
  }

  create_free_variance <- function(lca){
    freevariance <- lca$use[! lca$use %in% lca$categorical]
    freevariance <- freevariance[! freevariance %in% lca$poisson]
    lca$freevariance <- freevariance
    lca
  }

  create_correlate <- function(lca){
    correlate <- lca$use[! lca$use %in% lca$categorical]
    correlate <- correlate[! correlate %in% lca$poisson]
    correlate <- correlate[! correlate %in% lca$negbin]
    correlate <- correlate[! correlate %in% lca$censored_above]
    correlate <- correlate[! correlate %in% lca$censored_below]
    lca$correlate <- correlate
    lca
  }

  check_assertions <- function(lca){
    if(! all(lca$use %in% lca$names)) {stop('Please make sure all variables listed in use are columns in the data frame provided for analysis.')}
    if(!lca$id %in% colnames(lca$frame)) {stop('Please make sure the id variable is a variable in your data frame.')}
    if(lca$id %in% lca$use) {
      stop('Please make sure the id variable is not listed in use.')
      }
    if(lca$nclasses < 1) {stop('Please enter a positive integer value for the number of classes "nclasses".')}
    if(lca$cores < 1) {stop('Please enter a positive integer value for the number of cores "cores".')}
    if(length(lca$starts) != 6){stop('Please supply start values for either all model types as a list with dimensions 6 x nclasses or a single integer vaule for all types and classes.')}
    for (i in seq(6)){
      if(length(lca$starts[[i]]) != lca$nclasses){stop('Please supply start values for either all model types as a list with dimensions 6 x nclasses or a single integer vaule for all types and classes.')}
    }
    if(any(! lca$categorical %in% lca$use) ||
       any(! lca$censored_above %in% lca$use) ||
       any(! lca$censored_below %in% lca$use) ||
       any(! lca$inflated %in% lca$use) ||
       any(! lca$poisson %in% lca$use) ||
       any(! lca$negbin %in% lca$use)) {stop('Please make sure all variables listed in categorical, censored, inflated, poisson, and negbin are also listed in use.')}
    if(any(lca$categorical %in% lca$censored_above) ||
       any(lca$categorical %in% lca$censored_below) ||
       any(lca$categorical %in% lca$inflated) ||
       any(lca$categorical %in% lca$poisson) ||
       any(lca$categorical %in% lca$negbin)) {stop('Please make sure none of the variables listed in categorical are listed in censored, inflated, poisson, or negbin.')}
    if(! all(lca$inflated %in% c(lca$censored_above, lca$censored_below, lca$poisson, lca$negbin))) {
      stop('Please make sure inflated variables are also either censored, poisson or negbin variables.')
    }
    if(! check_categorical_values(lca$frame, lca$categorical)){
      stop('Please make sure categorical variables only contain integers >= 1.')
    }

    poisson_value_check <- any(sapply(lca$frame[lca$poisson], function(x) any(x < 0 | x != floor(x))))
    negbin_value_check <- any(sapply(lca$frame[lca$negbin], function(x) any(x < 0 | x != floor(x))))
    if(poisson_value_check | negbin_value_check){
      stop('Please make sure all negative binomial and poisson variables do not contain negative or non-integer values.')
    }
    if(any(colnames(lca$frame) %>% stringr::str_length() >8)){
      stop('Please make sure no variable name is longer that 8 characters.')
    }
  }

  check_categorical_values <- function(frame, categorical) {
    selected_columns <- frame[categorical]
    result <- sapply(selected_columns, function(col) {
      all(is.na(col) | (col >= 1 & col == floor(col)))
    })
    return(all(result))
  }

  lca <- list(frame = frame,
              analysis_name = analysis_name,
              id = id_variable,
              weights = weight_variable,
              use = use,
              nclasses = as.integer(nclasses),
              starts = starts,
              cores = as.integer(cores),
              categorical = categorical,
              censored_above = censored_above,
              censored_below = censored_below,
              inflated = inflated,
              poisson = poisson,
              negbin = negbin,
              lmrlrt = lmrlrt)


  lca <- create_lca_environment(lca)
  check_assertions(lca)

  structure(lca, class = 'lca_settings')
}
