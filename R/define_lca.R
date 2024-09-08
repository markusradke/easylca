#' Define LCA
#'
#' This command creates a settings environment for performing the easy LCA. It also performs basic checks to see if the information provided can be used to successfully perform an LCA.
#'
#' @param frame
#' This dataframe contains the data you want to perform the LCA on.
#' @param analysis_name
#' A name for the analysis. Please choose a meaningful short name that will also be used for constructing subfolders needed for the files. Date and time will be added to the name automatically.
#' @param nclasses The maximum number of classes the LCA should be performed for.
#' @param starts Number of random starts for each class and model type. It is advisable to use numbers of the form 2^X*10. Must be provided either as single integer or in the form of a List with six entries for the 6 model types, each entry comprising an integer vector of length nclasses.
#' @param cores The number of core to use when performing the LCA.
#' @param use Character vector with all variables in the data frame used for the LCA. All other variables will be saved as auxiliary variables in the LCA and not used for computing.
#' @param categoricals Character vector with all categorical variables.
#' @param censored Character vector with all censored variables.
#' @param inflated Character vector with all zero-inflated variables.
#' @param poisson Character vector with all poisson-distributed variables.
#' @param negbin Character vector with all negative binomial distributed variables.
#' @param LMRLRT Logical indicating wether to perform the Lo-Mendell-Rubin Likelihood Ratio Test. Attention: Takes a lot of time to perform.
#'
#' @return Environment with settings for the LCA that can be passed to the perform_lca() command.
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
#'  use = c('nd', 'cat1'),
#'  categorical = 'cat1')

define_lca <- function(frame,
                       analysis_name,
                       id_variable,
                       nclasses = 4,
                       starts = 160,
                       cores = 16,
                       use = character(),
                       categorical = character(),
                       censored = character(),
                       inflated = character(),
                       poisson = character(),
                       negbin = character(),
                       lmrlrt = FALSE){

  create_lca_environment <- function(lca){
    lca$names <- colnames(lca$frame)
    lca$auxvariables <- colnames(lca$frame)[! colnames(lca$frame) %in% lca$use]
    lca$auxvariables <- lca$auxvariables[! lca$auxvariables == lca$id]
    if(length(lca$starts) == 1){
      create_starts_list()
    }
    write_time_to_analysis_name()
    create_use_variables()
    create_free_variance()
    create_correlate()
  }

  create_use_variables <- function(){
    if(length(use) == 0){
      lca$use <- colnames(frame)[colnames(frame) != lca$id]
    }
  }

  write_time_to_analysis_name <- function(){
    currenttime <- format(Sys.time(), "%Y%m%d_%H-%M")
    lca$folder_name <- paste(lca$analysis_name, currenttime, sep = '_')
  }

  create_starts_list <- function(){
    new_starts <- list()
    for (i in seq(6)){
      new_starts <- c(new_starts, list(rep(lca$starts, lca$nclasses)))
    }
    lca$starts <- new_starts
  }

  create_free_variance <- function(){
    freevariance <- lca$use[! lca$use %in% lca$categorical]
    freevariance <- freevariance[! freevariance %in% lca$poisson]
    freevariance <- freevariance[! freevariance %in% lca$negbin]
    lca$freevariance <- freevariance
  }

  create_correlate <- function(){
    correlate <- lca$use[! lca$use %in% lca$categorical]
    correlate <- correlate[! correlate %in% lca$poisson]
    correlate <- correlate[! correlate %in% lca$negbin]
    correlate <- correlate[! correlate %in% lca$censored]
    lca$correlate <- correlate
  }

  check_assertions <- function(){
    if(! all(lca$use %in% lca$names)) {stop('Please make sure all variables listed in use are columns in the data frame provided for analysis.')}
    if(!lca$id %in% colnames(lca$frame)) {stop('Please make sure the id variable is a variable in your data frame.')}
    if(lca$id %in% lca$categorical ||
       lca$id %in% lca$censored ||
       lca$id %in% lca$inflated ||
       lca$id %in% lca$poisson ||
       lca$id %in% lca$negbin) {stop('Please make sure the id variable is listed not in any of the following: use, categorical,censored, inflated, poisson, negbin.')}
    if(lca$nclasses < 1) {stop('Please enter a positive integer value for the number of classes "nclasses".')}
    if(lca$cores < 1) {stop('Please enter a positive integer value for the number of cores "cores".')}
    if(length(lca$starts) != 6){stop('Please supply start values for either all model types as a list with dimensions 6 x nclasses or a single integer vaule for all types and classes.')}
    for (i in seq(6)){
      if(length(lca$starts[[i]]) != lca$nclasses){stop('Please supply start values for either all model types as a list with dimensions 6 x nclasses or a single integer vaule for all types and classes.')}
    }
    if(any(! lca$categorical %in% lca$use) ||
       any(! lca$censored %in% lca$use) ||
       any(! lca$inflated %in% lca$use) ||
       any(! lca$poisson %in% lca$use) ||
       any(! lca$negbin %in% lca$use)) {stop('Please make sure all variables listed in categorical, censored, inflated, poisson, and negbin are also listed in use.')}
    if(any(lca$categorical %in% lca$censored) ||
       any(lca$categorical %in% lca$inflated) ||
       any(lca$categorical %in% lca$poisson) ||
       any(lca$categorical %in% lca$negbin)) {stop('Please make sure none of the variables listed in categorical are listed in censored, inflated, poisson, or negbin.')}
    if(! all(lca$inflated %in% c(lca$censored, lca$poisson, lca$negbin))) {
      stop('Please make sure inflated variables are also either censored, poisson or negbin variables.')
    }
  }


  lca <- rlang::env(frame = frame,
                    analysis_name = analysis_name,
                    id = id_variable,
                    use = use,
                    nclasses = as.integer(nclasses),
                    starts = starts,
                    cores = as.integer(cores),
                    categorical = categorical,
                    censored = censored,
                    inflated = inflated,
                    poisson = poisson,
                    negbin = negbin,
                    lmrlrt = lmrlrt)


  create_lca_environment(lca)
  check_assertions()

  structure(lca, class = 'lca_settings')
}
