#' CREATE MPLUS TEMPLATES ACCORDING TO THE SPECIFICATIONS MADE
#'
#' @param frame
#' @param analysis_name
#' @param nclasses
#' @param starts
#' @param cores
#' @param categoricals
#' @param censored
#' @param inflated
#' @param poisson
#' @param negbin
#' @param aux
#' @param LMRLRT
#'
#' @return List of character vectors with model templates. Also saves them as .txt files in current working directory.
#'
#' @examples

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
    if(is.integer(lca$starts)){
      create_starts_list()
    }
    write_time_to_analysis_name()
    create_use_variables()
    create_starts_list()
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
    lca$analysis_name <- paste(lca$analysis_name, currenttime, sep = '_')
  }

  create_starts_list <- function(){
    starts <- list()
    for (i in seq(6)){
      starts <- c(starts, list(rep(lca$starts, lca$nclasses)))
    }
    lca$starts <- starts
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
    if(any(lca$categorical %in% lca$cnesored) ||
       any(lca$categorical %in% lca$inflated) ||
       any(lca$categorical %in% lca$poisson) ||
       any(lca$categorical %in% lca$negbin)) {stop('Please make sure none of the variables listed in categorical are listed in censored, inflated, poisson, or negbin.')}
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
