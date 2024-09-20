#' Show diagnosis for a specific model
#'
#' @param easylca easylca object as can be obtained with perform_lca() or rerun_lca()
#' @param modeltype type of model. For an explanation on model types please refer to the documentation of perform_lca(). Must have been calculated for the settings specified in the easylca object.
#' @param classes Which k-class solution for the modeltype to show. Must be <= nclasses parameter in the easylca setting.
#'
#' @return Specified model as a list
#' @export
#'
#' @examples
#' show_model(testresults, 2, 3) # shows results for model type 2, 3 class solution
show_model <- function(easylca, modeltype, classes){
  if(! 'easylca' %in% class(easylca)){
    stop('Please provide an object of type "easylca".')
  }
  if(classes > easylca$settings$nclasses){
    stop('You are looking for a class solution that has not been modeled.')
  }
  if(! any(stringr::str_detect(names(easylca$models), as.character(modeltype)))){
    stop('You are looking for a modeltype that has not been modeled.')
  }

  model <- easylca$models[[paste0('modeltype_', modeltype)]][[classes]]
  plot(easylca$plots[[paste0('modeltype_', modeltype)]][[classes]])
  params <- extract_profile_for_plotting(model, easylca$settings) %>%
    dplyr::select(-yposinflation)
  return(dplyr::as_tibble(params))
}
