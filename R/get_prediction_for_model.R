#' Get Model Predictions
#'
#' @param easylca easylca object created with [perform_lca()] or [rerun_lca()].
#' @param modeltype type of model. For an explanation on model types please refer to the documentation of [perform_lca()]. Must have been calculated for the settings specified in the easylca object.
#' @param classes Which k-class solution for the modeltype to show. Must be less or equal to nclasses parameter in the easylca setting.
#'
#' @return Data frame including all data as well as model predictions (hard classes, probabilites for classes).
#' @export
#'
#' @seealso [generate_model_selection_report()]
#' [generate_model_report()]
#'
#' @examples
#' # get_prediction_for_model(titanic_results, 3, 3)
get_prediction_for_model <- function(easylca, modeltype, classes){
  if(! 'easylca' %in% class(easylca)){
    stop('Please provide an object of type "easylca".')
  }
  if(classes > easylca$settings$nclasses){
    stop('You are looking for a class solution that has not been modeled.')
  }
  if(! any(stringr::str_detect(names(easylca$models), as.character(modeltype)))){
    stop('You are looking for a modeltype that has not been modeled.')
  }
  savedata <- easylca$models[[paste0('modeltype_', modeltype)]][[classes]]$savedata
  colnames(savedata) <- tolower(colnames(savedata))
  return(savedata)
}
