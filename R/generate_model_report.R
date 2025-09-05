#' Show information on a specific model
#'
#' Generates an HTML report with detailed information on the model specified.
#'
#' @param easylca easylca object as can be obtained with [perform_lca()] or [rerun_lca()].
#' @param modeltype type of model. For an explanation on model types please refer to the documentation of [perform_lca()]. Must have been calculated for the settings specified in the easylca object.
#' @param classes Which k-class solution for the modeltype to show. Must be less or equal to nclasses parameter in the easylca setting.
#'
#' @return NULL. An html file with the model information will be generated in the current working directory.
#' @export
#'
#' @seealso [generate_model_selection_report()]
#' [get_prediction_for_model()]
#' @examples
#' generate_model_report(testresults, 2, 3) # generates a report for model a type-2 model with 3 classes
generate_model_report <- function(easylca, modeltype, classes){
  if(! 'easylca' %in% class(easylca)){
    stop('Please provide an object of type "easylca".')
  }
  if(classes > easylca$settings$nclasses){
    stop('You are looking for a class solution that has not been modeled.')
  }
  if(! any(stringr::str_detect(names(easylca$models), as.character(modeltype)))){
    stop('You are looking for a modeltype that has not been modeled.')
  }

  rmd_file <- system.file("model_report_template.Rmd", package = "easylca")
  if (!file.exists(rmd_file)) {
    stop("Cannot find the template file for summary report generation: 'model_report_template.Rmd'.\nPlease check your easylca installation.")
  }
  rmarkdown::render(input = rmd_file,
                    output_dir = getwd(),
                    output_file = sprintf('modeltype-%d_%d-classes_%s.html',
                                          modeltype, classes,
                                          easylca$settings$folder_name),
                    params = list(easylca = easylca,
                                  modeltype = modeltype,
                                  classes = classes))
  return(NULL)
}
