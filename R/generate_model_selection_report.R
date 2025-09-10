#' Generate LCA model selection report
#'
#' Generates an HTML report that compares all models that were estimated for model selection.
#'
#' @param easylca easylca object created with [perform_lca()] or [rerun_lca()].
#'
#' @return NULL. An html file with the model selection report will be generated in the current working directory.
#' @export
#'
#'
#' @seealso [generate_model_report()]
#' [get_prediction_for_model()]
#' @examples
#' # generate_model_selection_report(titanic_results)
generate_model_selection_report <- function(easylca){
  if(! 'easylca' %in% class(easylca)){
    stop('Please provide an object of type "easylca".')
  }
  rmd_file <- system.file("model_selection_report_template.Rmd", package = "easylca")
  if (!file.exists(rmd_file)) {
    stop("Cannot find the template file for summary report generation: 'model_selection_report_template.Rmd'.\nPlease check your easylca installation.")
  }
  rmarkdown::render(input = rmd_file,
                    output_dir = getwd(),
                    output_file = sprintf('summary_%s.html', easylca$settings$folder_name),
                    params = list(data = easylca$summary,
                                  n_observations = nrow(easylca$settings$frame),
                                  title = easylca$settings$folder_name))
  return(NULL)
}
