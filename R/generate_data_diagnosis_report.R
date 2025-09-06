#' Diagnose Data
#'
#'Generates an HTML report that informs on:
#'1. Relative frequency of Missings for all indicators
#'2. Correlations between continuous indicators and contigency tables between discrete indicators
#'3. Distributions of variables for all indicators
#'
#' Discrete variables must be supplied as integers with values > 0 to be interpreted the right way (as is also necessary for the LCA). Discrete variables should not have more than 10 levels.
#'
#' **Coutinuous variables must be explicitly be supplied as type double**, else they will be treated as discrete variables.
#'
#' @param frame The data frame you want to perform the LCA on.
#' @param idcol The id column of the data frame. It will be excluded from the diagnosis.
#' @param title title of the data set / project for display in the HTML document.
#'
#' @return NULL. An html file with the diagnosis will be generated in the current working directory.
#' @export
#'
#' @examples
#' generate_data_diagnosis_report(titanic_passengers, 'id', 'titanic')
#' file.remove('data_diagnosis_report.html')
generate_data_diagnosis_report <- function(frame, idcol, title){
  assert_data_format(frame, idcol)

  rmd_file <- system.file("diagnosis_report_template.Rmd", package = "easylca")
  if (!file.exists(rmd_file)) {
    stop("Cannot find the template file for data diagnosis report generation: 'diagnosis_report_template.Rmd'.\nPlease check your easylca installation.")
  }
  rmarkdown::render(input = rmd_file,
                    output_dir = getwd(),
                    output_file = sprintf('data_diagnosis_%s.html', title),
                    params = list(data = frame,
                                  idcol = idcol,
                                  title = title))
}

assert_data_format <- function(frame, idcol){
  is_dataframe <- 'data.frame' %in% class(frame)
  if(! is_dataframe){stop('Please provide an object of class data.frame or tibble for analysis.')}

  types <- sapply(frame %>% dplyr::select(-all_of(idcol)), class)
  discrete <- types[types == 'integer'] %>% names()
  continuous <- types[types == 'numeric'] %>% names()

  wrong_types <- setdiff(colnames(frame %>% dplyr::select(-all_of(idcol))),
                            union(discrete, continuous))
  is_wrong_types <- length(wrong_types) > 0
  if(is_wrong_types){stop(sprintf('Please make sure all indicators are coded as either double or integer.\nProblem caused by: %s', paste(wrong_types, collapse = ', ')))}

  only_discretes <- frame %>%
    dplyr::select(all_of(discrete))
  if(ncol(only_discretes) > 0){
    unique_counts <- sapply(only_discretes, function(x) length(unique(x)))
    more_than_10 <- unique_counts[unique_counts > 10] %>% names()
    is_levels_more_10 <- length(more_than_10) > 0
  } else {is_levels_more_10 <- F}
  if(is_levels_more_10){stop(sprintf('Please make sure all discrete indicators have <= 10 levels. Make sure that all continuous varialbes are explictly coded as doubles by using as.double.\nProblem caused by: %s',
                                     paste(more_than_10, collapse = ', ')))}

}
