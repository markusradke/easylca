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
#' @return List of character vectors with model templates. Also saves them as .txt files in current workind directory.
#' @export
#'
#' @examples
create_templates <- function(frame,
                            analysis_name,
                            nclasses = 4,
                            starts = rep(160, 4), # same for all types
                            cores = 16,
                            categoricals = c(),
                            censored = c(),
                            inflated = c(),
                            poisson = c(),
                            negbin = c(),
                            aux = c(),
                            LMRLRT = FALSE){

  headers <- create_headers(analysis_name, nclasses) # incl TITLE: DATA: lines => get a list of 6 strings for each type
  # create_variable_specs() # same for all types
  # create_model1() # model specs for each type
  # create_model2() # model specs for each type
  # create_model3() # model specs for each type
  # create_model4() # model specs for each type
  # create_model5() # model specs for each type
  # create_model6() # model specs for each type
  # create_analysis() # same for all types (also with starts, yes)
  # create_plot_save() # get a list of 6 strings for each type

  # make_templates() # list with 6 templates
  save_templates(headers) # save all templates
  # return(templates)
  return(NULL) #
}

create_headers <- function(analysis_name, nclasses){
  res <- list()
  for(i in seq(6)){
    extended_name <- paste0(analysis_name, '_model', i, '_lca')
    model_header <- c('[[init]]',
                      'iterators = classes;',
                      paste0('classes = 1:', nclasses),
                      paste0('filename = \"[[classes]]_', extended_name,'.inp\";'),
                      paste0('outputDirectory = \"', extended_name, '\";'),
                      paste0('[[/init]]'),
                      '',
                      paste0('TITLE: ', analysis_name, '[[classes]] classes'),
                      paste0('FILE IS ', extended_name, '_[[classes]].dat;'))
    res <- c(res, list(model_header))
  }
  res
}

save_templates <- function(templates){
  writeLines(templates[[1]], 'base_lca_model1_template.txt')
  writeLines(templates[[2]], 'base_lca_model2_template.txt')
  writeLines(templates[[3]], 'base_lca_model3_template.txt')
  writeLines(templates[[4]], 'base_lca_model4_template.txt')
  writeLines(templates[[5]], 'base_lca_model5_template.txt')
  writeLines(templates[[6]], 'base_lca_model6_template.txt')
}
