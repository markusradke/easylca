#' CREATE MPLUS TEMPLATES ACCORDING TO THE SPECIFICATIONS MADE
#'
#' @param lcasettings Settings for the lca, including data, variable specification, and additional technical specifications. Please use the define_lca() command to generate the settings and refer to its documentation for further details.
#' @return List of character vectors with model templates. Also saves them as .txt files in a subfolder with timestamp in the current working directory.
#'
#' @export
#' @examples

create_templates <- function(settings){
  make_templates_from_parts <- function(settings){
    templates <- vector("list", length = 6)
    for (i in seq(6)) {
      templates[[i]] <- c(headers[[i]],
                          '',
                          variable_specs[[i]],
                          '',
                          models[[i]],
                          '',
                          analysis[[i]],
                          '',
                          plot_save[[i]])
    }
    templates
  }

  headers <- create_headers(settings)
  variable_specs <- create_variable_specs(settings)
  models <- create_models(settings)
  analysis <- create_analysis(settings)
  plot_save <- create_plot_save(settings)

  templates <- make_templates_from_parts()
  save_templates(templates, settings)
  return(templates)
}

create_headers <- function(settings){
  headers <- list()
  for(i in seq(6)){
    extended_name <- paste0(settings$analysis_name, '_model', i, '_lca')
    model_header <- c('[[init]]',
                      'iterators = classes;',
                      paste0('classes = 1:', settings$nclasses, ';'),
                      paste0('filename = \"[[classes]]_', extended_name,'.inp\";'),
                      paste0('outputDirectory = \"', extended_name, '\";'),
                      paste0('[[/init]]'),
                      '',
                      paste0('TITLE: ', settings$analysis_name, '[[classes]] classes'),
                      paste0('DATA: FILE IS ', extended_name, '.dat;'))
    headers <- c(headers, list(model_header))
  }
  headers
}

create_variable_specs <- function(settings){
  variable_specs <- c('VARIABLE:',
                      paste0('NAMES = ', paste(settings$names, collapse = ' '), ';'),
                      paste0('IDVARIABLE = ', settings$id, ';'),
                      paste0('USEVARIABLES = ', paste(settings$use, collapse = ' '), ';'))
  if(length(settings$categorical) != 0){
    variable_specs <- c(variable_specs,
                        paste0('CATEGORICAL = ', paste(settings$categorical, collapse = ' '), ';'))
  }
  if (length(settings$censored_above) != 0 || length(settings$censored_below) != 0){
    censoring <- create_censoring(settings)
    variable_specs <- c(variable_specs, censoring)
  }
  if (length(settings$poisson) != 0 || length(settings$negbin) != 0){
    censoring <- create_count(settings)
    variable_specs <- c(variable_specs, censoring)
  }
  if(length(settings$auxvariables) != 0){
    variable_specs <- c(variable_specs,
                        paste0('AUXILIARY = ', paste(settings$auxvariables, collapse = ' '), ';'))
  }
  variable_specs <- c(variable_specs,
                      'MISSING = .;',
                      'CLASSES = class ([[classes]]);')
  variable_specs <- rep(list(variable_specs), times = 6)
  variable_specs
}

create_censoring <- function(settings) {
  censoring <- 'CENSORED ='
  censoring <- add_type_inflation_specification_to_variable_listing(censoring, settings$censored_above, 'a', settings$inflated)
  censoring <- add_type_inflation_specification_to_variable_listing(censoring, settings$censored_below, 'b', settings$inflated)
  censoring <- paste0(censoring, ';')
}

create_count <- function(settings) {
  count <- 'COUNT ='
  count <- add_type_inflation_specification_to_variable_listing(count, settings$poisson, 'p', settings$inflated)
  count <- add_type_inflation_specification_to_variable_listing(count, settings$negbin, 'nb', settings$inflated)
  count <- paste0(count, ';')
}

add_type_inflation_specification_to_variable_listing <- function(listing, variables, type, inflated){
  for (var in variables){
    if (var %in% inflated){
      listing <- paste0(listing, ' ', var, ' (', type, 'i)')
    }
    else {listing <- paste0(listing, ' ', var, ' (', type, ')')}
  }
  listing
}

create_analysis <- function(settings){
  analysis <- list()
  for (model in seq(6)){
    model_analysis <- c('ANALYSIS:',
                        'TYPE = MIXTURE;',
                        paste0('PROCESSORS = ', settings$cores, ';'))
    for (class in seq(settings$nclasses)){
      modelclass_starts <- settings$starts[[model]][class]
      class_starts <- c(paste0('[[classes = ', class ,']]'),
                        paste0('STITERATIONS = ', as.integer(modelclass_starts / 5), ';'),
                        paste0('STARTS = ', modelclass_starts, ' ', as.integer(modelclass_starts / 5), ';'),
                        paste0('[[/classes = ', class, ']]'))
      model_analysis <- c(model_analysis, class_starts)
    }
    analysis <- c(analysis, list(model_analysis))
  }
  analysis
}

create_plot_save <- function(settings){
  plot_save <- list()
  for(i in seq(6)){
    extended_name <- paste0(settings$analysis_name, '_model', i, '_lca')
    model_plot_save <- c('OUTPUT:')
    if(settings$lmrlrt) {model_plot_save <- c(model_plot_save, 'SVALUES ENTROPY TECH1 TECH4 TECH11;')}
    else{model_plot_save <- c(model_plot_save, 'SVALUES ENTROPY TECH1 TECH4;')}
    model_plot_save <- c(model_plot_save,
                        'PLOT: TYPE=PLOT1 PLOT2 PLOT3;',
                        'SAVEDATA:',
                        paste0('FILE IS ', extended_name, '_[[classes]].dat'),
                        'SAVE = cprobabilites bchweights;')
    plot_save <- c(plot_save, list(model_plot_save))
  }
  plot_save
}

save_templates <- function(templates, settings){
  if(! dir.exists(settings$folder_name)){
    dir.create(settings$folder_name)
  }
  for(i in seq(6)){
    writeLines(templates[[i]],
               paste0(settings$folder_name, '/', settings$analysis_name, '_model', i, '_template.txt'))
  }
}
