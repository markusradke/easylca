#' CREATE MPLUS TEMPLATES ACCORDING TO THE SPECIFICATIONS MADE
#'
#' @param lcasettings
#' @return List of character vectors with model templates. Also saves them as .txt files in current working directory.
#'
#' @export
#' @examples

create_templates <- function(settings){
  make_templates_from_parts <- function(settings){
    templates <- vector("list", length = 6)
    for (i in seq(6)) {
      templates[[i]] <- c(headers[[i]],
                          variable_specs[[i]],
                          analysis[[i]],
                          plot_save[[i]])
    }
    templates
  }

  headers <- create_headers(settings)
  variable_specs <- create_variable_specs(settings)
  # create_model1() # model specs for each type
  # create_model2() # model specs for each type
  # create_model3() # model specs for each type
  # create_model4() # model specs for each type
  # create_model5() # model specs for each type
  # create_model6() # model specs for each type
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
                      paste0('classes = 1:', settings$nclasses),
                      paste0('filename = \"[[classes]]_', extended_name,'.inp\";'),
                      paste0('outputDirectory = \"', extended_name, '\";'),
                      paste0('[[/init]]'),
                      '',
                      paste0('TITLE: ', settings$analysis_name, '[[classes]] classes'),
                      paste0('FILE IS ', extended_name, '_[[classes]].dat;'))
    headers <- c(headers, list(model_header))
  }
  headers
}

create_variable_specs <- function(settings){
  variable_specs <- c('VARIABLE:',
                      paste0('NAMES = ', paste(settings$names, collapse = ' '), ';'),
                      paste0('IDVARIABLE = ', settings$id, ';'),
                      paste0('USEVARIABLES = ', paste(settings$use, collapse = ' '), ';'))
  if(! is.null(settings$categorical)){
    variable_specs <- c(variable_specs,
                        paste0('CATEGORICAL = ', paste(settings$categorical, collapse = ' '), ';'))
  }
  if(! is.null(settings$auxvariables)){
    variable_specs <- c(variable_specs,
                        paste0('AUXILIARY = ', paste(settings$auxvariables, collapse = ' '), ';'))
  }
  variable_specs <- c(variable_specs,
                      'MISSING = .;',
                      'CLASSES = class ([[classes]]);')
  variable_specs <- rep(list(variable_specs), times = 6)
  variable_specs
}

create_analysis <- function(settings){ # TODO
  analysis <- list()
  for (model in seq(6)){
    model_analysis <- c('',
                        'ANALYSIS:',
                        'TYPE = MIXTURE;',
                        paste0('PROCESSORS = ', settings$cores))
    for (class in seq(settings$nclasses)){
      modelclass_starts <- settings$starts[[model]][class]
      class_starts <- c(paste0('[[classes =', class ,']]'),
                        paste0('STITERATIONS = ', as.integer(modelclass_starts / 10)),
                        paste0('STARTS = ', modelclass_starts, ' ', as.integer(modelclass_starts / 10)),
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
    model_plot_save <- c('',
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
