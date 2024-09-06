#' CREATE MPLUS TEMPLATES ACCORDING TO THE SPECIFICATIONS MADE
#'
#' @param lcasettings
#' @return List of character vectors with model templates. Also saves them as .txt files in current working directory.
#'
#' @examples
create_templates <- function(lca_settings){


  create_headers()
  create_variable_specs()
  # create_model1() # model specs for each type
  # create_model2() # model specs for each type
  # create_model3() # model specs for each type
  # create_model4() # model specs for each type
  # create_model5() # model specs for each type
  # create_model6() # model specs for each type
  create_analysis()
  create_plot_save()

  templates <- make_templates_from_parts()
  save_templates(templates)
  remove_global_lca_environment()
  return(templates)
}

remove_global_lca_environment <- function(){
  rm(lca, envir = globalenv())
}

create_headers <- function(){
  headers <- list()
  for(i in seq(6)){
    extended_name <- paste0(lca$analysis_name, '_model', i, '_lca')
    model_header <- c('[[init]]',
                      'iterators = classes;',
                      paste0('classes = 1:', lca$nclasses),
                      paste0('filename = \"[[classes]]_', extended_name,'.inp\";'),
                      paste0('outputDirectory = \"', extended_name, '\";'),
                      paste0('[[/init]]'),
                      '',
                      paste0('TITLE: ', lca$analysis_name, '[[classes]] classes'),
                      paste0('FILE IS ', extended_name, '_[[classes]].dat;'))
    headers <- c(headers, list(model_header))
  }
  lca$headers <- headers
}

create_variable_specs <- function(){
  variable_specs <- c('VARIABLE:',
                      paste0('NAMES = ', paste(lca$names, collapse = ' '), ';'),
                      'MISSING = .;',
                      paste0('IDVARIABLE = ', lca$id, ';'),
                      paste0('AUXILIARY = ', paste(lca$aux, collapse = ' ')),
                      paste0('USEVARIABLES = ', paste(lca$usevariables, collapse = ' '), ';'))
  if(! is.null(lca$categorical)){
    variable_specs <- c(variable_specs,
                        paste0('CATEGORICAL  = ', paste(lca$categorical, collapse = ' '), ';'))
  }
  variable_specs <- c(variable_specs, 'CLASSES = class ([[classes]])')
  lca$variable_specs <- rep(list(variable_specs), times = 6)
}

create_analysis <- function(){ # TODO
  analysis <- list()
  for (model in seq(6)){
    model_analysis <- c('',
                        'ANALYSIS:',
                        'TYPE = MIXTURE;',
                        paste0('PROCESSORS = ', lca$cores))
    for (class in seq(lca$nclasses)){
      modelclass_starts <- lca$starts[[model]][class]
      class_starts <- c(paste0('[[classes =', class ,']]'),
                        paste0('STITERATIONS = ', as.integer(modelclass_starts / 10)),
                        paste0('STARTS = ', modelclass_starts, ' ', as.integer(modelclass_starts / 10)),
                        paste0('[[/classes = ', class, ']]'))
      model_analysis <- c(model_analysis, class_starts)
    }
    analysis <- c(analysis, list(model_analysis))
  }
  lca$analysis <- analysis
}

create_plot_save <- function(){
  plot_save <- list()
  for(i in seq(6)){
    extended_name <- paste0(lca$analysis_name, '_model', i, '_lca')
    model_plot_save <- c('',
                        'PLOT: TYPE=PLOT1 PLOT2 PLOT3;',
                        'SAVEDATA:',
                        paste0('FILE IS ', extended_name, '_[[classes]].dat'),
                        'SAVE = cprobabilites bchweights;')
    plot_save <- c(plot_save, list(model_plot_save))
  }
  lca$plot_save <- plot_save
}

make_templates_from_parts <- function(){
  templates <- vector("list", length = 6)
  for (i in seq(6)) {
    templates[[i]] <- c(lca$headers[[i]],
                        lca$variable_specs[[i]],
                        lca$analysis[[i]],
                        lca$plot_save[[i]])
  }
  templates
}

save_templates <- function(templates){
  writeLines(templates[[1]], 'base_lca_model1_template.txt')
  writeLines(templates[[2]], 'base_lca_model2_template.txt')
  writeLines(templates[[3]], 'base_lca_model3_template.txt')
  writeLines(templates[[4]], 'base_lca_model4_template.txt')
  writeLines(templates[[5]], 'base_lca_model5_template.txt')
  writeLines(templates[[6]], 'base_lca_model6_template.txt')
}
