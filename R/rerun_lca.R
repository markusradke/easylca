#' Rerun LCA
#'
#' @param easylca easylca object obtained from the perform_lca() or rerun_lca() command.
#' @param models_and_starts models to rerun with new start values. Must be a data.frame with columns: classes, modeltype, starts. If NULL (default) will rerun all models that were not replicated with doubled number of starts.
#'
#' @return easylca object with updated values
#' @export
#'
#' @examples
#' lca <- perform_lca(testresults$settings, modeltypes = c(1,2))
#'
#' # automatic update of start values and choice of models
#' rerun_lca(lca)
#'
#' # manual update of start values and choice of models
#' rerun_lca(lca, data.frame(modeltype = c(1, 1), classes = c(2, 3), starts = c(30, 160)))
rerun_lca <- function(easylca, models_and_starts = NULL){
  if(class(easylca) != 'easylca'){
    stop('Please make sure to supply an object of type "easylca" as input to the rerun_lca function. \n"easylca" objects can be obtained through the perform_lca command.')
  }
  if (is.null(models_and_starts)) {
    models_and_starts <- create_models_and_starts_for_rerun(easylca)
  }
  print_rerun(models_and_starts)

  check_assertions_models_and_starts(easylca, models_and_starts)
  easylca$settings <- update_starts_in_settings(easylca$settings, models_and_starts)
  create_templates(easylca$settings)

  start_time <- Sys.time()
  for(row in seq(nrow(models_and_starts))){
    rerun_modeltype <- rerun_mplus_lca_single_model(easylca$settings,
                                              models_and_starts$modeltype[row],
                                              models_and_starts$classes[row])
    easylca$models[[models_and_starts$modeltype[row]]] <- rerun_modeltype
  }

  easylca$summary <- create_modeloverview(easylca$models, settings)
  easylca <- create_all_figures(easylca, models_and_starts$modeltype %>% unique())

  end_time <- Sys.time()
  print_elapsed_time(start_time, end_time)
  return(easylca)
}

create_models_and_starts_for_rerun <- function(easylca){
  starts <- easylca$settings$starts
  easylca$summary %>% dplyr::filter(! replicated) %>%
    dplyr::select(classes, modeltype) %>%
    dplyr::mutate(starts = purrr::map2_int(modeltype, classes,
                                           function(modeltype, classes){ starts[[modeltype]][[classes]] * 2}))
}

check_assertions_models_and_starts <- function(easylca, models_and_starts){
  if(! all(c('classes', 'modeltype', 'starts') %in% colnames(models_and_starts))){
    stop('models_and_frame parameter does not contain all neccessary columns for the operation. Please include "classes", "modeltype" and "start".')
  }
  if(any(! models_and_starts$modeltype %in% c(1,2,3,4,5,6))) {
    stop('Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')
  }
  if(any(models_and_starts$classes %>% floor != models_and_starts$classes) | any(models_and_starts$classes < 1)){
    stop('Please make sure models_and_frame contains only classes with integers > 0.')
  }
}


print_rerun <- function(models_and_starts){
  frameprint <- capture.output(print(models_and_starts))
  message('Rerunning:\n', paste(frameprint, collapse = "\n"))
}

update_starts_in_settings <- function(settings, models_and_starts){
  for (row in seq(nrow(models_and_starts))){
    modeltype <- models_and_starts$modeltype[row]
    classes <- models_and_starts$classes[row]
    settings$starts[[modeltype]][[classes]] <- models_and_starts$starts[row]
  }
  settings
}

rerun_mplus_lca_single_model <- function(settings, modeltype, class){
  setwd(settings$folder_name)
  analysis <- paste0(settings$analysis_name, '_model', modeltype)
  type_folder <- paste0(analysis, '_lca')

  if (!dir.exists(type_folder)) {dir.create(type_folder)}
  file.copy(paste0(analysis,"_template.txt"),paste0(analysis,"_lca/"))

  datafile <- paste0(analysis,"_lca/",analysis,"_lca.dat")
  if (! file.exists(datafile)){
    MplusAutomation::prepareMplusData(settings$frame, datafile)
  }
  MplusAutomation::createModels(paste0(analysis,"_lca/",analysis,"_template.txt"))

  MplusAutomation::runModels(paste0(type_folder, '/', class, '_', type_folder, '.inp'),
                             logFile=paste0(analysis,"_lca/", analysis,"_log.txt"),showOutput=F,quiet=F)

  mplus_results<- MplusAutomation::readModels(type_folder,recursive=T)
  saveRDS(mplus_results,paste0(analysis,"_lca/",analysis,".rds"))
  setwd('..')
  return(mplus_results)
}
