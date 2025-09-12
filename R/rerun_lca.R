#' Rerun LCA
#'
#' Rerun LCA that was initially conducted with the [perform_lca()] command with other start values to get replicated results.
#' Inspect the results with [generate_model_selection_report()], [generate_model_report()], and [get_prediction_for_model()].
#'
#' @param easylca easylca object obtained from the [perform_lca()] or [rerun_lca()] command.
#' @param models_and_starts models to rerun with new start values. Must be a data.frame with columns: classes, modeltype, starts. If NULL (default) will rerun all models that were not replicated with doubled number of starts.
#' @param recursive If TRUE will repeat the rerun process while doubling the number of starts until all models are converged and replicated (maximum number of 10 repeats).
#' @param vlmrt Logical indicating wether to perform a Likelihood Ratio Test for class enumaration. The test was proposed by Lo, Mendell, & Rubin (2001) based on work by Vuong (1989). Attention: Takes a lot of time to perform.
#'
#' @return easylca object with updated values
#' @export
#' @seealso [perform_lca()]
#' @examples
#' # takes a while to compute
#' # lca <- perform_lca(titanlic_settings, modeltypes = c(1,2))
#'
#' # automatic update of start values and choice of models
#' # rerun_lca(lca)
#'
#' # manual update of start values and choice of models
#' # rerun_lca(lca, data.frame(modeltype = c(1, 1), classes = c(2, 3), starts = c(30, 160)))
#'
#' # repeat until all converged and replicated (max. 10 times)
#' # rerun_lca(lca, recursive = TRUE)
rerun_lca <- function(easylca, models_and_starts = NULL, recursive = FALSE,
                      vlmrt = FALSE){
  if(! is_mplus_installed()){stop('Please make sure that Mplus is installed on this computer. It cannot be detected by the MplusAutomation package.')}
  if(! methods::is(easylca, 'easylca')){
    stop('Please make sure to supply an object of type "easylca" as input to the rerun_lca function. \n"easylca" objects can be obtained through the perform_lca command.')
  }
  if (is.null(models_and_starts)) {
    models_and_starts <- create_models_and_starts_for_rerun(easylca)
  }
  check_assertions_models_and_starts(easylca, models_and_starts)
  print_rerun(models_and_starts)

  counter <- 1
  easylca$settings$vlmrt_last_run <- vlmrt
  easylca$settings <- update_settings_for_higher_classnumbers(easylca$settings,
                                                              models_and_starts)


  easylca <- rerun_specified_models(easylca, models_and_starts)
  if(recursive & (! all(easylca$summary$replicated) | any(is.na(easylca$summary$Parameters)))){ #TODO check logic
    while(counter < 10 ){
      counter <- counter + 1
      message(paste0('Still not all replicated / converged. Rerunning again ',
                     counter, '. time... (max. 10 times)'))
      models_and_starts <- create_models_and_starts_for_rerun(easylca)
      print_rerun(models_and_starts)
      easylca <- rerun_specified_models(easylca, models_and_starts)
      settings <- easylca$settings
      saveRDS(easylca, paste0(settings$folder_name, '/',
                              settings$analysis_name, '_lca_results_rerun-no-',
                              counter, '.rds'))

      if(all(easylca$summary$replicated) & ! any(is.na(easylca$summary$Parameters))){break}
    }
  }

  message('Done.')
  return(easylca)
}

create_models_and_starts_for_rerun <- function(easylca){
  starts <- easylca$settings$starts
  easylca$summary %>% dplyr::filter(! .data$replicated | is.na(.data$Parameters)) %>%
    dplyr::select('classes', 'modeltype') %>%
    dplyr::mutate(starts = purrr::map2_int(.data$modeltype, .data$classes,
                                           function(modeltype, classes){ starts[[modeltype]][[classes]] * 2}))
}

check_assertions_models_and_starts <- function(easylca, models_and_starts){
  if(! all(c('classes', 'modeltype', 'starts') %in% colnames(models_and_starts))){
    stop('models_and_frame parameter does not contain all neccessary columns for the operation. Please include "classes", "modeltype" and "starts".')
  }
  if(any(! models_and_starts$modeltype %in% c(1,2,3,4,5,6))) {
    stop('Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')
  }
  if(any(models_and_starts$classes %>% floor != models_and_starts$classes) | any(models_and_starts$classes < 1)){
    stop('Please make sure models_and_frame contains only classes with integers > 0.')
  }
}


update_settings_for_higher_classnumbers <- function(settings, models_and_starts){
  settings$nclasses <- max(max(models_and_starts$classes), settings$nclasses)
  settings
}

print_rerun <- function(models_and_starts){
  frameprint <- utils::capture.output(print(models_and_starts))
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

rerun_specified_models <- function(easylca, models_and_starts){
  start_time <- Sys.time()
  easylca$settings <- update_starts_in_settings(easylca$settings, models_and_starts)
  create_templates(easylca$settings)

  for(row in seq(nrow(models_and_starts))){
    rerun_modeltype <- rerun_mplus_lca_single_model(
      easylca,
      easylca$settings,
      models_and_starts$modeltype[row],
      models_and_starts$classes[row]
    )
    name_modeltype <- paste0('modeltype_', models_and_starts$modeltype[row])
    easylca$models[[name_modeltype]] <- rerun_modeltype
  }
  remove_remaining_templates(easylca$settings)
  modeltypes_in_lca <- as.integer(gsub("[^0-9]", "", names(easylca$models)))
  easylca$summary <- create_modeloverview(easylca$models,
                                          easylca$settings,
                                          modeltypes_in_lca)

  end_time <- Sys.time()
  print_elapsed_time(start_time, end_time)
  easylca
}


rerun_mplus_lca_single_model <- function(easylca, settings, modeltype, class){
  setwd(settings$folder_name)
  type_folder <- sprintf('modeltype_%d', modeltype)
  template_file <- sprintf('modeltype_%d_template.txt', modeltype)

  if (!dir.exists(type_folder)) {dir.create(type_folder)}
  file.copy(template_file, paste0(type_folder, '/', template_file),
            overwrite = TRUE)

  datafile <- paste0(type_folder, '/', type_folder, '.dat')
  if (! file.exists(datafile)){
    MplusAutomation::prepareMplusData(settings$frame, datafile)
  }
  MplusAutomation::createModels(paste0(type_folder, '/', template_file))
  MplusAutomation::runModels(sprintf('%s/%.2d_classes.inp', type_folder, class),
                             logFile = paste0(type_folder, '/', type_folder,'_log.txt'),
                             showOutput = FALSE, quiet= FALSE)
  mplus_results <- MplusAutomation::readModels(type_folder, recursive = TRUE)
  mplus_results <- make_list_if_only_one_model(mplus_results, modeltype)
  models_for_type <- easylca$models[[sprintf('modeltype_%d', modeltype)]]
  models_for_type <- utils::modifyList(models_for_type, mplus_results)
  saveRDS(models_for_type,paste0(type_folder, '/', type_folder, '.rds'))
  setwd('..')
  return(models_for_type)
}
