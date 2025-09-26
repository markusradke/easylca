#' Perform LCA
#'
#' Performs the LCA with the settings specified via the [define_lca()] command for all given combinations of model types and classes. If wished for, a likelihood ratio test is performed for later model selection.
#' There are 6 modeltypes that can be estimated :
#' 1. Equal variances across classes, and covariances fixed to
#' 2. Varying variances across classes and covariances fixed to 0
#' 3. Equal variances and equal covariances accross classes
#' 4. Varying variances and equal covariances across classes
#' 5. Equal variances and varying covariances across classes
#' 6. Varying variances and varying covariances across classes
#'
#'
#' Inspect the results with [generate_model_selection_report()], [generate_model_report()], and [get_prediction_for_model()]. Rerun the model if not replicated with [rerun_lca()].
#'
#' @param settings Settings for the lca, including data, variable specification, and additional technical specifications. Please use the [define_lca()] command to generate the settings and refer to its documentation for further details.
#' @param modeltypes Modeltypes to perform the LCA for. Defaults to all 6 model types.
#' @param vlmrt Logical indicating wether to perform a Likelihood Ratio Test for class enumaration. The test was proposed by Lo, Mendell, & Rubin (2001) based on work by Vuong (1989). Attention: Takes a lot of time to perform.
#'
#' @return easylca object that contains settings, models, and plots
#' @export
#'
#' @seealso For a more detailed explation on the different modeltypes we recommend to visit \url{https://data-edu.github.io/tidyLPA/articles/Introduction_to_tidyLPA.html}.
#' @examples
#' # for more model types, add them to the modeltype vector, take a while to compute
#' # lca <- perform_lca(titanic_settings, modeltype = c(1, 2, 3))
perform_lca <- function(settings, modeltypes = seq(6), vlmrt = FALSE) {
  if (!is_mplus_installed()) {
    stop(
      'Please make sure that Mplus is installed on this computer. It cannot be detected by the MplusAutomation package.'
    )
  }
  if (!methods::is(settings, 'lca_settings')) {
    stop(
      'Please provide a settings object of type "lca_settings". It can be generated using the define_lca command.'
    )
  }
  settings$vlmrt_last_run <- vlmrt
  modeltypes <- set_modeltypes_1_if_only_discrete(settings, modeltypes)
  create_templates(settings)

  results <- list()
  results$settings <- settings

  start_time <- Sys.time()

  models <- list()
  for (type in modeltypes) {
    model_for_type <- mplus_lca(settings, modeltype = type)
    models[[paste0('modeltype_', type)]] <- model_for_type
  }
  remove_remaining_templates(settings)
  results$models <- models
  results$summary <- create_modeloverview(results$models, settings, modeltypes)

  class(results) <- 'easylca'
  saveRDS(
    results,
    paste0(
      settings$folder_name,
      '/',
      settings$analysis_name,
      '_lca_results.rds'
    )
  )

  end_time <- Sys.time()
  print_elapsed_time(start_time, end_time)

  message('Done.')
  results
}


set_modeltypes_1_if_only_discrete <- function(settings, modeltypes) {
  if (all(modeltypes == 1)) {
    return(modeltypes)
  }
  is_only_discrete <- setequal(
    settings$use,
    union(settings$categorical, settings$nominal)
  )
  if (is_only_discrete) {
    modeltypes <- 1
    warning(
      'Only discret indicators in settings. Thus, only modeltype 1 will be estimated.'
    )
  }
  return(modeltypes)
}

mplus_lca <- function(settings, modeltype) {
  setwd(settings$folder_name)
  type_folder <- sprintf('modeltype_%d', modeltype)
  template_file <- sprintf('modeltype_%d_template.txt', modeltype)

  if (!dir.exists(type_folder)) {
    dir.create(type_folder)
  }
  list.files(type_folder, include.dirs = F, full.names = T) %>% file.remove()
  file.copy(template_file, paste0(type_folder, '/', template_file))
  file.remove(template_file)

  datafile <- paste0(type_folder, '/', type_folder, '.dat')
  MplusAutomation::prepareMplusData(settings$frame, datafile)
  MplusAutomation::createModels(paste0(type_folder, '/', template_file))
  MplusAutomation::runModels(
    type_folder,
    logFile = paste0(type_folder, '/', type_folder, '_log.txt'),
    showOutput = FALSE,
    quiet = FALSE
  )
  mplus_results <- MplusAutomation::readModels(type_folder, recursive = TRUE)
  saveRDS(mplus_results, paste0(type_folder, '/', type_folder, '.rds'))

  setwd('..')
  return(mplus_results)
}
