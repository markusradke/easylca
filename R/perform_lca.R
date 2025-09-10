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
perform_lca <- function(settings, modeltypes = seq(6), vlmrt = FALSE){
  if(! is_mplus_installed()){stop('Please make sure that Mplus is installed on this computer. It cannot be detected by the MplusAutomation package.')}
  if(! methods::is(settings, 'lca_settings')) {stop('Please provide a settings object of type "lca_settings". It can be generated using the define_lca command.')}
  settings$vlmrt_last_run <- vlmrt
  create_templates(settings)

  results <- list()
  results$settings <- settings

  start_time <- Sys.time()

  models <- list()
  for(type in modeltypes){
    model_for_type <- mplus_lca(settings, modeltype = type)
    models[[paste0('modeltype_', type)]] <- model_for_type
  }
  results$models <- models

  results$summary <- create_modeloverview(results$models, settings, modeltypes)

  class(results) <- 'easylca'
  saveRDS(results, paste0(settings$folder_name, '/', settings$analysis_name, '_lca_results.rds'))

  end_time <- Sys.time()
  print_elapsed_time(start_time, end_time)

  message('Done.')
  results
}


mplus_lca <- function(settings, modeltype){
  setwd(settings$folder_name)
  analysis <- paste0(settings$analysis_name, '_model', modeltype)
  type_folder <- paste0(analysis, '_lca')

  if (!dir.exists(type_folder)) {dir.create(type_folder)}
  list.files(type_folder,include.dirs = F, full.names = T) %>% file.remove
  file.copy(paste0(analysis,"_template.txt"),paste0(analysis,"_lca/"))

  MplusAutomation::prepareMplusData(settings$frame,paste0(analysis,"_lca/",analysis,"_lca.dat"))
  MplusAutomation::createModels(paste0(analysis,"_lca/",analysis,"_template.txt"))
  MplusAutomation::runModels(type_folder,logFile=paste0(analysis,"_lca/", analysis,"_log.txt"),showOutput=F,quiet=F)
  mplus_results<- MplusAutomation::readModels(type_folder,recursive=T)
  saveRDS(mplus_results,paste0(analysis,"_lca/",analysis,".rds"))

  setwd('..')
  return(mplus_results)
}


