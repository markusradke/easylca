#' BETA VERSION: Read Already Existing Models that match the given settings
#'
#' Reads currently existing models according to given settings and creates figures for them.
#' #TODO: Read out start values and update them in the settings object.
#'
#' @param settings LCA settings object
#'
#' @return LCA object
#' @export
#'
#' @examples # no example possible, cause no folder structure available
read_models <- function(settings){
  results <- list()
  results$settings <- settings
  models <- list()
  modeltypes <- seq(6) # TODO
  for(type in modeltypes){
    model_for_type <- read_modeltype(settings, modeltype = type)
    models[[paste0('modeltype_', type)]] <- model_for_type
  }
  models <<- models
  results$models <- models
  results$summary <- create_modeloverview(results$models, settings, modeltypes)

  class(results) <- 'easylca'
  saveRDS(results, paste0(settings$folder_name, '/', settings$analysis_name, '_lca_results.rds'))
  message('Done.')
  results
}


read_modeltype <- function(settings, modeltype){
  setwd(settings$folder_name)
  analysis <- paste0(settings$analysis_name, '_model', modeltype)
  type_folder <- paste0(analysis, '_lca')
  message(paste0('Reading type ', modeltype, ' from folder ', type_folder, '...'))

  mplus_results<- MplusAutomation::readModels(type_folder,recursive=T)
  saveRDS(mplus_results,paste0(analysis,"_lca/",analysis,".rds"))

  setwd('..')
  return(mplus_results)
}
