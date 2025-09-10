#' BETA VERSION: Read Already Existing Models that match the given settings
#'
#' Reads currently existing models according to given settings. In case something went wrong during training and a results file could not be written.
#'
#' #TODO: Read out start values and update them in the settings object.
#'
#' @param settings LCA settings object
#'
#' @return LCA object
#' @export
#'
#' @examples # settings <- random_testresults$settings
#' # perform_lca(settings, modeltypes = c(1, 3)
#' # lca <- read_models(settings)
read_models <- function(settings){
  assert_is_settings(settings)
  assert_is_folder(settings)
  modeltypes <- get_estimated_modeltypes(settings)
  lca <- read_modeltypes(settings, modeltypes)
  return(lca)
}

assert_is_settings <- function(input){
  is_settings <- methods::is(input, 'lca_settings')
  if(! is_settings) {stop('Please provide an easylca_settings_object as input.')}
}

assert_is_folder <- function(settings){
  is_folder <- file.exists(settings$folder_name)
  if(! is_folder) {
    stop('Could not find corresponding folder in current subdirectory for the settings you provided.')
  }
}

get_estimated_modeltypes <- function(settings){
  directories <- list.dirs(settings$folder_name, recursive = FALSE)
  pattern <-paste0('(?<=', settings$folder_name,
                   '/modeltype_)[1-6]')
  types <- stringr::str_extract_all(directories, pattern) %>%
    unlist() %>% as.integer()
  if(length(types) == 0){
    stop('No models were found inside the corresponding folder for given settings.')
  }
  return(types)
}


read_modeltypes <- function(settings, modeltypes){
  results <- list()
  results$settings <- settings
  models <- list()
  for(type in modeltypes){
    model_for_type <- read_single_modeltype(settings, modeltype = type)
    models[[paste0('modeltype_', type)]] <- model_for_type
  }
  results$models <- models
  results$summary <- create_modeloverview(results$models, settings, modeltypes)

  class(results) <- 'easylca'
  saveRDS(results, paste0(settings$folder_name, '/', settings$analysis_name, '_lca_results.rds'))
  message('Done.')
  return(results)
}

read_single_modeltype <- function(settings, modeltype){
  setwd(settings$folder_name)
  type_folder <- sprintf('modeltype_%d', modeltype)
  message(paste0('Reading model type ', modeltype, '...'))
  mplus_results<- MplusAutomation::readModels(type_folder, recursive=T)

  if('summaries' %in% names(mplus_results)) { # when only one class is found
    temp <- mplus_results
    mplus_results <- list()
    mplus_results[[paste0(temp$summaries$NLatentClass, '_classes')]] = temp
  } else {
    new_names <- lapply(mplus_results, function(model){
      paste0(model$summaries$NLatentClass, '_classes')
    }) %>% unlist()
    names(mplus_results) <- new_names
  }
  saveRDS(mplus_results,paste0(type_folder, '/', type_folder, '.rds'))
  setwd('..')
  return(mplus_results)
}
