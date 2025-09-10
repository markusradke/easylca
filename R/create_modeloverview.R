create_modeloverview <- function(models, settings, modeltypes){
  message('Creating model summary...')
  modeloverview <- data.frame()

  for(type in modeltypes){
    typename <- paste0('modeltype_', type)
    summary <- get_mplus_summary_for_modeltype(models, typename)
    nmin <- get_nmin_for_modeltype(models, typename)
    replicated <- get_is_replicated_for_modeltype(models, typename)
    boundary_values <- get_are_boundary_values_for_modeltype(models, typename)

    overview <- summary %>%
      dplyr::mutate(nmin = nmin,
                    replicated = replicated,
                    boundary_values = boundary_values,
                    modeltype = type)

    modeloverview <- rbind(modeloverview, overview)
  }
  return(modeloverview)
}

get_mplus_summary_for_modeltype <- function(models, typename){
  summary <- models[[typename]] %>%
    lapply(function(model_per_class) {model_per_class[['summaries']]}) %>%
    purrr::map_dfr(~ .x) %>%
    dplyr::select('Title', 'Parameters', 'LL', 'AIC',
                  'AICC', 'BIC', saBIC = 'aBIC',
                  dplyr::any_of(c('Entropy', 'T11_VLMR_PValue','T11_LMR_PValue'))) %>%
    as.data.frame() %>%
    dplyr::mutate(classes = dplyr::row_number(), .before=1)
  if(! 'Entropy' %in% colnames(summary)){
    summary$Entropy <- NA
  }
  return(summary)
}

get_nmin_for_modeltype <- function(models, typename){
  suppressWarnings(models[[typename]] %>%
                     sapply(function(model_per_class){
                       model_per_class[['class_counts']][['modelEstimated']][['count']]
                     }) %>%
                     lapply(min) %>%
                     unlist)
}

get_is_replicated_for_modeltype <- function(models, typename){
  models[[typename]] %>%
    sapply(function(model_per_class) {list(model_per_class[['warnings']])}) %>%
    lapply(unlist) %>% as.character() %>%
    stringr::str_detect(pattern = 'NOT REPLICATED', negate = T)
}

get_are_boundary_values_for_modeltype <- function(models, typename){
  models[[typename]] %>%
    sapply(function(model_per_class) {list(model_per_class[['output']])}) %>%
    lapply(unlist) %>% as.character() %>%
    stringr::str_detect(pattern = 'THRESHOLDS APPROACHED EXTREME VALUES')
}
