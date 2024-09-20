create_modeloverview <- function(models, settings, modeltypes){
  message('Creating model summary...')
  modeloverview <- data.frame()

  for(type in modeltypes){
    typename <- paste0('modeltype_', type)
    general_info <- models[[typename]] %>%
      sapply(function(model_per_class) {model_per_class[['summaries']]}) %>%
      purrr::map_dfr(~ .x) %>%
      dplyr::select(Title,Parameters,LL,AIC,AICC,BIC,saBIC=aBIC, dplyr::any_of(c('Entropy', 'T11_VLMR_PValue','T11_LMR_PValue'))) %>%
      as.data.frame() %>%
      dplyr::mutate(classes = dplyr::row_number(), .before=1)

    if(! 'Entropy' %in% colnames(general_info)){
      general_info$Entropy <- NA
    }

    nmin <- suppressWarnings(models[[typename]] %>%
                               sapply(function(model_per_class) {model_per_class[['class_counts']][['modelEstimated']][['count']]}) %>%
                               lapply(min) %>%
                               unlist)

    replicated <- models[[typename]] %>%
      sapply(function(model_per_class) {list(model_per_class[['warnings']])}) %>%
      lapply(unlist) %>% as.character() %>%
      stringr::str_detect(pattern = 'NOT REPLICATED', negate = T)

    boundary_values <- models[[typename]] %>%
      sapply(function(model_per_class) {list(model_per_class[['output']])}) %>%
      lapply(unlist) %>% as.character() %>%
      stringr::str_detect(pattern = 'THRESHOLDS APPROACHED EXTREME VALUES')

    overview <- general_info %>%
      dplyr::mutate(nmin = nmin,
                    replicated = replicated,
                    boundary_values = boundary_values,
                    modeltype = type)

    modeloverview <- rbind(modeloverview, overview)
  }

  print(modeloverview)
  return(modeloverview)
}
