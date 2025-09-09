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

create_modeloverview_table <- function(overview){
  overview_selection <- select_overview_table_columns(overview)

  flextable::flextable(overview_selection) %>%
    flextable::bold(part='header') %>%
    flextable::align(align='center', part = 'all') %>%
    flextable::align(j = 1, align = 'left', part = 'all') %>%
    flextable::border_inner(part = 'body') %>%
    flextable::bg(bg = 'white', part = 'all') %>%
    flextable::bg(j = 'LL',
                  bg = ifelse(overview_selection$LL == max(overview_selection$LL),
                              '#adebad', 'white')) %>%
    flextable::bg(j = 'AIC',
                  bg = ifelse(overview_selection$AIC == min(overview_selection$AIC),
                              '#adebad', 'white')) %>%
    flextable::bg(j = 'AICC',
                  bg = ifelse(overview_selection$AICC == min(overview_selection$AICC),
                              '#adebad', 'white')) %>%
    flextable::bg(j = 'BIC',
                  bg = ifelse(overview_selection$BIC == min(overview_selection$BIC),
                              '#adebad', 'white')) %>%
    flextable::bg(j = 'saBIC',
                  bg = ifelse(overview_selection$saBIC == min(overview_selection$saBIC),
                              '#adebad', 'white')) %>%
    flextable::bg(j = 'p VLMRT',
                  bg = ifelse(overview_selection$`p VLMRT` == 'not calculated',
                              'grey',
                              ifelse(overview_selection$`p VLMRT` > 0.05, '#adebad', 'white'))) %>%
    flextable::bg(j = 'p adj. VLMRT',
                  bg = ifelse(overview_selection$`p adj. VLMRT` == 'not calculated',
                              'grey',
                              ifelse(overview_selection$`p adj. VLMRT` > 0.05, '#adebad', 'white'))) %>%
    flextable::bg(j = 'Entropy',
                  bg = ifelse(is.na(overview_selection$Entropy),
                              'grey', 'white')) %>%
    flextable::bg(j = 'Replicated',
                  bg = ifelse(overview_selection$Replicated, 'white', '#ff9999')) %>%
    flextable::bg(j = 'Boundary Values',
                  bg = ifelse(overview_selection$`Boundary Values`, '#ff9999', 'white')) %>%
    flextable::bg(j = 'n Min', bg = ifelse(overview_selection$`n Min` < 100,
                                           '#ff9999', 'white')) %>%
    flextable::merge_v() %>%
    flextable::autofit()
}

select_overview_table_columns <- function(overview){
  overview %>%
    dplyr::mutate(nmin = round(.data$nmin)) %>%
    dplyr::select(Type = 'modeltype',
                  Classes = 'classes',
                  .data$Parameters,
                  Replicated = 'replicated',
                  'Boundary Values' = 'boundary_values',
                  'n Min'= 'nmin',
                  'Entropy',
                  'LL',
                  'AIC',
                  'AICC',
                  'BIC',
                  'saBIC',
                  'p VLMRT' = 'T11_VLMR_PValue',
                  'p adj. VLMRT' = 'T11_LMR_PValue')
}
