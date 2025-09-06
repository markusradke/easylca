create_modeloverview <- function(models, settings, modeltypes){
  message('Creating model summary...')
  modeloverview <- data.frame()

  for(type in modeltypes){
    typename <- paste0('modeltype_', type)
    general_info <- models[[typename]] %>%
      sapply(function(model_per_class) {model_per_class[['summaries']]}) %>%
      purrr::map_dfr(~ .x) %>%
      dplyr::select(.data$Title, .data$Parameters, .data$LL, .data$AIC,
                    .data$AICC, .data$BIC, saBIC = .data$aBIC,
                    dplyr::any_of(c('Entropy', 'T11_VLMR_PValue','T11_LMR_PValue'))) %>%
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

create_modeloverview_table <- function(overview){
  overview_selection <- overview %>%
    dplyr::mutate(nmin = round(.data$nmin)) %>%
    dplyr::select(Type = .data$modeltype,
                  Classes = .data$classes,
                  .data$Parameters,
                  Replicated = .data$replicated,
                  'Boundary Values' = .data$boundary_values,
                  'n Min'= .data$nmin,
                  .data$Entropy,
                  .data$LL,
                  .data$AIC,
                  .data$AICC,
                  .data$BIC,
                  .data$saBIC,
                  .data$VLRMT)

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
    flextable::bg(j = 'VLRMT',
                  bg = ifelse(overview_selection$VLRMT == 'not calculated',
                              'grey', 'white')) %>%
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
