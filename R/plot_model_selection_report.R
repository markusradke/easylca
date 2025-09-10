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
                  bg = ifelse(overview_selection$`p VLMRT` == 'not calculated' |
                                is.na(overview_selection$`p VLMRT`),
                              'grey',
                              ifelse(overview_selection$`p VLMRT` > 0.05, '#ff9999', 'white'))) %>%
    flextable::bg(j = 'p adj. VLMRT',
                  bg = ifelse(overview_selection$`p adj. VLMRT` == 'not calculated' |
                                is.na(overview_selection$`p adj. VLMRT`),
                              'grey',
                              ifelse(overview_selection$`p adj. VLMRT` > 0.05, '#ff9999', 'white'))) %>%
    flextable::bg(j = 'Entropy',
                  bg = ifelse(is.na(overview_selection$Entropy),
                              'grey', 'white')) %>%
    flextable::bg(j = 'Replicated',
                  bg = ifelse(overview_selection$Replicated, 'white', '#ff9999')) %>%
    flextable::bg(j = 'Boundary Values',
                  bg = ifelse(overview_selection$`Boundary Values`, '#ff9999', 'white')) %>%
    flextable::bg(j = 'n Min', bg = ifelse(overview_selection$`n Min` < 100,
                                           '#ff9999', 'white')) %>%
    flextable::merge_v(colnames(overview_selection)[! colnames(overview_selection) %in%
                                                      c('p VLMRT', 'p adj. VLMRT')]) %>%
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
