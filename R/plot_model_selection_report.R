create_modeloverview_table <- function(overview, n_min = 100){
  overview_selection <- select_overview_table_columns(overview)
  overview_selection$loc_min_bic <- get_local_minima(overview_selection, 'BIC')
  overview_selection$loc_min_sabic <- get_local_minima(overview_selection, 'saBIC')
  overview_selection$is_vlmrt <- get_vlmrt(overview_selection, 'p VLMRT')
  overview_selection$is_adj_vlmrt <- get_vlmrt(overview_selection, 'p adj. VLMRT')
  dont_include <- c('loc_min_bic', 'loc_min_sabic', 'is_vlmrt', 'is_adj_vlmrt')

  flextable::flextable(overview_selection,
                       col_keys = colnames(overview_selection)[! colnames(overview_selection) %in% dont_include]) %>%
    flextable::bold(part='header') %>%
    flextable::align(align='center', part = 'all') %>%
    flextable::align(j = 1, align = 'left', part = 'all') %>%
    flextable::border_inner(part = 'body') %>%
    flextable::bg(bg = 'white', part = 'all') %>%
    flextable::bg(j = 'BIC',
                  bg = ifelse(overview_selection$loc_min_bic,
                              '#adebad', 'white')) %>%
    flextable::bg(j = 'saBIC',
                  bg = ifelse(overview_selection$loc_min_sabic,
                              '#adebad', 'white')) %>%
    flextable::bg(j = 'p VLMRT',
                  bg = ifelse(overview_selection$`p VLMRT` == 'not calculated' |
                                is.na(overview_selection$`p VLMRT`),
                              'grey',
                              ifelse(overview_selection$`p VLMRT` > 0.05, '#ff9999',
                                     ifelse(overview_selection$is_vlmrt,
                                            '#adebad', 'white')))) %>%
    flextable::bg(j = 'p adj. VLMRT',
                  bg = ifelse(overview_selection$`p adj. VLMRT` == 'not calculated' |
                                is.na(overview_selection$`p adj. VLMRT`),
                              'grey',
                              ifelse(overview_selection$`p adj. VLMRT` > 0.05, '#ff9999',
                                     ifelse(overview_selection$is_adj_vlmrt,
                                            '#adebad', 'white')))) %>%
    flextable::bg(j = 'Entropy',
                  bg = ifelse(is.na(overview_selection$Entropy),
                              'grey', 'white')) %>%
    flextable::bg(j = 'Replicated',
                  bg = ifelse(overview_selection$Replicated, 'white', '#ff9999')) %>%
    flextable::bg(j = 'Boundary Values',
                  bg = ifelse(overview_selection$`Boundary Values`, '#ff9999', 'white')) %>%
    flextable::bg(j = 'n Min', bg = ifelse(overview_selection$`n Min` < n_min,
                                           '#ff9999', 'white')) %>%
    flextable::merge_v(c('Replicated', 'Boundary Values', 'Type')) %>%
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

get_local_minima <- function(data, variable){
  res <- data %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::mutate(loc_min = is_local_minimum(.data[[variable]])) %>%
    dplyr::pull(.data$loc_min)
  if(all(!res)){
    res <- data[[variable]] == min(data[[variable]], na.rm = TRUE)
    res <- ifelse(is.na(res), FALSE, res)
  }
  res
}

is_local_minimum <- function(numeric_vector){
  !is.na(numeric_vector) &
    c(FALSE, diff(numeric_vector) < 0) &
    c(FALSE, diff(rev(numeric_vector)) < 0) %>% rev()
}

get_vlmrt <- function(data, variable){
  res <- data %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::mutate(is_vlmrt = is_vlmrt(.data[[variable]])) %>%
    dplyr::pull(.data$is_vlmrt)
  res
}

is_vlmrt <- function(pvals){
  res <- rep(FALSE, length(pvals))
  if('not calculated' %in% pvals){
    pvals <- ifelse(pvals == 'not calculated', NA, pvals)
  }
  if(all(is.na(pvals))){return(res)}
  last_significant <- which(pvals < 0.05) %>% max()
  if(length(last_significant) > 0){
    res[last_significant] <- TRUE
  }
  res
}

plot_ic_trajectory <- function(data, measure = 'BIC'){
  not_replicated <- data %>%
    dplyr::filter(! .data$replicated)
  max_ic_value <- max(data[[measure]])

  plot <- suppressWarnings(
    ggplot2::ggplot(data, ggplot2::aes(x = .data$classes,
                                     y = .data[[measure]],
                                     color = as.factor(.data$modeltype))) +
    ggplot2::geom_line(linewidth = 1, alpha = 0.8) +
    ggplot2::geom_point(data = not_replicated, color = 'red', size = 2) +
    ggplot2::scale_color_discrete(name = 'type') +
    ggplot2::scale_y_continuous(labels = scales::comma_format(big.mark = ','))+
    ggplot2::scale_x_continuous(breaks = seq(max(data$classes))) +
    ggplot2::labs(x = '# of classes',
                  y = measure) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_text(color = 'grey45'),
                   axis.text.y = ggplot2::element_text(color = 'grey45'),
                   axis.text.x = ggplot2::element_text(face = 'bold'),
                   legend.title = ggplot2::element_text(color = 'grey45'),
                   legend.text = ggplot2::element_text(color = 'grey45'),
                   legend.position = 'top',
                   legend.justification = 'left')
  )

  if(nrow(not_replicated) > 0){
    plot <- plot +
      ggplot2::geom_point(data = data.frame(x = 2, y = max_ic_value),
                          ggplot2::aes(x = .data$x, y = .data$y),
                          color = 'red', size = 2) +
      ggplot2::annotate(geom = 'text', x = 2, y = max_ic_value, label = '= not replicated',
                        color = 'red', size = 4, hjust = -0.1)
  }
  plot
}
