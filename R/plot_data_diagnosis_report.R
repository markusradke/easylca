get_missings_frequencies <- function(data, idcol){
  missings <- sapply(data %>% dplyr::select(-dplyr::all_of(idcol)),
         function(x) 1 - length(stats::na.omit(x)) / length(x))
  missings <- data.frame(var = names(missings),
                         relfreq_na = missings) %>%
    dplyr::arrange(dplyr::desc(.data$relfreq_na)) %>%
    dplyr::filter(.data$relfreq_na != 0) %>%
    dplyr::mutate(label = sprintf('%.0f%%', .data$relfreq_na * 100))
}

get_pivoted_variable_selection <- function(data, selection){
  data %>% dplyr::select(dplyr::all_of(selection)) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "variable", values_to = "value")
}

get_medians_for_all_variables <- function(data){
  data %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarize(median = stats::median(.data$value, na.rm = TRUE))
}

get_modus_for_all_variables <- function(data){
  data %>%
    dplyr::count(.data$variable, .data$value) %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::slice_max(n = 1, order_by = .data$n) %>%
    dplyr::select('variable', 'value') %>%
    dplyr::mutate(modus = TRUE)
}

get_iqrs_for_all_variables <- function(data){
  data %>% dplyr::group_by(.data$variable) %>%
    dplyr::summarize(iqr = stats::IQR(.data$value, na.rm = T) * 1.5) %>%  # equals variance for Gaussian
    dplyr::arrange(-.data$iqr)
}

plot_missings_frequencies <- function(missings){
  ggplot2::ggplot(missings, ggplot2::aes(y = forcats::fct_rev(forcats::fct_inorder(.data$var)),
                                         x = .data$relfreq_na)) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$relfreq_na), show.legend = F) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.025)),
                                limits = c(0,1), position = 'top')+
    ggplot2::scale_fill_gradientn(colors = c("grey85", "#c40d20"),
                                  values = scales::rescale(c(0, 0.4))) +
    ggplot2::xlab('relative frequency of NAs') +
    ggplot2::ylab('')+
    ggplot2::geom_text(ggplot2::aes(label = .data$label), hjust = 0,
                       nudge_x = 0.01) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = 'none',
                   panel.grid = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(color = 'grey45',
                                                        size = 14, hjust = 0),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 14, face = 'italic'))
}

plot_spearman_correlations <- function(data, continuous){
  cor_matrix <- stats::cor(data %>% dplyr::select(dplyr::all_of(continuous)),
                           use = 'pairwise.complete.obs',
                           method = "spearman")
  cor_df <- as.data.frame(cor_matrix)
  cor_long <- cor_df %>%
    tibble::rownames_to_column(var = 'Var1') %>%
    tidyr::pivot_longer(cols = -'Var1', names_to = 'Var2',values_to = 'value') %>%
    dplyr::mutate(lower_triangular = match(.data$Var1, unique(.data$Var1)) >
                    match(.data$Var2, unique(.data$Var2)),
                  label = sprintf('%.2f', .data$value),
                  Var1 = forcats::fct_inorder(.data$Var1),
                  Var2 = forcats::fct_inorder(.data$Var2))

  ggplot2::ggplot(cor_long, ggplot2::aes(.data$Var1, .data$Var2,
                                         fill = ifelse(.data$lower_triangular,
                                                       NA, .data$value))) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_x_discrete(position = 'top') +
    ggplot2::scale_fill_gradient(low = "white", high = "#c40d20",na.value = "white") +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(.data$lower_triangular, '', .data$label)),
                       color = 'grey25')+
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 14,
                                                       angle = 45, hjust = 0),
                   axis.text.y = ggplot2::element_text(size = 14),
                   legend.position = 'none')
}

plot_continuous_histograms <- function(data, continuous, ncol){
  continuous_data <- get_pivoted_variable_selection(data, continuous)
  medians <- get_medians_for_all_variables(continuous_data)

  n_bins <- 80
  ggplot2::ggplot(continuous_data,
                  ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(fill = 'grey45', bins = n_bins, na.rm = T) +
    ggplot2::geom_vline(data = medians,
                        ggplot2::aes(xintercept = .data$median),
                        color = '#c40d20', linewidth = 0.75) +
    ggplot2::facet_wrap(~ .data$variable,
                        ncol = ncol,
                        scales = 'free')+
    ggplot2::theme_minimal() +
    ggplot2::labs(y = 'frequency',
                  subtitle = 'median') +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = 'bold',
                                                       hjust = 0),
                    plot.subtitle = ggplot2::element_text(size = 10,
                                                          color = '#c40d20'),
                    axis.title = ggplot2::element_text(color = 'grey45'),
                    axis.text = ggplot2::element_text(color = 'grey45'),
                    panel.grid = ggplot2::element_blank())
}

plot_continuous_iqrs <- function(data, continuous){
  continuous_data <- get_pivoted_variable_selection(data, continuous)
  iqrs <- get_iqrs_for_all_variables(continuous_data)
  ggplot2::ggplot(iqrs, ggplot2::aes(x = .data$iqr,
                                     y = forcats::fct_rev(forcats::fct_inorder(.data$variable)))) +
    ggplot2::geom_col(fill = 'grey45') +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.0,
                                                                     0.05)),
                                position = 'top')+
    ggplot2::xlab('Interquartile range (IQR) x 1.5') +
    ggplot2::ylab('')+
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(color = 'grey45'),
                   panel.grid = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(color = 'grey45',
                                                        hjust = 0,
                                                        size = 14),
                   axis.text.y = ggplot2::element_text(size = 14, face = 'italic'))
}

plot_discrete_histograms <- function(data, discrete, ncol){
  discrete_data <- get_pivoted_variable_selection(data, discrete)
  modus <- get_modus_for_all_variables(discrete_data)

  discrete_data <- suppressMessages(dplyr::left_join(discrete_data, modus)) %>%
    dplyr::mutate(modus = ifelse(is.na(.data$modus), FALSE, modus))

  ggplot2::ggplot(discrete_data,
                  ggplot2::aes(y = as.factor(.data$value),
                               fill = .data$modus)) +
    ggplot2::geom_bar(stat = 'count',
                      na.rm = T) +
    ggplot2::scale_fill_manual(values = c('grey45', '#c40d20')) +
    ggplot2::labs(x = 'frequency', y = '',
                  subtitle = 'modus') +
    ggplot2::facet_wrap(~ variable,
                        ncol = ncol,
                        scales = 'free_y') +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 12, face = 'bold',
                                                       hjust = 0),
                    plot.subtitle = ggplot2::element_text(size = 10,
                                                          color = '#c40d20'),
                    legend.position = 'none',
                    axis.title = ggplot2::element_text(color = 'grey45'),
                    axis.text.y = ggplot2::element_text(size =  12,
                                                        color = 'black'),
                    axis.text.x = ggplot2::element_text(color = 'grey45'),
                    panel.grid = ggplot2::element_blank())
}
