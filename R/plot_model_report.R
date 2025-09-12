plot_class_prevalences <- function(model){
  class_counts <- model$class_counts$modelEstimated %>%
    dplyr::arrange(.data$count) %>%
    dplyr::mutate(count = round(.data$count),
                  greater100 = ifelse(.data$count > 100, T, F))
  class_colors <- discrete_colors_for_classes[class_counts$class] # internal from package
  class_counts_formatted <- class_counts %>%
    dplyr::mutate(
      class = forcats::fct_inorder(sprintf('class %d', .data$class)),
      proportion = sprintf('%.2f %%', .data$proportion * 100)
    )

  ggplot2::ggplot(class_counts_formatted, ggplot2::aes(x = .data$count,
                                             y = .data$class,
                                             fill = .data$class))+
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = 100, color = 'black',
                        size = 1) +
    ggplot2::annotate('text', x = 100, y = nrow(class_counts_formatted),
                      label = '100', color = 'white',
                      size = 5, hjust = -0.1) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .3)),
                                position = 'top') +
    ggplot2::scale_fill_manual(values = class_colors) +
    ggplot2::geom_text(ggplot2::aes(label = .data$proportion),
                       size = 5, color = 'black', hjust = -0.1) +
    ggplot2::labs(x = 'model estimated count', y = '') +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = 'none',
      axis.title = ggplot2::element_text(color = 'grey45', size = 12, hjust = 0),
      axis.text.x = ggplot2::element_text(color = 'grey45', size = 10, hjust = 0),
      axis.text.y = ggplot2::element_text(size = 14, face = 'bold'),
      panel.grid = ggplot2::element_blank()
    )
}

create_mean_assignement_probabilities_table <- function(model){
  prediction_probabilites <- model$savedata %>%
    dplyr::mutate(cprob = apply(dplyr::select(., dplyr::starts_with('CPROB')),
                                1, max, na.rm = TRUE)) %>%
    dplyr::select(class = 'CLASS', 'cprob')
  mean_assignement_probabilites <- prediction_probabilites %>%
    dplyr::group_by(.data$class) %>%
    dplyr::summarize(mean_prob = mean(.data$cprob)) %>%
    dplyr::arrange(-.data$mean_prob)

  class_colors <- discrete_colors_for_classes[mean_assignement_probabilites$class] # internal from package
  formatted_probabilites <- mean_assignement_probabilites %>%
    dplyr::mutate(
      class = sprintf('class %d', .data$class),
      class = forcats::fct_inorder(.data$class),
      mean_prob = sprintf('%.1f%%', .data$mean_prob * 100))

  flextable::flextable(formatted_probabilites) %>%
    flextable::delete_part('header') %>%
    flextable::border_remove() %>%
    flextable::fontsize(size = 14) %>%
    flextable::bold(j = 2) %>%
    flextable::color(j = 2, color = class_colors) %>%
    flextable::set_table_properties(layout = "autofit",
                                    align = 'left')

}

plot_kruskal_profiles <- function(model){

  .get_kruskal_chisquare_from_prediction_for_item <- function(item){
    are_all_in_same_class <- predicted[['CLASS']] %>% unique() %>% length() == 1
    if(are_all_in_same_class){ return(0) }
    stats::kruskal.test(predicted[[toupper(item)]], predicted[['CLASS']])$statistic[[1]] %>%
      round(2)
  }

  .get_kruskal_p_from_prediction_for_item <- function(item){
    are_all_in_same_class <- predicted[['CLASS']] %>% unique() %>% length() == 1
    if(are_all_in_same_class){ return('') }
    pvalue <- stats::kruskal.test(predicted[[toupper(item)]], predicted[['CLASS']])$p.value
    pvalue <- pvalue * nitems # Bonferroni
    if(pvalue > 0.05){return('')}
    if(pvalue > 0.05){return('*')}
    if(pvalue > 0.01){return('**')}
    return('***')
  }

  .get_kruskal_chisquares <- function(){
    data.frame(item = items) %>%
      dplyr::filter(!stringr::str_detect(.data$item, '#1')) %>%
      dplyr::mutate(
        item = as.character(.data$item),
        chisquare = purrr::map_dbl(.data$item,
                                   .get_kruskal_chisquare_from_prediction_for_item),
        p = purrr::map_chr(.data$item,
                           .get_kruskal_p_from_prediction_for_item)
      ) %>%
      dplyr::arrange(-.data$chisquare)
  }

  predicted <- model$savedata
  if(is.null(predicted[['CLASS']])) {
    warning(paste0('CLASS PREDICTIONS FOR ',
                   model$summaries$Title
                   ,' WERE NOT STORED BY MPLUS. Check Model outputs for any further hints.'))
    predicted[['CLASS']] <- 1
  }
  items <- model$input$variable$usevariables %>% stringr::str_split_1(' ')
  items  <- items[items != '']
  nitems <- items %>% length
  chisquares <- .get_kruskal_chisquares()
  max_chi_square <- chisquares$chisquare %>% max

  total_entropy <- model$summaries$Entropy
  suppressWarnings(
    ggplot2::ggplot(chisquares, ggplot2::aes(y = stats::reorder(.data$item, .data$chisquare),
                                             x = .data$chisquare)) +
      ggplot2::geom_bar(stat = 'identity', fill = 'grey') +
      ggplot2::geom_text(ggplot2::aes(label = .data$p), hjust = -0.1, size = 6, color = 'black') +
      ggplot2::scale_x_continuous(limits = c(0, max_chi_square * 1.5), expand = c(0, 0.05),
                                  position = 'top') +
      ggplot2::labs(subtitle = 'Kruskal-Wallis Chi square', y = '',
                    x = '* p < 0.05, **  p < 0.01, *** p < 0.001, Bonferroni corrected')) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_text(color = 'grey45', size = 10, hjust = 0),
                   panel.grid = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(hjust = 0, size = 12),
                   axis.text.y = ggplot2::element_text(size = 14, face = 'bold'),
                   axis.text.x = ggplot2::element_text(size = 10, color = 'grey45', hjust = 0))
}

plot_continuous_profiles <- function(profiles, ncol_plot=2, show_significance = FALSE){
  profiles <- profiles %>% dplyr::filter(.data$plotgroup == 'continuous')
  nclasses <- profiles$class %>% unique() %>% length()
  n_col_plot <- ifelse(nclasses > 6, 1, 2)
  class_colors <- discrete_colors_for_classes[1:nclasses] # internal from package


  average_means <- suppressMessages(
    profiles %>%
      dplyr::group_by(.data$item) %>%
      dplyr::summarize(average_mean = mean(.data$est)) %>%
      dplyr::right_join(profiles)
  )


  plot <- ggplot2::ggplot(average_means, ggplot2::aes(x = as.factor(.data$class),
                                              y = .data$est,
                                              color = .data$class))+
    ggplot2::facet_wrap(.~.data$item,  scales = "free", ncol = ncol_plot)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower, ymax = .data$upper))+
    ggplot2::geom_point(size = 2)+
    ggplot2::geom_text(ggplot2::aes(y = .data$yposinflation, label = .data$pzero),
                       vjust = 0.2, size = 3, color = 'black')+
    ggplot2::scale_color_manual(values = class_colors)+
    ggplot2::geom_hline(ggplot2::aes(yintercept = .data$average_mean),
                        color = 'grey70', linewidth = 0.6, linetype = 'twodash') +
    ggplot2::geom_text(ggplot2::aes(y =  .data$average_mean),
                       label = 'mean', x = 0, hjust = -1,
                       vjust = 1.05, color = 'grey70') +
    ggplot2::scale_x_discrete(position = 'top') +
    ggplot2::ylab('model estimate') +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10, vjust = 1,
                                                       # color = 'grey45',
                                                       color = class_colors,
                                                       ),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   legend.position = 'none',
                   axis.text.y = ggplot2::element_text(color = 'grey45', size = 10),
                   axis.title.y = ggplot2::element_text(color = 'grey45', size = 12),
                   axis.title.x = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = 'bold', size = 14),
                   strip.placement = 'outside',
                   panel.spacing.x = ggplot2::unit(14, 'points'),
                   panel.spacing.y = ggplot2::unit(36, 'points')
                   )
  if(show_significance & ! all(profiles$pzero == '')){
    plot <- plot +
      ggplot2::labs(subtitle = 'for P(y <= 0): * p < 0.05, ** p < 0.01, *** p < 0.001') +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(color = 'grey45', size = 10))
  }
  suppressWarnings(plot)

}


plot_binary_profiles <- function(profiles, show_significance = FALSE){
  profiles <- dplyr::filter(profiles, .data$plotgroup == 'binary')
  nclasses <- profiles$class %>% unique() %>% length()
  class_colors <- discrete_colors_for_classes[1:nclasses] # internal from package

  relative_freqs <- profiles %>%
    dplyr::filter(.data$param == 'probability' & .data$level==2) %>%
    dplyr::rename(share_of_2 = 'est') %>%
    dplyr::mutate(share_label = paste0(round(.data$share_of_2, 2), .data$significance)) %>%
    dplyr::select('class', 'item', 'share_of_2', 'share_label')

  plot <- ggplot2::ggplot(relative_freqs, ggplot2::aes(x = .data$class,
                                               y = .data$item, fill = .data$class)) +
    ggplot2::geom_tile(ggplot2::aes(alpha = .data$share_of_2), color = "white",
                       show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = .data$share_label), color = "black") +
    ggplot2::scale_alpha_continuous(range = c(0, 0.75)) +
    ggplot2::scale_x_discrete(position = 'top') +
    ggplot2::scale_fill_manual(values = class_colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = '',
                  subtitle = 'P(level 2 | class)',
                  x = '') +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 14, face = 'bold'),
      axis.text.x = ggplot2::element_text(size = 14,
                                          color = class_colors,
                                          # color = 'grey45'
                                          ),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = 'top',
      axis.title.x = ggplot2::element_text(size = 10, color = 'grey45', hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0),
      panel.grid = ggplot2::element_blank()
    )
  if(show_significance){
    plot <- plot +
      ggplot2::labs(x = '* p < 0.05, ** p < 0.01, *** p < 0.001')
  }
  suppressWarnings(plot)
}

plot_discrete_profiles <- function(profiles, show_significance = FALSE){
  profiles <- dplyr::filter(profiles, .data$plotgroup == 'discrete') %>%
    dplyr::mutate(label = sprintf('%.0f%%%s',
                                  round(100 * .data$est),
                                  .data$significance),
                  label_color = ifelse(.data$est < 0.5, 0, 1) %>% as.factor())
  nclasses <- profiles$class %>% unique() %>% length()
  class_colors <- discrete_colors_for_classes[1:nclasses] # internal from package
  max_levels <- max(profiles$level, na.rm = TRUE)
  plot_n_cols <- ifelse(max_levels < 4, 2, 1)

  plot <- ggplot2::ggplot(data = profiles,
                  ggplot2::aes(x = forcats::fct_inorder(as.character(.data$level)),
                               y =  forcats::fct_inorder(as.character(.data$item)),
                               fill = forcats::fct_inorder(as.character(.data$class))))+
    ggplot2::facet_wrap(.~.data$class, ncol = plot_n_cols)+
    ggplot2::geom_tile(ggplot2::aes(alpha = .data$est)) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label, color = .data$label_color),
                       hjust = 0, vjust = 0.5, nudge_x = -0.25) +
    ggplot2::scale_color_manual(values = c('grey35', 'grey90')) +
    ggplot2::scale_fill_manual(values = class_colors) +
    ggplot2::scale_x_discrete(expand = c(0, 0), position = 'top')+
    ggplot2::labs(subtitle = 'P(level | class)',
                  x = 'level', y = '')+
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                    legend.position = 'none',
                    strip.placement = 'inside',
                    strip.text = ggplot2::element_text(
                      size = 14,
                      face = 'bold',
                      color = 'black',
                    ),
                    axis.text.y = ggplot2::element_text(size = 12),
                    axis.text.x = ggplot2::element_text(size = 12, color = 'grey45'),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_text(size = 10, color = 'grey45', hjust = 0),
                    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0))
  if(show_significance){
    plot <- plot +
      ggplot2::labs(x = 'level\n* p < 0.05, ** p < 0.01, *** p < 0.001, (ref) reference level for nominals')
  }
  plot
}
