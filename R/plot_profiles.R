plot_metric_profiles <- function(profiles, ncol_plot=2){
  nclasses <- profiles$segment %>% levels() %>% length()
  class_colors <- discrete_colors_for_classes[1:nclasses] # internal from package

  profiles <- profiles %>% dplyr::filter(param == 'Means')

  average_means <- suppressMessages(
    profiles %>%
      dplyr::group_by(item) %>%
      dplyr::summarize(average_mean = mean(est)) %>%
      dplyr::right_join(profiles)
  )


  ggplot2::ggplot(average_means, ggplot2::aes(x = as.factor(segment), y = est, color = segment))+
    ggplot2::facet_wrap(.~item,  scales = "free", ncol = ncol_plot)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper))+
    ggplot2::geom_point(size = 2)+
    ggplot2::geom_text(ggplot2::aes(y = yposinflation, label = pzero), vjust = 0.2, size = 3, color = 'black')+
    ggplot2::scale_color_manual(values = class_colors)+
    ggplot2::geom_hline(ggplot2::aes(yintercept = average_mean), color = 'grey70', size = 0.6, linetype = 'twodash') +
    ggplot2::geom_text(ggplot2::aes(y =  average_mean), label = 'mean', x = 0, hjust = -1,
                       vjust = 1.05, color = 'grey70') +
    ggplot2::scale_x_discrete(position = 'top') +
    ggplot2::labs(y = 'model estimate',
                  subtitle = 'for P(y ≤ 0): * p < 0.05, ** p < 0.01, *** p < 0.001')+
    ggplot2::theme_minimal() +
    suppressWarnings(
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
                     plot.subtitle = ggplot2::element_text(color = 'grey45', size = 10),
                     strip.text = ggplot2::element_text(face = 'bold', size = 14),
                     strip.placement = 'outside',
                     panel.spacing.x = ggplot2::unit(14, 'points'),
                     panel.spacing.y = ggplot2::unit(36, 'points')
                     )
    )
}


plot_binary_profiles <- function(profiles){
  nclasses <- profiles$segment %>% levels() %>% length()
  class_colors <- discrete_colors_for_classes[1:nclasses] # internal from package

  relative_freqs <- profiles %>%
    dplyr::filter(param == 'Probabilities' & level==1) %>%
    dplyr::rename(share_of_1 = est) %>%
    dplyr::mutate(share_label = paste0(round(share_of_1, 2), significance)) %>%
    dplyr::select(segment, item, share_of_1, share_label)

  ggplot2::ggplot(relative_freqs, ggplot2::aes(x = segment, y = item, fill = segment)) +
    ggplot2::geom_tile(ggplot2::aes(alpha = share_of_1), color = "white", show.legend = F) +
    ggplot2::geom_text(ggplot2::aes(label = share_label), color = "black") +
    ggplot2::scale_alpha_continuous(range = c(0, 0.75)) +
    ggplot2::scale_x_discrete(position = 'top') +
    ggplot2::scale_fill_manual(values = class_colors) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = '',
                  subtitle = 'P(2 | class)',
                  x = '* p < 0.05, ** p < 0.01, *** p < 0.001') +
    suppressWarnings(
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
    )
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
      dplyr::filter(!stringr::str_detect(item, '#1')) %>%
      dplyr::mutate(item = as.character(item),
             chisquare = purrr::map_dbl(item, .get_kruskal_chisquare_from_prediction_for_item),
             p = purrr::map_chr(item, .get_kruskal_p_from_prediction_for_item)) %>%
      dplyr::arrange(-chisquare)
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
    ggplot2::ggplot(chisquares, ggplot2::aes(y = stats::reorder(item, chisquare), x = chisquare)) +
      ggplot2::geom_bar(stat = 'identity', fill = 'grey') +
      ggplot2::geom_text(ggplot2::aes(label = p), hjust = -0.1, size = 6, color = 'black') +
      ggplot2::scale_x_continuous(limits = c(0, max_chi_square * 1.5), expand = c(0, 0.05),
                                  position = 'top') +
      ggplot2::labs(subtitle = 'Kruskal-Wallis χ²', y = '',
        x = '* p < 0.05, **  p < 0.01, *** p < 0.001, Bonferroni corrected')) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_text(color = 'grey45', size = 10, hjust = 0),
                   panel.grid = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(hjust = 0, size = 12),
                   axis.text.y = ggplot2::element_text(size = 14, face = 'bold'),
                   axis.text.x = ggplot2::element_text(size = 10, color = 'grey45', hjust = 0))
}
