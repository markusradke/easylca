plot_metric_profiles <- function(profiles, ncol_plot=2){
  nclasses <- profiles$segment %>% levels() %>% length()
  profiles <- dplyr::filter(profiles, param != 'Probabilities') %>%
    dplyr::mutate(pzero_alpha = ifelse(pzero == '', 0, 0.5))


  ggplot2::ggplot(profiles, ggplot2::aes(x = as.factor(segment), y = est, color = segment))+
    ggplot2::facet_wrap(.~item,  scales = "free", ncol = ncol_plot)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper))+
    ggplot2::geom_point(size = 2)+
    ggplot2::geom_point(ggplot2::aes(alpha = pzero_alpha, y = yposinflation), size =20) +
    ggplot2::geom_text(ggplot2::aes(y = yposinflation, label = pzero), vjust = 0.2, size = 3, color = 'black')+
    ggplot2::scale_color_discrete('')+
    ggplot2::scale_alpha_continuous(range = c(0, 1))+
    ggplot2::guides(alpha = 'none')+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())+
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5)))+
    ggplot2::xlab('') +
    ggplot2::ylab('model estimate')
}


plot_binary_profiles <- function(profiles){
  relative_share <- profiles %>%
    dplyr::filter(param == 'Probabilities' & level==1) %>%
    dplyr::rename(share_of_1 = est) %>%
    dplyr::select(segment, item, share_of_1)

  ggplot2::ggplot(relative_share, ggplot2::aes(x = segment, y = item)) +
    ggplot2::geom_point(ggplot2::aes(color = segment, alpha = share_of_1), size = 10) +
    ggplot2::geom_text(ggplot2::aes(label = round(share_of_1, 2)), color = "black") +
    ggplot2::scale_alpha_continuous(range = c(0.2, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = 'class', y = 'indicator') +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 0.5),
      legend.position = 'none'
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
             p = purrr::map_chr(item, .get_kruskal_p_from_prediction_for_item),
             chisquarep = paste0(chisquare, p)) %>%
      dplyr::arrange(-chisquare)
  }

  predicted <- model$savedata
  items <- model$input$variable$usevariables %>% stringr::str_split_1(' ')
  nitems <- items %>% length
  chisquares <- .get_kruskal_chisquares()
  max_chi_square <- chisquares$chisquare %>% max

  total_entropy <- model$summaries$Entropy
  ggplot2::ggplot(chisquares, ggplot2::aes(y = stats::reorder(item, chisquare), x = chisquare)) +
    ggplot2::geom_bar(stat = 'identity', fill = 'white', color = 'black') +
    ggplot2::geom_text(ggplot2::aes(label = chisquarep), hjust = -0.25, size = 3) +
    ggplot2::scale_x_continuous(limits = c(0, max_chi_square * 1.5)) +
    ggplot2::labs(title = paste0('chi square for Kruskall Wallis test, total entropy: ', total_entropy),
      x = 'Kruskal-Wallis χ²',
      y = 'indicator',
      caption = '* p < 0.05, **  p < 0.01, *** p < 0.001, Bonferroni corrected')
}


plot_modeltype_class_diagnostics <- function(results, type, class){
  settings <- results$settings
  model <- results$models[[type]][[class]]
  profiles <- extract_profile_for_plotting(model, settings)
  binary <- plot_binary_profiles(profiles)
  kruskal <- plot_kruskal_profiles(model)

  folder <- paste0(settings$folder_name, '/', settings$analysis_name, '_model', type,'_lca/plots')
  if(! dir.exists(folder)){
    dir.create(folder)
  }


  design_portrait <- 'AA
                      BC'
  metric <- plot_metric_profiles(profiles, ncol_plot = 2)
  portrait <- patchwork::wrap_plots(A = metric, B = binary, C = kruskal,
                        heights = c(3,1),
                        design = design_portrait)
  ggplot2::ggsave(paste0(folder, '/', class, '_', settings$analysis_name, '_model', type,'_diagnostic_portrait.png'),
                  portrait,
                  width = 9,
                  height = 16)

  metric <- plot_metric_profiles(profiles, ncol_plot = 4)
  design_landscape <- 'AB
                       AC'
  landscape <- patchwork::wrap_plots(A = metric, B = binary, C = kruskal,
                                    widths = c(3,1),
                                    design = design_landscape)
  ggplot2::ggsave(paste0(folder, '/', class, '_', settings$analysis_name, '_model', type,'_diagnostic_landscape.png'),
                  landscape,
                  width = 22,
                  height = 9)
  landscape
}
