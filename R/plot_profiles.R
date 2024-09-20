plot_metric_profiles <- function(profiles, ncol_plot=2){
  nclasses <- profiles$segment %>% levels() %>% length()


  ggplot2::ggplot(profiles, ggplot2::aes(x = as.factor(segment), y = est, color = segment))+
    ggplot2::facet_wrap(.~item,  scales = "free", ncol = ncol_plot)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper))+
    ggplot2::geom_point(size = 2)+
    ggplot2::geom_text(ggplot2::aes(y = yposinflation, label = pzero), vjust = 0.2, size = 3, color = 'black')+
    ggplot2::scale_color_discrete('')+
    ggplot2::scale_alpha_continuous(range = c(0, 1))+
    ggplot2::guides(alpha = 'none', color = 'none')+
    ggplot2::labs(x = '', y = 'model estimate',
                  title = 'Class Conditional Means and Standard Deviations')+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
}


plot_binary_profiles <- function(profiles){
  relative_freqs <- profiles %>%
    dplyr::filter(param == 'Probabilities' & level==1) %>%
    dplyr::rename(share_of_1 = est) %>%
    dplyr::select(segment, item, share_of_1)

  ggplot2::ggplot(relative_freqs, ggplot2::aes(x = segment, y = item)) +
    ggplot2::geom_point(ggplot2::aes(color = segment, alpha = share_of_1), size = 10) +
    ggplot2::geom_text(ggplot2::aes(label = round(share_of_1, 2)), color = "black") +
    ggplot2::scale_alpha_continuous(range = c(0.2, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = 'class', y = 'indicator',
                  title = 'Class Conditional Item Probabilities',
                  caption = 'redundant value set to 1') +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = 'none'
    )
}

plot_prevalences <- function(profiles){
  distinct_prevalences <- dplyr::distinct(profiles, segment, .keep_all = T)
  distinct_prevalences$ypos <- cumsum(distinct_prevalences$count) - 0.5 * distinct_prevalences$count
  distinct_prevalences$ypos <- sum(distinct_prevalences$count) - distinct_prevalences$ypos

  ggplot2::ggplot(distinct_prevalences, ggplot2::aes(x = '', y = count, fill = segment)) +
    ggplot2::geom_bar(width = 1, stat = 'identity') +
    ggplot2::coord_polar('y') +
    ggplot2::geom_text(ggplot2::aes(y = ypos, label = count), color = 'white', fontface ='bold') +
    ggplot2::labs(title = 'Model Estimated Class Prevalences') +
    ggplot2::scale_fill_discrete('')+
    ggplot2::theme_void()
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
  suppressWarnings(
    ggplot2::ggplot(chisquares, ggplot2::aes(y = stats::reorder(item, chisquare), x = chisquare)) +
      ggplot2::geom_bar(stat = 'identity', fill = 'white', color = 'black') +
      ggplot2::geom_text(ggplot2::aes(label = chisquarep), hjust = -0.25, size = 3) +
      ggplot2::scale_x_continuous(limits = c(0, max_chi_square * 1.5)) +
      ggplot2::labs(title = paste0('Indicator Relevance, Total Entropy: ', total_entropy),
        x = 'Kruskal-Wallis χ²',
        y = 'indicator',
        caption = '* p < 0.05, **  p < 0.01, *** p < 0.001, Bonferroni corrected')
  )
}


plot_modeltype_class_diagnostics <- function(results, type, class){
  .plot_categorical_only <- function(){
    binary <- plot_binary_profiles(profiles)
    design_portrait <- 'A
                        B
                        C'
    portrait <- patchwork::wrap_plots(A = binary, B = kruskal, C = prevalences,
                                      heights = c(1,1,1),
                                      design = design_portrait)
    design_landscape <- 'A#
                         BC'
    landscape <- patchwork::wrap_plots(A = binary, B = kruskal, C = prevalences,
                                       widths = c(1,1),
                                       design = design_landscape)
    diagnosisplots <- list()
    diagnosisplots[['portrait']] <- portrait
    diagnosisplots[['landscape']] <- landscape
    diagnosisplots
  }

  .plot_metric_only <- function(){
    design_portrait <- 'AA
                        BC'
    metric_portrait <- plot_metric_profiles(profiles, ncol_plot = 2)
    metric_landscape <- plot_metric_profiles(profiles, ncol_plot = 4)
    portrait <- patchwork::wrap_plots(A = metric_portrait, B = kruskal, C = prevalences,
                                      heights = c(3,1),
                                      design = design_portrait)
    design_landscape <- 'AB
                         AC'
    landscape <- patchwork::wrap_plots(A = metric_landscape, B = kruskal, C = prevalences,
                                       widths = c(3,1),
                                       design = design_landscape)
    diagnosisplots <- list()
    diagnosisplots[['portrait']] <- portrait
    diagnosisplots[['landscape']] <- landscape
    diagnosisplots
  }

  .plot_all_profiles <- function(){
    binary <- plot_binary_profiles(profiles)
    design_portrait <- 'AA
                        BC
                        D#'
    metric_portrait <- plot_metric_profiles(profiles, ncol_plot = 2)
    metric_landscape <- plot_metric_profiles(profiles, ncol_plot = 4)

    portrait <- patchwork::wrap_plots(A = metric_portrait, B = binary,
                                      C = kruskal, D = prevalences,
                                      heights = c(3,1,1),
                                      design = design_portrait)
    design_landscape <- 'ABD
                         AC#'
    landscape <- patchwork::wrap_plots(A = metric_landscape, B = binary,
                                       C = kruskal, D = prevalences,
                                       widths = c(3,1,1),
                                       design = design_landscape)
    diagnosisplots <- list()
    diagnosisplots[['portrait']] <- portrait
    diagnosisplots[['landscape']] <- landscape
    diagnosisplots
  }

  settings <- results$settings
  model <- results$models[[type]][[class]]
  profiles <- extract_profile_for_plotting(model, settings)
  prevalences <- plot_prevalences(profiles)
  kruskal <- plot_kruskal_profiles(model)

  folder <- paste0(settings$folder_name, '/', settings$analysis_name, '_model', type,'_lca/plots')
  if(! dir.exists(folder)){
    dir.create(folder)
  }

  if(all(unique(profiles$param) %in% c('Probabilities'))){
    diagnosisplots <- .plot_categorical_only()
  }
  if(all(unique(profiles$param) %in% c('Means'))){
    diagnosisplots <- .plot_metric_only()
  }
  if(all(c('Means', 'Probabilities') %in% unique(profiles$param))){
    diagnosisplots <- .plot_all_profiles()
  }

  save_diagnostic_plots(diagnosisplots, settings, folder, type, class)

  diagnosisplots[['landscape']]
}


save_diagnostic_plots <- function(diagnosisplots, settings, folder, type, class){
  ggplot2::ggsave(paste0(folder, '/', class, '_', settings$analysis_name, '_model', type,'_diagnostic_portrait.png'),
                  diagnosisplots[['portrait']],
                  width = 9,
                  height = 16)


  ggplot2::ggsave(paste0(folder, '/', class, '_', settings$analysis_name, '_model', type,'_diagnostic_landscape.png'),
                  diagnosisplots[['landscape']],
                  width = 22,
                  height = 9)
}
