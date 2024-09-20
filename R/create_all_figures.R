create_all_figures <- function(results, modeltypes){
  message('Creating plots...\n')
  results$plots$summary <- create_sabicplots(results$summary, results$settings)
  for(type in modeltypes){
    plotlist <- list()
    for(class in seq(results$settings$nclasses)){
      isconverged <- ! is.na(results$summary %>% dplyr::filter(classes == class & modeltype == type) %>% dplyr::pull(Parameters))
      if(isconverged){
        plot <- plot_modeltype_class_diagnostics(results, type, class)
        plotlist <- c(plotlist, list(plot))
      }
      else{ plot <- c(plotlist, list(NA))}
    }
    results$plots[[paste0('modeltype_', type)]] <- plotlist
  }
  results
}


create_sabicplots <- function(summary, settings){
  subtitle <- paste0(settings$analysis_name, ', n = ', settings$frame %>% nrow())
  if(length(settings$weights) != 0){
    subtitle <- paste0(subtitle, ' (weighted by ', settings$weights, ')')
  }

  summary_long <- tidyr::pivot_longer(summary,cols = c(BIC,saBIC), names_to = 'measure') %>%
    dplyr::filter(! is.na(value))

  summaryplot <- ggplot2::ggplot(summary_long, ggplot2::aes(x = classes, y = value, color = as.factor(modeltype)))+
    ggplot2::geom_line() +
    ggplot2::labs(title = 'Latent Class Analysis with MPlus 8.4',
                  subtitle = subtitle,
                  color = 'model type') +
    ggplot2::facet_wrap(~measure,nrow=1) +
    ggplot2::scale_x_continuous(limits=c(1, max(summary$classes)), breaks = seq(1,  max(summary$classes), 1))
  if(! all(summary$replicated)) {summaryplot <- summaryplot + ggplot2::labs(caption = 'ATTENTION: NOT ALL SOLUTIONS WERE REPLICATED.')}
  summaryplot


  ggplot2::ggsave(filename=paste0(settings$folder_name,"/", settings$analysis_name,'_summary_landscape.png'),
                  plot = summaryplot, device="png",
                  width = 1600,height = 900,units="px", scale=1.5)
  print(summaryplot)
  ggplot2::ggsave(filename=paste0(settings$folder_name,"/", settings$analysis_name,'_summary_portrait.png'),
                  plot = summaryplot, device="png",
                  width = 1000,height = 1000,units="px", scale=1.5)
  print(summaryplot)

  summaryplot
}
