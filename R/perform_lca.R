perform_lca <- function(settings){
  create_templates(settings)

  results <- list()
  results$settings <- settings

  models <- list()
  for(i in seq(6)){
    modeltype <- mplus_lca(settings, model = i)
    models <- c(models, list(modeltype))
  }
  results$models <- models

  results$summary <- create_modeloverview(results$models, settings)
  results$plot <- list(create_sabicplots(results$summary, settings))

  class(results) <- 'easylca'
  saveRDS(results, paste0(settings$folder_name, '/', settings$analysis_name, '_lca_results.rds'))
  results
}


mplus_lca <- function(settings, modeltype){
  setwd(settings$folder_name)
  analysis <- paste0(settings$analysis_name, '_model', modeltype)
  type_folder <- paste0(analysis, '_lca')

  if (!dir.exists(type_folder)) {dir.create(type_folder)}
  list.files(type_folder,include.dirs = F, full.names = T) %>% file.remove
  file.copy(paste0(analysis,"_template.txt"),paste0(analysis,"_lca/"))

  MplusAutomation::prepareMplusData(settings$frame,paste0(analysis,"_lca/",analysis,"_lca.dat"))
  MplusAutomation::createModels(paste0(analysis,"_lca/",analysis,"_template.txt"))
  MplusAutomation::runModels(type_folder,logFile=paste0(analysis,"_lca/", analysis,"_log.txt"),showOutput=F,quiet=F)
  mplus_results<- MplusAutomation::readModels(type_folder,recursive=T)
  saveRDS(mplus_results,paste0(analysis,"_lca/",analysis,".rds"))

  setwd('..')
  return(mplus_results)
}


create_modeloverview <- function(models, settings){
  modeloverview <- data.frame()

  for(type in seq(length(models))){
    general_info <- models[[type]] %>%
      sapply(function(model_per_class) {model_per_class[['summaries']]}) %>%
      purrr::map_dfr(~ .x) %>%
      dplyr::select(Title,Parameters,LL,AIC,AICC,BIC,saBIC=aBIC,Entropy, dplyr::any_of(c("T11_VLMR_PValue","T11_LMR_PValue"))) %>%
      as.data.frame() %>%
      dplyr::mutate(classes = dplyr::row_number(), .before=1)

    nmin <- models[[type]] %>%
      sapply(function(model_per_class) {model_per_class[['class_counts']][['modelEstimated']][['count']]}) %>%
      lapply(min) %>%
      unlist

    replicated <- models[[type]] %>%
      sapply(function(model_per_class) {list(model_per_class[['warnings']])}) %>%
      lapply(unlist) %>% as.character() %>%
      stringr::str_detect(pattern = 'NOT REPLICATED', negate = T)

    boundary_values <- models[[type]] %>%
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

create_sabicplots <- function(summary, settings){
  summary_long <- tidyr::pivot_longer(summary,cols = c(BIC,saBIC), names_to = 'measure')

  summaryplot <- ggplot2::ggplot(summary_long, ggplot2::aes(x = classes, y = value, color = as.factor(modeltype)))+
    ggplot2::geom_line() +
    ggplot2::labs(title = 'Latent Class Analysis with MPlus 8.4',
                  subtitle = paste0(settings$analysis_name, ', n = ', settings$frame %>% nrow()),
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
