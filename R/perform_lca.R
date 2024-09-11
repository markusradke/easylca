perform_lca <- function(settings){
  create_templates(settings)

  results <- list()
  results$settings <- settings

  models <- list()
  for(i in seq(6)){
    model <- mplus_lca(settings, model = i)
    models <- c(models, model)
  }

  class(results) <- 'easylca'
  results
}


mplus_lca <- function(settings, modeltype){
  setwd(settings$folder_name)
  analysis <- paste0(settings$analysis_name, '_model', modeltype)

  if (!dir.exists(paste0(analysis, '_lca'))) {dir.create(paste0(analysis, '_lca'))}
  list.files(paste0(analysis, '_lca'),include.dirs = F, full.names = T) %>% file.remove
  file.copy(paste0(analysis,"_template.txt"),paste0(analysis,"_lca/"))

  MplusAutomation::prepareMplusData(settings$frame,paste0(analysis,"_lca/",analysis,"_lca.dat"))
  MplusAutomation::createModels(paste0(analysis,"_lca/",analysis,"_template.txt"))
  MplusAutomation::runModels(paste0(analysis, '_lca'),logFile=paste0(analysis,"_lca/", analysis,"_log.txt"),showOutput=F,quiet=F)
  mplus_results<- MplusAutomation::readModels(paste0(analysis, '_lca'),recursive=T)
  saveRDS(mplus_results,paste0(analysis,"_lca/",analysis,".rds"))

  setwd('..')
  # return(mplus_results)
}
