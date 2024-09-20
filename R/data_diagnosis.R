data_diagnosis <- function(data, id){
  res <- list()
  variables <- colnames(data)[colnames(data) != id]
  for(var in variables){
    res <- c(res, hist(data[[var]]))
  }
  res <- c(res, data.frame(var  = 'a', missings_relfreq = 0))
}
