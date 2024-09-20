#' Diagnose Data
#'
#'Show histograms for all variables in the data frame (except id) and the relative frequencies of NAs for all variables in the data frame (except id). Histograms will also be drawn for categorical variables. Categorical variables must be supplied as integers with values > 0 to be interpreted the right way (as is also necessary for the LCA).
#'
#' @param frame The data frame you want to perform the LCA on.
#' @param id The id columns of the data frame. It will be excluded from the diagnosis. For all other variables a histogram will be generated and the relative frequency of NAs will be shown.
#'
#' @return List with histograms for all variables and a data frame that shows the relative frequencies of NAs in every variable at the end of the list.
#' @export
#'
#' @examples
#' diagnose_data(testdata, 'id')
diagnose_data <- function(frame, id){
  if(! 'data.frame' %in% class(frame)){stop('Please provide an object of class data.frame for analysis.')}
  if(! all(frame %>% purrr::map(class) %>% unlist() %in% c('integer', 'numeric'))){
    stop('Please provide only numeric and integer data columns. Make sure categorical variables are coded as integers > 0.')}

  res <- list()
  variables <- colnames(frame)[colnames(frame) != id]
  for(i in seq(length(variables))){
    res[[i]] <- draw_histogram_for_variable(frame, variables, i)
  }
  res[[length(variables) + 1]] <- purrr::map_df(variables, get_missings_relative_frequency, frame)
  message('Relative frequencies of missings for all variables in the dataframe:\n')
  print(res[[length(variables) + 1]])
  res
}

draw_histogram_for_variable <- function(frame, variables, i){
  var <- variables[i]
  vector <- frame[[var]]
  n_unique_values <- unique(vector) %>% length()
  nbreaks <- ifelse(n_unique_values < 100, n_unique_values, 100)
  histogram <- hist(vector,
                   main = paste('Histogram of', var),
                   xlab = var,
                   breaks = nbreaks,
                   xaxt='n')
  if(n_unique_values < 10){
    xticks <- seq(min(vector, na.rm = T), max(vector, na.rm = T), by=1)
    axis(1, at=xticks, labels=xticks)
  }
  else{
    axis(1)
  }
  histogram
}

get_missings_relative_frequency <- function(variable, frame){
  data.frame(variable = variable,
             NA_relfreq = nrow(dplyr::filter(frame, is.na(.data[[variable]])))/ nrow(frame))
}
