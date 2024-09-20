#' Show LCA summary
#'
#' @param easylca easylca object created with perform_lca() or rerun_lca().
#'
#' @return Data frame with LCA summary.
#' @export
#'
#' @examples
#' show_summary(testresults)
show_summary <- function(easylca){
  if(! 'easylca' %in% class(easylca)){
    stop('Please provide an object of type "easylca".')
  }
  plot(easylca$plots$summary)
  dplyr::as_tibble(easylca$summary)
}
