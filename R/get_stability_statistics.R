get_stability_statistics <- function(cases) {
  nclasses <- get_number_of_classes(cases)
  N <- nrow(cases) / length(unique(cases$top_n))
  get_max_bic_diff(cases)
  browser()
  NULL
}

get_number_of_classes <- function(cases) {
  colnames(dplyr::select(
    cases,
    dplyr::matches('^cprob[0-9]{1,2}$')
  )) %>%
    length()
}

get_max_bic_diff <- function(cases) {
  # with BIC = nparameters * ln(N) - 2LL, reduces to D_BIC = 2(LL1 - LL2) with both number of parameters and number of observations equal
  # this is equal to 2 * (max(LL) - min(LL))
  2 * (max(cases$ll_model) - min(cases$ll_model))
}
