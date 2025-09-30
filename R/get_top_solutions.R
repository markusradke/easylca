get_top_n_solution_seeds <- function(model, n) {
  mplus_output <- model$output
  ll_lines <- get_ll_lines_from_output(mplus_output)
  all_seeds <- get_seeds_from_ll_lines(ll_lines)
  if (length(all_seeds) < n) {
    warning(sprintf(
      'For this model less than %d runs converged. Not performing replication check. Please increase number of random starts.',
      n
    ))
    return(NULL)
  }
  all_seeds[1:n]
}

get_ll_lines_from_output <- function(mplus_output) {
  start_index <- stringr::str_detect(
    mplus_output,
    "Final stage loglikelihood values"
  ) %>%
    which() +
    2 # for detected line and empty line
  cut_pre <- mplus_output[start_index:length(mplus_output)]
  end_index <- which(cut_pre == "")[1] - 1 # for detected line
  cut_pre_post <- cut_pre[1:end_index]
  cut_pre_post
}

get_seeds_from_ll_lines <- function(ll_lines) {
  stringr::str_extract(ll_lines, "\\s+(-?\\d+)(?=\\s)") %>% trimws()
}

create_input_files_for_replication <- function(
  settings,
  modeltype,
  nclasses,
  seeds
) {
  NULL
}
