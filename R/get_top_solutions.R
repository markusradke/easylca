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
  folders <- get_modeltype_and_replication_folder(settings, modeltype, nclasses)
  create_folder(folders$replication)
  copy_dat_file_for_replication(folders, modeltype, nclasses)
}

get_modeltype_and_replication_folder <- function(
  settings,
  modeltype,
  nclasses
) {
  modeltype_folder <- sprintf('%s/modeltype_%d', settings$folder, modeltype)
  replication_folder <- sprintf(
    '%s/check_replication_%.2d_classes',
    modeltype_folder,
    nclasses
  )
  list(modeltype = modeltype_folder, replication = replication_folder)
}

create_folder <- function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
}

copy_dat_file_for_replication <- function(folders, modeltype, nclasses) {
  file.copy(
    from = sprintf(
      '%s/modeltype_%d.dat',
      folders$modeltype,
      modeltype
    ),
    to = sprintf(
      '%s/modeltype_%d.dat',
      folders$replication,
      modeltype
    )
  )
}
