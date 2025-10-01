#' Get model stability check during perform_lca
#'
#' Performs a stability check for the current combination of modeltype and class. The function reestimates the top n models using their corresponding seed and extracts the resulting casewise (N cases) log likelihoods (LL) and class assignment probabilities. These are then used to calculate:
#' 1. max. difference of BIC
#' 2. a parameter stability test p-value: Wilcoxon signed rank test on the parameters
#' 3. a parameter stability value: standard deviation of difference in casewise LL, standardized on the BIC scale: BIC = nclasses * ln(N) - 2ln(sd(LL))
#' 4. a class assignement test p-value: permuation test on the assingement probabilities with matched classes (with Hungarian algorithm)
#' 5. a class assignement stability value: standard deviation of difference in class assignement, one class left out (redundant information)
#'
#' @param model an MplusAutomation model as result of an LCA (single solution for a combination of modeltype and class)
#' @param settings as easylca settings object corresponding to the model
#' @param modeltype the modeltype for the model
#' @param ntop the number of top n solutions that will be compared for the replication check. Defaults to 10
#'
#' @returns list object with the stability check results
get_stability_check <- function(model, settings, modeltype, ntop = 10) {
  nclasses <- get_nclasses_from_model(model)
  seeds <- get_top_n_solution_seeds(model, ntop)
  if (!is.null(seeds)) {
    create_input_files_for_replication(
      settings,
      modeltype,
      nclasses,
      seeds
    )
    replication_models <- run_replication_models(settings, modeltype, nclasses)
    case_estimates <- get_case_cprob_and_ll(replication_models, settings)
  } else {
    return(NULL)
  }
}

get_nclasses_from_model <- function(model) {
  model$input$variable$classes %>%
    stringr::str_extract('[0-9]+') %>%
    as.integer()
}

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
  new_inp_content <- get_new_input_files_lines(seeds, nclasses, folders)
  new_inp_filepaths <- get_new_input_files_paths(seeds, folders)
  create_new_input_files(new_inp_content, new_inp_filepaths)
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

get_new_input_files_lines <- function(seeds, nclasses, folders) {
  mplus_input_wout_seed <- readLines(sprintf(
    '%s/%.2d_classes.inp',
    folders$modeltype,
    nclasses
  ))
  seed_idx <- which(mplus_input_wout_seed == 'ANALYSIS:') + 1
  lapply(seeds, function(seed) {
    c(
      mplus_input_wout_seed[1:seed_idx - 1],
      sprintf('OPTSEED = %s;', seed),
      mplus_input_wout_seed[seed_idx:length(mplus_input_wout_seed)]
    )
  })
}

get_new_input_files_paths <- function(seeds, folders) {
  lapply(seq_along(seeds), function(i) {
    sprintf('%s/%.2d_seed_%s.inp', folders$replication, i, seeds[i])
  }) %>%
    unlist()
}

create_new_input_files <- function(new_inp_content, new_inp_filepaths) {
  lapply(seq_along(new_inp_content), function(i) {
    writeLines(text = new_inp_content[[i]], con = new_inp_filepaths[i])
  })
}

run_replication_models <- function(settings, modeltype, nclasses) {
  folders <- get_modeltype_and_replication_folder(settings, modeltype, nclasses)
  MplusAutomation::runModels(
    folders$replication,
    logFile = sprintf('%s/replication_log.txt', folders$replication),
    showOutput = FALSE,
    quiet = TRUE
  )
  suppressWarnings(MplusAutomation::readModels(
    folders$replication,
    recursive = TRUE
  ))
}

get_case_cprob_and_ll <- function(replication_models, settings) {
  save_data <- do.call(
    dplyr::bind_rows,
    lapply(seq_along(replication_models), function(i) {
      replication_models[[i]]$savedata %>% dplyr::mutate(top_n = i)
    })
  )
  colnames(save_data) <- tolower(colnames(save_data))
  case_estimates <- save_data %>%
    dplyr::select(
      tolower(settings$id),
      dplyr::contains('cprob'),
      'top_n',
      'll_case' = 'outlogl'
    )
  ll_model <- case_estimates %>%
    dplyr::group_by(.data[['top_n']]) %>%
    dplyr::summarize(ll_model = sum(.data[['ll_case']]))
  dplyr::inner_join(case_estimates, ll_model, by = 'top_n')
}
