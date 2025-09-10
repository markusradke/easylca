print_elapsed_time <- function(start_time, end_time){
  elapsed_time <- difftime(end_time, start_time, units = "secs")
  total_seconds <- as.numeric(elapsed_time)

  hours <- total_seconds %/% 3600 %>% as.integer()
  minutes <- (total_seconds %% 3600) %/% 60 %>% as.integer()
  seconds <- total_seconds %% 60 %>% as.integer()

  message(sprintf("The time needed for the analysis was: %02d:%02d:%02d.\n", hours, minutes, seconds))
}

is_mplus_installed <- function() {
  result <- tryCatch({
    detection <- MplusAutomation::detectMplus()
    is.character(detection) && length(detection) > 0
  }, error = function(e) {
    FALSE
  })
  return(result)
}

remove_remaining_templates <- function(settings){
  files <- list.files(settings$folder_name, full.names = TRUE)
  template_files <- files[stringr::str_detect(files, 'template.txt')]
  lapply(template_files, file.remove)
}

make_list_if_only_one_model <- function(mplus_results, modeltype){
  if('summaries' %in% names(mplus_results)) { # when only one class is found
    temp <- mplus_results
    mplus_results <- list()
    model_name <- sprintf('modeltype_%d.%.2d_classes.out',
                          modeltype,
                          temp$summaries$NLatentClass)
    mplus_results[[model_name]] = temp
  }
  mplus_results
}
