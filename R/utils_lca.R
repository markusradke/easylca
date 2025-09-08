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
