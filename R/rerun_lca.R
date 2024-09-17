rerun_lca <- function(lca, models_and_starts = NULL){
  if(class(lca) != 'easylca'){
    stop('Please make sure to supply an object of type "easylca" as input to the rerun_lca function. \n"easylca" objects can be obtained through the perform_lca command.')
  }
  if (is.null(models_and_starts)) {
    models_and_starts <- create_models_and_starts_for_rerun(lca)
  }

  check_assertions_models_and_starts(lca, models_and_starts)
  lca$settings <- update_starts_in_settings(lca$settings)
  # update settings from starts
  # create all templates


  # perform and update results for every row of models_and_starts

  return(lca)
}

# todo make it possible to perform single lcas for single templates and update the results accordingly

create_models_and_starts_for_rerun <- function(lca){
  starts <- lca$settings$starts
  lca$summary %>% dplyr::filter(! replicated) %>%
    dplyr::select(classes, modeltype) %>%
    dplyr::mutate(starts = purrr::map2_int(modeltype, classes,
                                           function(modeltype, classes){ starts[[modeltype]][[classes]] * 2}))
}

check_assertions_models_and_starts <- function(lca, models_and_starts){
  if(! all(c('classes', 'modeltype', 'starts') %in% colnames(models_and_starts))){
    stop('models_and_frame parameter does not contain all neccessary columns for the operation. Please include "classes", "modeltype" and "start".')
  }
  if(any(! models_and_starts$modeltype %in% c(1,2,3,4,5,6))) {
    stop('Please make sure models_and_frame contains only modeltypes 1, 2, 3, 4, 5 or 6.')
  }
  if(any(models_and_starts$classes %>% floor == models_and_starts$classes | models_and_starts$classes < 1)){
    stop('Please make sure models_and_frame contains only classes with integers > 0.')
  }
}

update_starts_in_settings <- function(settings, models_and_starts){
  settings
}
