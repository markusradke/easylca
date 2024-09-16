extract_profile_for_plotting <- function(model, settings){
  profiles <- extract_profile(model, settings)
  means <- get_means_from_profiles(profiles)
  errors <- get_errors_from_profiles(profiles, means)

  means <- correct_negbin_poisson_means(means, settings)
  errors <- correct_negbin_poisson_errors(means, errors, profiles, settings)
  pzero <- get_pzero_from_profiles(profiles)
  categorical <- get_categorical_from_profiles(profiles)

  metric <- combine_metric_items(means, errors, pzero)
  metric <- calculate_ypos_for_pzero(metric)
  profile_ready <- rbind(metric, categorical)
  profile_ready
}


extract_profile<- function(model, settings){
  .make_empty_frame <- function(){
    data.frame(param = character(),
           item = character(),
           est = numeric(),
           se = numeric(),
           est_se = numeric(),
           pval = numeric(),
           segment = character(),
           level = numeric())
  }

  if(!is.null(model[["parameters"]][["probability.scale"]])){
    profile_categorical<-model[["parameters"]][["probability.scale"]] %>% as.data.frame() %>%
      dplyr::rename(item=param,level=category,segment=LatentClass) %>%
      dplyr::mutate(param = 'Probabilities') %>%
      dplyr::mutate(level=as.integer(level)-1)
  }
  else {profile_categorical  <- .make_empty_frame()}

  if(!is.null(model[["parameters"]][["unstandardized"]])){
    profile_metric <- model[["parameters"]][["unstandardized"]] %>% as.data.frame() %>%
      dplyr::rename(item=param,param=paramHeader, segment=LatentClass) %>%
      dplyr::mutate(level = NA) %>%
      dplyr::filter(param != 'Thresholds') %>%
      dplyr::filter(!stringr::str_detect(item, 'CLASS#[0-9]+'))
  }
  else {profile_metric <- .make_empty_frame()}


  profile <- rbind(profile_metric, profile_categorical) %>%
    dplyr::mutate(est_se = as.double(ifelse(stringr::str_detect(est_se, '\\*'), NA, est_se))) %>%
    dplyr::mutate(item=tolower(item)) %>%
    dplyr::mutate_at(c("segment","level","item"),as.factor)

  prevalences <- round((model[["class_counts"]][["modelEstimated"]][["proportion"]]*100),2)
  levels(profile$segment) <- paste0("class ",levels(profile$segment)," (",prevalences,"%)")

  profile %>% dplyr::mutate(est = ifelse(pval > 0.05, 0, est))
}


get_means_from_profiles <- function(profiles){
  profiles %>%
    dplyr::filter(! stringr::str_detect(item, '#1')) %>%
    dplyr::filter(stringr::str_detect(param, 'Means'))
}

get_errors_from_profiles <-function(profiles, means){
  suppressMessages(profiles %>%
                     dplyr::filter(param == 'Variances' | param == 'Dispersion') %>%
                     dplyr::left_join(means %>% dplyr::select(item, segment, 'mean'= est)) %>%
                     dplyr::mutate(sd = sqrt(est),
                                   sd = ifelse(sd == 'Inf', 0, sd),
                                   upper = mean + sd,
                                   lower = mean - sd) %>%
                     dplyr::select(upper, lower, segment, item))
}

correct_negbin_poisson_means <- function(means, settings){
  means %>%
    dplyr::mutate(est = ifelse(item %in% settings$negbin, exp(est), est)) %>%
    dplyr::mutate(est = ifelse(item %in% settings$poisson & est < 0, 0, est))
}

correct_negbin_poisson_errors <- function(means, errors, profiles, settings){
  errors_dispersion <- suppressMessages(profiles %>%
                                          dplyr::filter(param == 'Dispersion') %>%
                                          dplyr::select(segment, item, 'disp' = est) %>%
                                          dplyr::inner_join(means) %>% dplyr::inner_join(errors) %>%
                                          dplyr::mutate(upper = est + sqrt(est + exp(disp) * est ** 2),
                                                        lower = est - sqrt(est + exp(disp) * est ** 2)) %>%
                                          dplyr::select(upper, lower, segment, item))

  errors_dispersion_correct <- errors %>% dplyr::filter(! item %in% settings$negbin) %>%
    rbind(errors_dispersion)

  errors_poisson <- means %>%
    dplyr::filter(item %in% settings$poisson) %>%
    dplyr::mutate(est = ifelse(est < 0, 0, est),
                  upper = est + sqrt(est),
                  lower = est - sqrt(est)) %>%
    dplyr::select(upper, lower, segment, item)

  errors_dispersion_correct %>% dplyr::filter(! item %in% settings$poisson) %>%
    rbind(errors_poisson)
}

get_pzero_from_profiles <- function(profiles){
  profiles %>%
    dplyr::filter(stringr::str_detect(item, '#1')) %>%
    dplyr::mutate(pzero = exp(est) / (exp(est) +1),
                  pzero = round(pzero,2),
                  pzero = paste0('P(y ≤ 0)\n= ', pzero),
                  item = stringr::str_remove_all(item, '#1')) %>%
    dplyr::select(-dplyr::all_of(c('est', 'se', 'est_se', 'pval', 'level')))
}


get_categorical_from_profiles <- function(profiles) {
  profiles %>%
    dplyr::filter(param == 'Probabilities') %>%
    dplyr::mutate(upper = NA,
                  lower = NA,
                  pzero = '',
                  yposinflation = NA)
}

combine_metric_items <- function(means, errors, pzero){
  combined <- suppressMessages(dplyr::left_join(means, errors) %>%
                                 dplyr::left_join(pzero) %>%
                                 dplyr::mutate(pzero = ifelse(is.na(pzero), '', pzero)))
  combined
}

calculate_ypos_for_pzero <- function(allparameters){
  upperclass <- suppressMessages(allparameters %>%
                                   dplyr::group_by(.data[['item']]) %>% dplyr::slice_max(.data[['upper']], n = 1) %>% dplyr::ungroup() %>%
                                   dplyr::select('item', 'uppermax' = 'upper') %>%
                                   dplyr::right_join(allparameters))

  lowerclass <- suppressMessages(allparameters %>%
                                   dplyr::group_by(.data[['item']]) %>% dplyr::slice_min(.data[['lower']], n=1) %>% dplyr::ungroup() %>%
                                   dplyr::select('item', 'lowermin' = 'lower') %>% dplyr::right_join(upperclass))

  lowerclass %>%
    dplyr::mutate(paramrange = uppermax - lowermin,
                  yposinflation = lowermin - 0.3 * paramrange) %>%
    dplyr::select(-lowermin, -uppermax, -paramrange)
}
