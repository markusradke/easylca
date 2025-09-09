get_profiles_for_plotting <- function(model, settings){
  profile_types <- get_profile_types(settings)
  if(length(profile_types$continuous) > 0){
    continuous <- get_continuous_profiles(model, profile_types)
  }
  if(length(profile_types$categorical) > 0){
    categoricals <- get_categorical_profiles(model,
                                             profile_types$categorical,
                                             profile_types$binary)
  } else{categoricals <- c()}
  if(length(profile_types$nominal) > 0){
    nominals <- get_nominal_profiles(model, profile_types$nominal)
  } else{nominals <- c()}

  init <- data.frame('se' = double(), 'est_se' = double(), 'level' = integer(),
                     'upper' = double(), 'lower' = double(),
                     'pzero' = character(), 'yposinflation' = double())
  combined_profiles <- dplyr::bind_rows(init, continuous, categoricals, nominals)
  class_prevalences <- get_model_estimated_class_prevalences(model)
  suppressMessages(dplyr::inner_join(combined_profiles, class_prevalences)) %>%
    dplyr::mutate(class = paste('class', class)) %>%
    rename_items(profile_types)
}

get_profile_types <- function(settings){
  categorical <- settings$categorical
  binary <- get_binary_indicators(settings)
  nominal <- settings$nominal
  continuous <- settings$use[! settings$use %in% union(categorical, nominal)]
  negbin <- settings$negbin
  poisson <- settings$poisson
  list(continuous = continuous,
       categorical = categorical,
       binary = binary,
       nominal = nominal,
       negbin = negbin,
       poisson = poisson)
}

get_binary_indicators <- function(settings){
  potential_variables <- union(settings$categorical, settings$nominal)
  dplyr::summarize_at(settings$frame, potential_variables,
                      function(x) length(unique(x))) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::filter(.data$value == 2) %>%
    dplyr::pull(.data$name)
}


get_categorical_profiles <- function(model, categorical, binary){
  categoricals <- model$parameters$probability.scale %>%
    dplyr::mutate(item = .data$param %>% tolower(),
                  param = 'probability',
                  significance = get_significance_level(.data$pval),
                  plotgroup = ifelse(.data$item %in% binary,
                                     'binary', 'discrete'),
                  level = as.integer(.data$category)) %>%
    dplyr::filter(.data$item %in% categorical) %>%
    dplyr::rename(class = 'LatentClass') %>%
    dplyr::select(-'category')
  categoricals
}

get_nominal_profiles <- function(model, nominal){
  nominals_logit <- model$parameters$unstandardized %>%
    dplyr::mutate(item = .data$param %>% tolower()) %>%
    dplyr::filter(stringr::str_detect(.data$item,
                                      paste(nominal, collapse = '|'))) %>%
    dplyr::mutate(param = 'probability',
                  plotgroup = 'discrete',
                  significance = get_significance_level(.data$pval),
                  level = stringr::str_extract(.data$item, '(?<=#)[0-9]*') %>%
                    as.integer(),
                  item = stringr::str_extract(.data$item, '.*(?=#)')) %>%
    dplyr::rename(class = 'LatentClass') %>%
    dplyr::select(-'paramHeader', -'se', -'est_se')
  nominals <- calculate_probabilities_from_logits(nominals_logit)
  nominals
}

calculate_probabilities_from_logits <- function(logit){
  # logit for reference category (last) is 0
  odds <- logit %>% dplyr::mutate(est = exp(.data$est))
  sum_odds <- odds %>%
    dplyr::group_by(.data$item, .data$class) %>%
    dplyr::summarize(sum_odds = sum(.data$est) + 1)  # +1 for reference level
  odds <- suppressMessages(dplyr::full_join(sum_odds, odds))
  probabilites <- odds %>% # calculate via softmax function
    dplyr::mutate(est = .data$est / .data$sum_odds) %>%
    dplyr::select(-'sum_odds')
  reference_levels <- probabilites %>%
    dplyr::group_by(.data$item, .data$class) %>%
    dplyr::summarize(
      est = 1 - sum(.data$est, na.rm = TRUE),
      param = 'probability',
      pval = NA,
      plotgroup = 'discrete',
      significance = ' (ref)',
      level = max(.data$level) + 1
    ) %>%
    dplyr::ungroup()
  dplyr::bind_rows(probabilites, reference_levels)
}

get_continuous_profiles <- function(model, profile_types){
  continuous <- model$parameters$unstandardized %>%
    dplyr::mutate(item = .data$param %>% tolower(),
                  param = .data$paramHeader,
                  est_se = account_for_non_estimated_values(est),
                  se = account_for_non_estimated_values(se),
                  est = account_for_non_estimated_values(est_se),
                  significance = get_significance_level(.data$pval)) %>%
    dplyr::rename(class = 'LatentClass') %>%
    dplyr::filter(stringr::str_detect(.data$item,
                                      paste(profile_types$continuous, collapse = '|'))) %>%
    dplyr::select(-'paramHeader')
  means <- get_means_from_profiles(continuous)
  means <- correct_negbin_poisson_means(means, profile_types$negbin, profile_types$possion)
  errors <- get_errors_from_profiles(continuous, means)
  errors <- correct_negbin_errors(means,
                                                       errors,
                                                       continuous,
                                                       profile_types$negbin)
  errors <- correct_poisson_errors(means,
                                                        errors,
                                                        continuous,
                                                        profile_types$possion)
  pzero <- get_pzero_from_profiles(continuous)
  continuous <- combine_continuous_items(means, errors, pzero)
  continuous <- calculate_ypos_for_pzero(continuous)
  continuous
}

get_means_from_profiles <- function(continuous){
  continuous %>%
    dplyr::filter(! stringr::str_detect(.data$item, '#1')) %>%
    dplyr::filter(stringr::str_detect(.data$param, 'Means'))
}

correct_negbin_poisson_means <- function(means, negbin, poisson){
  if(length(negbin) > 0){
    means <- means %>%
      dplyr::mutate(est = ifelse(.data$item %in% negbin,
                               exp(.data$est), .data$est))
  }
  if(length(poisson) > 0){
    means <- means %>%
      dplyr::mutate(est = ifelse(.data$item %in% poisson & .data$est < 0,
                                 0, .data$est))
  }
  means
}

get_errors_from_profiles <-function(profiles, means){
  suppressMessages(profiles %>%
                     dplyr::filter(.data$param == 'Variances' | .data$param == 'Dispersion') %>%
                     dplyr::left_join(means %>% dplyr::select('item',
                                                              'class',
                                                              'mean'= 'est')) %>%
                     dplyr::mutate(sd = sqrt(.data$est),
                                   sd = ifelse(.data$sd == 'Inf', 0, .data$sd),
                                   upper = .data$mean + .data$sd,
                                   lower = .data$mean - .data$sd) %>%
                     dplyr::select('upper', 'lower', 'class', 'item'))
}

correct_negbin_errors <- function(means, errors, continuous, negbin){
  errors_dispersion <- suppressMessages(
    continuous %>%
      dplyr::filter(.data$param == 'Dispersion') %>%
      dplyr::select('class',
                    'item',
                    'disp' = 'est') %>%
      dplyr::inner_join(means) %>%
      dplyr::inner_join(errors) %>%
      dplyr::mutate(upper = .data$est + sqrt(.data$est + exp(.data$disp) * .data$est ** 2),
                    lower = .data$est - sqrt(.data$est + exp(.data$disp) * .data$est ** 2)) %>%
      dplyr::select('upper', 'lower', 'class', 'item')
  )

  errors %>% dplyr::filter(! .data$item %in% negbin) %>%
    rbind(errors_dispersion)
}

correct_poisson_errors <- function(means, errors, continuous, poisson){
  errors_poisson <- means %>%
    dplyr::filter(.data$item %in% poisson) %>%
    dplyr::mutate(est = ifelse(.data$est < 0, 0, .data$est),
                  upper = .data$est + sqrt(.data$est),
                  lower = .data$est - sqrt(.data$est)) %>%
    dplyr::select('upper', 'lower', 'class', 'item')

  errors %>% dplyr::filter(! .data$item %in% poisson) %>%
    rbind(errors_poisson)
}

get_pzero_from_profiles <- function(continuous){
  continuous %>%
    dplyr::filter(stringr::str_detect(.data$item, '#1')) %>%
    dplyr::mutate(pzero = exp(.data$est) / (exp(.data$est) +1),
                  pzero = round(.data$pzero, 2),
                  pzero = paste0('P(y <= 0)\n= ', .data$pzero, '\n', .data$significance),
                  item = stringr::str_remove_all(.data$item, '#1')) %>%
    dplyr::select(-dplyr::all_of(c('est', 'se', 'est_se', 'pval', 'significance')))
}

combine_continuous_items <- function(means, errors, pzero){
  combined <- suppressMessages(dplyr::left_join(means, errors) %>%
                                 dplyr::left_join(pzero) %>%
                                 dplyr::mutate(pzero = ifelse(is.na(pzero), '', pzero),
                                               plotgroup = 'continuous'))
  combined
}

calculate_ypos_for_pzero <- function(continuous) {
  upperclass <- suppressMessages(continuous %>%
                                   dplyr::mutate(upper = ifelse(is.na(.data$upper),
                                                                .data$est, .data$upper)) %>%  # for poisson
                                   dplyr::group_by(.data$item) %>%
                                   dplyr::slice_max(.data$upper, n = 1) %>%
                                   dplyr::ungroup() %>%
                                   dplyr::select(item = 'item', uppermax = 'upper') %>%
                                   dplyr::right_join(continuous))

  classrange <- suppressMessages(continuous %>%
                                   dplyr::mutate(lower = ifelse(is.na(.data$lower),
                                                                .data$est, .data$lower)) %>%  # for poisson
                                   dplyr::group_by(.data$item) %>%
                                   dplyr::slice_min(.data$lower, n = 1) %>%
                                   dplyr::ungroup() %>%
                                   dplyr::select('item', lowermin = 'lower') %>%
                                   dplyr::right_join(upperclass))

  classrange %>%
    dplyr::mutate(paramrange = .data$uppermax - .data$lowermin,
                  yposinflation = .data$lowermin - 0.3 * .data$paramrange) %>%
    dplyr::select(-'lowermin', -'uppermax', -'paramrange')
}


get_significance_level <- function(pval){
  significance <- ifelse(pval < 0.001, '***',
                         ifelse(pval < 0.01, '**',
                                ifelse(pval < 0.05, '*',
                                       '')))
  significance
}

account_for_non_estimated_values <- function(value){
  ifelse(value == '*********', NA, value) %>%
    as.double()
}

get_model_estimated_class_prevalences <- function(model){
  model$class_counts$modelEstimated %>%
    dplyr::select(-'proportion') %>%
    dplyr::mutate(class = as.character(.data$class))
}

rename_items <- function(profiles, profile_types){
  profiles %>%
    dplyr::mutate(item = ifelse(.data$item %in% profile_types$negbin,
                                paste0(item, '\n(negbin)'), item),
                  item = ifelse(.data$item %in% profile_types$poisson,
                                paste0(item, '\n(poisson)'), item))
}
