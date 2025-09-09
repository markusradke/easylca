get_profiles_for_plotting <- function(model, settings){
  profile_types <- get_profile_types(settings)
  categoricals <- get_categorical_profiles(model,
                                           profile_types$categorical,
                                           profile_types$binary)

}

get_profile_types <- function(settings){
  categorical <- settings$categorical
  binary <- get_binary_indicators(settings)
  nominal <- settings$nominal
  continuous <- settings$use[! settings$use %in% union(categorical, nominal)]
  list(continuous = continuous,
       categorical = categorical,
       binary = binary,
       nominal = nominal)
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
                  significance = get_significance_level(pval),
                  plotgroup = ifelse(.data$item %in% binary,
                                     'binary', 'discrete')) %>%
    dplyr::filter(.data$item %in% categorical) %>%
    dplyr::rename(level = 'category',
                  class = 'LatentClass')
  categoricals
}

get_nominal_profiles <- function(model, nominal){
  nominals_logit <- model$parameters$unstandardized %>%
    dplyr::mutate(item = .data$param %>% tolower()) %>%
    dplyr::filter(stringr::str_detect(.data$item,
                                      paste(nominal, collapse = '|'))) %>%
    dplyr::mutate(param = 'probability',
                  plotgroup = 'discrete',
                  significance = get_significance_level(pval),
                  level = stringr::str_extract(item, '(?<=#)[0-9]*') %>%
                    as.integer(),
                  item = stringr::str_extract(item, '.*(?=#)')) %>%
    dplyr::rename(class = 'LatentClass') %>%
    dplyr::select(-paramHeader, -se, -est_se)
  nominals <- calculate_probabilities_from_logits(nominals_logit)
  nominals
}

calculate_probabilities_from_logits <- function(logit){
  # logit for reference category (last) is 0
  odds <- logit %>% dplyr::mutate(est = exp(.data$est))
  sum_odds <- odds %>%
    dplyr::group_by(item, class) %>%
    dplyr::summarize(sum_odds = sum(est) + 1)  # +1 for reference level
  odds <- suppressMessages(dplyr::full_join(sum_odds, odds))
  probabilites <- odds %>% # calculate via softmax function
    dplyr::mutate(est = .data$est / .data$sum_odds) %>%
    dplyr::select(-sum_odds)
  reference_levels <- probabilites %>%
    dplyr::group_by(item, class) %>%
    dplyr::summarize(
      est = 1 - sum(est, na.rm = TRUE),
      param = 'probability',
      pval = NA,
      plotgroup = 'discrete',
      significance = ' (ref)',
      level = max(level) + 1
    ) %>%
    dplyr::ungroup()
  dplyr::bind_rows(probabilites, reference_levels)
}

get_significance_level <- function(pval){
  significance <- ifelse(pval < 0.001, '***',
                         ifelse(pval < 0.01, '**',
                                ifelse(pval < 0.05, '*',
                                       '')))
  significance
}

# get_profiles_for_plotting <- function(model, settings){
#   profiles <- extract_profile(model, settings)
#   means <- get_means_from_profiles(profiles)
#   errors <- get_errors_from_profiles(profiles, means)
#
#   means <- correct_negbin_poisson_means(means, settings)
#   errors <- correct_negbin_poisson_errors(means, errors, profiles, settings)
#   pzero <- get_pzero_from_profiles(profiles)
#
#   binary_indicators <- get_binary_indicators(settings)
#   categorical <- get_categorical_from_profiles(profiles, binary_indicators)
#
#   metric <- combine_metric_items(means, errors, pzero)
#   metric <- calculate_ypos_for_pzero(metric)
#   profile_ready <- rbind(metric, categorical)
#   profile_ready
# }
#
#
# extract_profile<- function(model, settings){
#   .make_empty_frame <- function(){
#     data.frame(param = character(),
#            item = character(),
#            est = numeric(),
#            se = numeric(),
#            est_se = numeric(),
#            pval = numeric(),
#            segment = character(),
#            level = numeric())
#   }
#   if(!is.null(model[["parameters"]][["probability.scale"]])){
#     profile_categorical <- model[["parameters"]][["probability.scale"]] %>% as.data.frame() %>%
#       dplyr::rename(item = 'param',
#                     level= 'category',
#                     segment = 'LatentClass') %>%
#       dplyr::mutate(param = 'Probabilities') %>%
#       dplyr::mutate(level = as.integer(.data$level)-1)
#   }
#   else {profile_categorical  <- .make_empty_frame()}
#
#   if(!is.null(model[["parameters"]][["unstandardized"]])){
#     profile_metric <- model[["parameters"]][["unstandardized"]] %>% as.data.frame() %>%
#       dplyr::rename(item = 'param',
#                     param = 'paramHeader',
#                     segment= 'LatentClass') %>%
#       dplyr::mutate(level = NA,
#                     est = ifelse(.data$est == '*********', NA, .data$est),
#                     est = as.double(.data$est)) %>%
#       dplyr::filter(.data$param != 'Thresholds') %>%
#       dplyr::filter(!stringr::str_detect(.data$item, 'CLASS#[0-9]+'))
#   }
#   else {profile_metric <- .make_empty_frame()}
#
#
#   profile <- rbind(profile_metric, profile_categorical) %>%
#     dplyr::mutate(est_se = as.double(ifelse(stringr::str_detect(.data$est_se, '\\*'),
#                                             NA,
#                                             .data$est_se))) %>%
#     dplyr::mutate(item = tolower(.data$item),
#                   level = as.factor(.data$level)) %>%
#     dplyr::mutate_at(c("segment","item"), forcats::fct_inorder)
#
#   relative_prevalences <- round((model[["class_counts"]][["modelEstimated"]][["proportion"]]*100),
#                                 2)
#   levels(profile$segment) <- paste0("class ",levels(profile$segment))
#
#   profile$count <- profile$segment
#   levels(profile$count) <- round(model[['class_counts']][['modelEstimated']][['count']])
#   profile$count <- as.numeric(levels(profile$count))[as.integer(profile$count)]
#
#   profile %>% dplyr::mutate(significance = ifelse(.data$pval < 0.05, '*', ''),
#                             significance = ifelse(.data$pval < 0.01, '**', .data$significance),
#                             significance = ifelse(.data$pval < 0.001, '***', .data$significance))
# }
#
#
# get_means_from_profiles <- function(profiles){
#   profiles %>%
#     dplyr::filter(! stringr::str_detect(.data$item, '#1')) %>%
#     dplyr::filter(stringr::str_detect(.data$param, 'Means'))
# }
#
# get_errors_from_profiles <-function(profiles, means){
#   suppressMessages(profiles %>%
#                      dplyr::filter(.data$param == 'Variances' | .data$param == 'Dispersion') %>%
#                      dplyr::left_join(means %>% dplyr::select('item',
#                                                               'segment',
#                                                               'mean'= 'est')) %>%
#                      dplyr::mutate(sd = sqrt(.data$est),
#                                    sd = ifelse(.data$sd == 'Inf', 0, .data$sd),
#                                    upper = .data$mean + .data$sd,
#                                    lower = .data$mean - .data$sd) %>%
#                      dplyr::select('upper', 'lower', 'segment', 'item'))
# }
#
# correct_negbin_poisson_means <- function(means, settings){
#   means %>%
#     dplyr::mutate(est = ifelse(.data$item %in% settings$negbin, exp(.data$est), .data$est)) %>%
#     dplyr::mutate(est = ifelse(.data$item %in% settings$poisson & .data$est < 0, 0, .data$est))
# }
#
# correct_negbin_poisson_errors <- function(means, errors, profiles, settings){
#   errors_dispersion <- suppressMessages(
#     profiles %>%
#       dplyr::filter(.data$param == 'Dispersion') %>%
#       dplyr::select('segment',
#                     'item',
#                     'disp' = 'est') %>%
#       dplyr::inner_join(means) %>%
#       dplyr::inner_join(errors) %>%
#       dplyr::mutate(upper = .data$est + sqrt(.data$est + exp(.data$disp) * .data$est ** 2),
#                     lower = .data$est - sqrt(.data$est + exp(.data$disp) * .data$est ** 2)) %>%
#       dplyr::select('upper', 'lower', 'segment', 'item')
#   )
#
#   errors_dispersion_correct <- errors %>% dplyr::filter(! .data$item %in% settings$negbin) %>%
#     rbind(errors_dispersion)
#
#   errors_poisson <- means %>%
#     dplyr::filter(.data$item %in% settings$poisson) %>%
#     dplyr::mutate(est = ifelse(.data$est < 0, 0, .data$est),
#                   upper = .data$est + sqrt(.data$est),
#                   lower = .data$est - sqrt(.data$est)) %>%
#     dplyr::select('upper', 'lower', 'segment', 'item')
#
#   errors_dispersion_correct %>% dplyr::filter(! .data$item %in% settings$poisson) %>%
#     rbind(errors_poisson)
# }
#
# get_pzero_from_profiles <- function(profiles){
#   profiles %>%
#     dplyr::filter(stringr::str_detect(.data$item, '#1')) %>%
#     dplyr::mutate(pzero = exp(.data$est) / (exp(.data$est) +1),
#                   pzero = round(.data$pzero, 2),
#                   pzero = paste0('P(y <= 0)\n= ', .data$pzero, '\n', .data$significance),
#                   item = stringr::str_remove_all(.data$item, '#1')) %>%
#     dplyr::select(-dplyr::all_of(c('est', 'se', 'est_se', 'pval',
#                                    'level', 'count', 'significance')))
# }
#

# get_categorical_from_profiles <- function(profiles, binary_indicators) {
#   profiles %>%
#     dplyr::filter(.data$param == 'Probabilities') %>%
#     dplyr::mutate(upper = NA,
#                   lower = NA,
#                   pzero = '',
#                   yposinflation = NA,
#                   plotgroup = ifelse(.data$item %in% binary_indicators,
#                                      'binary', 'discrete'))
# }
#
# combine_metric_items <- function(means, errors, pzero){
#   combined <- suppressMessages(dplyr::left_join(means, errors) %>%
#                                  dplyr::left_join(pzero) %>%
#                                  dplyr::mutate(pzero = ifelse(is.na(pzero), '', pzero),
#                                                plotgroup = 'continuous'))
#   combined
# }
#
# calculate_ypos_for_pzero <- function(allparameters) {
#   upperclass <- suppressMessages(allparameters %>%
#                                    dplyr::group_by(.data$item) %>%
#                                    dplyr::slice_max(.data$upper, n = 1) %>%
#                                    dplyr::ungroup() %>%
#                                    dplyr::select(item = 'item', uppermax = 'upper') %>%
#                                    dplyr::right_join(allparameters))
#
#   lowerclass <- suppressMessages(allparameters %>%
#                                    dplyr::group_by(.data$item) %>%
#                                    dplyr::slice_min(.data$lower, n = 1) %>%
#                                    dplyr::ungroup() %>%
#                                    dplyr::select('item', lowermin = 'lower') %>%
#                                    dplyr::right_join(upperclass))
#
#   lowerclass %>%
#     dplyr::mutate(paramrange = .data$uppermax - .data$lowermin,
#                   yposinflation = .data$lowermin - 0.3 * .data$paramrange) %>%
#     dplyr::select(-'lowermin', -'uppermax', -'paramrange')
# }
