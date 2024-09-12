extract_profile_for_plotting <- function(model, settings){
  profiles <- extract_profile(model, settings)
  means <- get_means_from_profiles(profiles)
  variances <- get_variances_from_profiles(profiles)
  pzero <- get_pzero_from_profiles(profiles)
  # binary <- get_binary_from_profiles(profiles)
  allparameters <- combine_all_profile_items(means, variances, pzero)
  profile_ready <- calculate_ypos_for_pzero(allparameters)

  # profile_ready <-make_class_lables(model, profile_ready)
  # profile_ready
  allparameters
}


extract_profile<- function(model, settings){
  .make_empty_tibble <- function(){
    tibble(param = character(),
           item = character(),
           est = numeric(),
           se = numeric(),
           est_se = numeric(),
           pval = numeric(),
           segment = character(),
           level = numeric())
  }

  if(!is.null(model[["parameters"]][["probability.scale"]])){
    profile_binary<-model[["parameters"]][["probability.scale"]] %>% as.data.frame() %>%
      dplyr::rename(item=param,level=category,segment=LatentClass) %>%
      dplyr::mutate(param = 'probability') %>%
      dplyr::mutate(level=as.integer(level)-1)
  }
  else {profile_binary  <- .make_empty_tibble()}

  if(!is.null(model[["parameters"]][["unstandardized"]])){
    profile_metric <- model[["parameters"]][["unstandardized"]] %>% as.data.frame() %>%
      dplyr::rename(item=param,param=paramHeader, segment=LatentClass) %>%
      dplyr::mutate(level = NA) %>%
      dplyr::filter(param != 'Thresholds') %>%
      dplyr::filter(!stringr::str_detect(item, 'CLASS#[0-9]+'))
  }
  else {profile_metric <- .make_empty_tibble()}


  profile <- rbind(profile_metric, profile_binary) %>%
    dplyr::mutate(est_se = as.double(ifelse(stringr::str_detect(est_se, '\\*'), NA, est_se))) %>%
    dplyr::mutate(item=tolower(item)) %>%
    dplyr::mutate_at(c("segment","level","item"),as.factor)

  prevalences <- round((model[["class_counts"]][["modelEstimated"]][["proportion"]]*100),2)
  levels(profile$segment) <- paste0("class ",levels(profile$segment)," (",prevalences,"%)")

  return(profile)
}


get_means_from_profiles <- function(profiles){
  profiles %>%
    dplyr::filter(! stringr::str_detect(item, '#1')) %>%
    dplyr::filter(stringr::str_detect(param, 'Means'))
}

get_variances_from_profiles <-function(profiles){
  profiles %>%
    dplyr::filter(param == 'Variances' | param == 'Dispersion') %>%
    dplyr::mutate(est = ifelse(pval > 0.05, 0, est),
                  sd = sqrt(est),
                  sd = ifelse(sd == 'Inf', 0, sd)) %>%
    dplyr::select(sd, segment, item)
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

combine_all_profile_items <- function(means, variances, pzero){
  suppressMessages(dplyr::left_join(means, variances)) %>%
    dplyr::mutate(upper = est + sd,
                  lower = est - sd,
                  upper = ifelse(item == 'widsgnfc', exp(est) + sqrt(exp(est) + exp(sd**2) * exp(est)**2), upper),
                  lower = ifelse(item == 'widsgnfc', exp(est) - sqrt(exp(est) + exp(sd**2) * exp(est)**2), lower),
                  est = ifelse(item == 'widsgnfc', exp(est), est)) %>%
    dplyr::left_join(pzero) %>%
    dplyr::mutate(pzero = ifelse(is.na(pzero), '', pzero))
}

calculate_ypos_for_pzero <- function(allparameters){
  upperclass <- allparameters %>%
    dplyr::group_by(.data[['item']]) %>% dplyr::slice_max(.data[['upper']], n = 1) %>% dplyr::ungroup() %>%
    dplyr::select('item', 'uppermax' = 'upper') %>%
    dplyr::glimpse() %>%
    dplyr::right_join(allparameters)

  lowerclass <- allparameters %>%
    dplyr::group_by(.data[['item']]) %>% dplyr::slice_min(.data[['lower']], n=1) %>% dplyr::ungroup() %>%
    dplyr::select('item', 'lowermin' = 'lower') %>% dplyr::right_join(upperclass)

  lowerclass %>%
    dplyr::mutate(paramrange = uppermax - lowermin,
           yposinflation = lowermin - 0.3 * paramrange) %>%
    dplyr::select(-lowermin, -uppermax, -paramrange)
}

make_class_lables <- function(model, profile_ready){
  estimated_classcounts <- model$class_counts$modelEstimated %>%
    dplyr::mutate(count = round(count, 2),
                  proportion = paste0(round(proportion * 100, 2),'%'),
                  countlabel = paste0('(',count, ', ', proportion, ')')) %>%
    dplyr::select(classnumber = class, countlabel)

  profile_ready$classnumber <- as.integer(str_extract(profile_ready$segment, '(?<= )[1-9](?= )'))
  profile_ready$classlabel <- interpretation[profile_ready$classnumber]
  profile_ready %>% dplyr::inner_join(estimated_classcounts) %>%
    dplyr::mutate(segment = paste(classlabel, countlabel))
}

plot_lca_profiles <- function(profiles, ncol=2, y_lab = 'model estimate'){
  nclasses <- profiles$segment %>% levels() %>% length()
  ggplot2::ggplot(profiles, aes(x = as.factor(segment), y = est, color = segment))+
    ggplot2::facet_wrap(.~item,  scales = "free", ncol = ncol)+
    ggplot2::geom_errorbar(aes(ymin = lower, ymax = upper))+
    ggplot2::geom_point(size = 2)+
    ggplot2::geom_point(aes(alpha = pzero, y = yposinflation), size =20) +
    ggplot2::geom_text(aes(y = yposinflation, label = pzero), vjust = 0.2, size = 3, color = 'black')+
    ggplot2::scale_color_discrete('')+
    ggplot2::guides(alpha = 'none')+
    ggplot2::theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())+
    ggplot2::guides(color = guide_legend(override.aes = list(size = 5)))+
    ggplot2::xlab('') +
    ggplot2::ylab(y_lab)
}
