create_models <- function(settings){
  settings$inflation_block <- create_inflation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  settings$correlation_block <- create_correlation_block(settings)

  # TODO: What if all models are the same (e.g., only categorical variables?)
  model1 <- create_model1(settings)
  model2 <- create_model2(settings)
  model3 <- create_model3(settings)
  model4 <- create_model4(settings)
  model5 <- create_model5(settings)
  model6 <- create_model6(settings)

  list(model1, model2, model3, model4, model5, model6)
}

create_model1 <- function(settings){
  if (length(settings$inflation_block) == 0){
    return(character())
  }
  model1 <- c('MODEL:')
  for(i in seq(settings$nclasses)){
    if(i != 1) {model1 <- c(model1, paste0('[[classes > ', i-1, ']]'))}
    model1 <- c(model1, paste0('%CLASS#', i, '%'), settings$inflation_block)
    if(i != 1) {model1 <- c(model1, paste0('[[/classes > ', i-1, ']]'))}
  }
  model1
}

create_model2 <- function(settings){
  if (length(settings$inflation_block) == 0 && length(settings$freevariance_block) == 0){
    return(character())
  }
  model2 <- c('MODEL:')
  for(i in seq(settings$nclasses)){
    if(i != 1) {model2 <- c(model2, paste0('[[classes > ', i-1, ']]'))}
    model2 <- c(model2, paste0('%CLASS#', i, '%'), settings$inflation_block, settings$freevariance_block)
    if(i != 1) {model2 <- c(model2, paste0('[[/classes > ', i-1, ']]'))}
  }
  model2
}

create_model3 <- function(settings){
  if (length(settings$inflation_block) == 0 && length(settings$correlation_block) == 0){
    return(character())
  }
  model3 <- c('MODEL:')
  if (length(settings$correlation_block) != 0) {model3 <- c(model3, '%OVERALL%', settings$correlation_block)}
  if (length(settings$inflation_block) != 0){
    for(i in seq(settings$nclasses)){
      if(i != 1) {model3 <- c(model3, paste0('[[classes > ', i-1, ']]'))}
      model3 <- c(model3, paste0('%CLASS#', i, '%'), settings$inflation_block)
      if(i != 1) {model3 <- c(model3, paste0('[[/classes > ', i-1, ']]'))}
    }
  }
  model3
}

create_model4 <- function(settings){
  if (length(settings$inflation_block) == 0 && length(settings$correlation_block) == 0 && length(settings$freevariance_block) == 0){
    return(character())
  }
  model4 <- c('MODEL:')
  if (length(settings$correlation_block) != 0) {model4 <- c(model4, '%OVERALL%', settings$correlation_block)}
  if (length(settings$inflation_block) != 0 || length(settings$freevariance_block != 0)){
    for(i in seq(settings$nclasses)){
      if(i != 1) {model4 <- c(model4, paste0('[[classes > ', i-1, ']]'))}
      model4 <- c(model4, paste0('%CLASS#', i, '%'))
      if (length(settings$inflation_block) != 0) {model4 <- c(model4, settings$inflation_block)}
      if (length(settings$freevariance_block) != 0) {model4 <- c(model4, settings$freevariance_block)}
      if(i != 1) {model4 <- c(model4, paste0('[[/classes > ', i-1, ']]'))}
    }
  }
  model4
}

create_model5 <- function(settings){
  if (length(settings$inflation_block) == 0 && length(settings$correlation_block) == 0){
    return(character())
  }
  model5 <- c('MODEL:')
  for(i in seq(settings$nclasses)){
    if(i != 1) {model5 <- c(model5, paste0('[[classes > ', i-1, ']]'))}
    model5 <- c(model5, paste0('%CLASS#', i, '%'))
    if (length(settings$inflation_block) != 0) {model5 <- c(model5, settings$inflation_block)}
    if (length(settings$correlation_block) != 0) {model5 <- c(model5, settings$correlation_block)}
    if(i != 1) {model5 <- c(model5, paste0('[[/classes > ', i-1, ']]'))}
  }
  model5
}

create_model6 <- function(settings){
  if (length(settings$inflation_block) == 0 && length(settings$correlation_block) == 0 && length(settings$freevariance_block) == 0){
    return(character())
  }
  model6 <- c('MODEL:')
  for(i in seq(settings$nclasses)){
    if(i != 1) {model6 <- c(model6, paste0('[[classes > ', i-1, ']]'))}
    model6 <- c(model6, paste0('%CLASS#', i, '%'))
    if (length(settings$inflation_block) != 0) {model6 <- c(model6, settings$inflation_block)}
    if (length(settings$correlation_block) != 0) {model6 <- c(model6, settings$correlation_block)}
    if (length(settings$freevariance_block) != 0) {model6 <- c(model6, settings$freevariance_block)}
    if(i != 1) {model6 <- c(model6, paste0('[[/classes > ', i-1, ']]'))}
  }
  model6
}

create_inflation_block <- function(settings) {
  if(length(settings$inflated) == 0){
    return(character())
  }
  inflation_block <- c()
  for(var in settings$inflated){
    inflation_block <- c(inflation_block, paste0('[ ', var, '#1 ];'))
  }
  inflation_block
}

create_freevariance_block <- function(settings) {
  if(length(settings$freevariance) == 0){
    return(character())
  }
  freevariance_block <- c()
  for(var in settings$freevariance){
    freevariance_block <- c(freevariance_block, paste0(var, ';'))
  }
  freevariance_block
}

create_correlation_block <- function(settings) {
  if(length(settings$correlate) < 2){
    return(character())
  }
  correlation_block <- as.character(combn(settings$correlate, 2, function(x) paste0(x[1], ' WITH ', x[2], ';')))
  correlation_block
}
