create_models <- function(settings){
  settings$inflation_block <- create_inflation_block(settings)
  settings$freevariance_block <- create_freevariance_block(settings)
  settings$correlation_block <- create_correlation_block(settings)

  # verzweigung einbauen die für leere 3 blöcke nur 1 modell schreibt
  model1 <- create_model1(settings)

  # create_model2() # model specs for each type
  # create_model3() # model specs for each type
  # create_model4() # model specs for each type
  # create_model5() # model specs for each type
  # create_model6() # model specs for each type
  list()
}

create_model1 <- function(settings){
  if (length(settings$inflation_block) == 0){
    return(character())
  }
  model1 <- c('MODEL:')
  for(i in seq(settings$nclasses)){
    if(i != 1) {model1 <- c(model1, paste0('[[classes > ', i-1, ']]'))}
    model1 <- c(model1, paste0('%CLASS', i, '%'), settings$inflation_block)
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
    model2 <- c(model2, paste0('%CLASS', i, '%'), settings$inflation_block, settings$freevariance_block)
    if(i != 1) {model2 <- c(model2, paste0('[[/classes > ', i-1, ']]'))}
  }
  model2
}

create_model3 <- function(settings){
  if (length(settings$inflation_block) == 0 && length(settings$correlate) == 0){
    return(character())
  }
  model3 <- c('MODEL:')
  if (length(settings$correlation_block) != 0) {model3 <- c(model3, '%OVERALL%', settings$correlation_block)}
  if (length(settings$inflation_block) != 0){
    for(i in seq(settings$nclasses)){
      if(i != 1) {model3 <- c(model3, paste0('[[classes > ', i-1, ']]'))}
      model3 <- c(model3, paste0('%CLASS', i, '%'), settings$inflation_block)
      if(i != 1) {model3 <- c(model3, paste0('[[/classes > ', i-1, ']]'))}
    }
  }
  model3
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
  if(length(settings$correlate) == 0){
    return(character())
  }
  correlation_block <- as.character(combn(settings$correlate, 2, function(x) paste0(x[1], ' WITH ', x[2], ';')))
  correlation_block
}
