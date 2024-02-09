
get_categorical_vars <- function(data){
  
  # Categorical variables
  return(data %>% select_if(is.factor) %>% names())
  
}

get_numerical_vars <- function(data) {
  
  # Numeric variables
  return(data %>% select_if(is.numeric) %>% names())
  
}