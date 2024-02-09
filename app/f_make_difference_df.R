make_dif_df <- function(data, numeric_variables, categorical_variables) {
  
  data[categorical_variables] <- lapply(data[categorical_variables], as.factor)
  data[numeric_variables] <- lapply(data[numeric_variables], as.numeric)
  
  # Выбираем только числовые переменные
  selected_variables <- data %>%
    select(!matches("P\\d{1}") & !matches("N\\d{1}")) %>% 
    select_if(is.numeric) %>% 
    names()
  
  # переменные которые не нужно вычитать
  add_df <- data %>% 
    filter(visit == 1) %>% 
    select(id, age, `disease.duration`, `THF.dose`, CPZE)
  
  # Создаем новый датафрейм для хранения разницы
  difference_df <- data %>%
    select(id, visit, all_of(selected_variables)) %>%
    group_by(id) %>%
    summarise(across(selected_variables, ~diff(.))) %>% 
    mutate(age = add_df$age,
           `disease.duration` = add_df$`disease.duration`,
           `THF.dose` = add_df$`THF.dose`, 
           CPZE = add_df$CPZE
    ) %>% 
    rename_with(~paste0(., "_dif"), -c(id, age, `disease.duration`, `THF.dose`, CPZE))  # Добавляем суффикс "_dif" 
  
  return(difference_df)
}
