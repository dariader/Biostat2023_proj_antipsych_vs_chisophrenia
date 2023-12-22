# Список библиотек для установки, если их нет
libraries_to_install <- c("tidyverse")

# Проверка и установка библиотек
for (library_name in libraries_to_install) {
  if (!requireNamespace(library_name, quietly = TRUE)) {
    install.packages(library_name, dependencies = TRUE)
  }
}

library(tidyverse)
make_dif_df <- function(data) {

  numeric_columns <- c("age", "disease duration", "THF dose", "gait", "arm dropping",
                       "shoulder shaking", "elbow rigidity", "wrist rigidity", "head rotation",
                       "glabella tap", "tremor", "salivation", "akathisia", "Total score SAS",
                       "P1", "P2", "P3", "P4", "P5", "P6", "P7", "Positive scale",
                       "N1", "N2", "N3", "N4", "N5", "N6", "N7", "Negative scale",
                       "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10",
                       "G11", "G12", "G13", "G14", "G15", "G16", "General Psychopathology scale",
                       "Total score PANSS", "Verbal Memory", "ZVM", "Digit Sequencing", "ZDS",
                       "Token Motor Task", "ZMT", "Verbal Fluency", "ZVF", "Symbol Coding", "ZSC",
                       "Tower of London", "ZToL", "Comp Z")
  
  factor_columns <- c("gender", "visit", "antipsychotic",
                      "course", "education", "smoke", "antipsychotic generation")
  
  data[factor_columns] <- lapply(data[factor_columns], as.factor)
  data[numeric_columns] <- lapply(data[numeric_columns], as.numeric)
  
  # Выбираем только числовые переменные
  selected_variables <- data %>%
    select(!matches("P\\d{1}") & !matches("N\\d{1}")) %>% 
    select_if(is.numeric) %>% 
    names()
  
  # переменные которые не нужно вычитать
  add_df <- data %>% 
    filter(visit == 1) %>% 
    select(id, age, `disease duration`, `THF dose`, CPZE)
  
  # Создаем новый датафрейм для хранения разницы
  difference_df <- data %>%
    select(id, visit, all_of(selected_variables)) %>%
    group_by(id) %>%
    summarise(across(selected_variables, ~diff(.))) %>% 
    mutate(age = add_df$age,
           `disease duration` = add_df$`disease duration`,
           `THF dose` = add_df$`THF dose`, 
           CPZE = add_df$CPZE
    ) %>% 
    rename_with(~paste0(., "_dif"), -c(id, age, `disease duration`, `THF dose`, CPZE))  # Добавляем суффикс "_dif" 
  difference_df
}
