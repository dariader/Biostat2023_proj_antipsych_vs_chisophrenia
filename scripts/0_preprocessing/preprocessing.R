# Список библиотек для установки, если их нет
libraries_to_install <- c("tidyverse", "ggplot2", "writexl", "openxlsx", "here")

# Проверка и установка библиотек
for (library_name in libraries_to_install) {
  if (!requireNamespace(library_name, quietly = TRUE)) {
    install.packages(library_name, dependencies = TRUE)
  }
}

library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)
library(here)

output <- here("scripts/_misc", "Data_SAS_fixed.xlsx")
preprocessing <- function(filename) {
  data <- readxl::read_xlsx(filename, sheet = 2)
  data$'antipsychotic generation' <- sub("\\.0$", "", data$'antipsychotic generation')
  # Переводим числовые столбцы в числовой формат
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
  
  # все десятичные разделители делаем точками
  data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
    as.numeric(gsub(",", ".", x))
  })
  
  data[factor_columns] <- lapply(data[factor_columns], as.factor)

  data <- data %>%
    mutate(`Total score SAS` = rowSums(select(., gait:akathisia)),
           `Positive scale` = rowSums(select(., P1:P7)),
           `Negative scale` = rowSums(select(., N1:N7)),
           `General Psychopathology scale` = rowSums(select(., G1:G16)),
           `Total score PANSS` = `Positive scale` + `Negative scale` + `General Psychopathology scale`)  
  
  # Расчет Z-баллов по нормативным данным для российской популяции [Саркисян, Г. Р., Гурович, И. Я., & Киф, Р. С. (2010). Нормативные данные для российской популяции и стандартизация шкалы «Краткая оценка когнитивных функций у пациентов с шизофренией» (BACS). Социальная и клиническая психиатрия, 20 (3), 13-19.]
  data <- data %>%
    mutate(
      ZVM = case_when(
        gender == 'м' & age < 30 ~ round((`Verbal Memory` - 49.55) / 7.1, 2),
        gender == 'м' & age > 29 & age < 40 ~ round((`Verbal Memory` - 48.67) / 7.1, 2),
        gender == 'м' & age > 39 & age < 50 ~ round((`Verbal Memory` - 44.44) / 5.47, 2),
        gender == 'ж' & age < 30 ~ round((`Verbal Memory` - 50.52) / 7.94, 2),
        gender == 'ж' & age > 29 & age < 40 ~ round((`Verbal Memory` - 49.77) / 7.25, 2),
        gender == 'ж' & age > 39 & age < 50 ~ round((`Verbal Memory` - 47.00) / 6.35, 2),
        gender == 'ж' & age > 49 & age < 60 ~ round((`Verbal Memory` - 44.33) / 7.32, 2),
        TRUE ~ NA_real_
      ),
      ZDS = case_when(
        gender == 'м' & age < 30 ~ round((`Digit Sequencing` - 21.8) / 2.57, 2),
        gender == 'м' & age > 29 & age < 40 ~ round((`Digit Sequencing` - 22.14) / 3.35, 2),
        gender == 'м' & age > 39 & age < 50 ~ round((`Digit Sequencing` - 20.07) / 3.33, 2),
        gender == 'ж' & age < 30 ~ round((`Digit Sequencing` - 20.24) / 3.50, 2),
        gender == 'ж' & age > 29 & age < 40 ~ round((`Digit Sequencing` - 21.59) / 3.14, 2),
        gender == 'ж' & age > 39 & age < 50 ~ round((`Digit Sequencing` - 20.60) / 3.56, 2),
        gender == 'ж' & age > 49 & age < 60 ~ round((`Digit Sequencing` - 17.90) / 3.69, 2),
        TRUE ~ NA_real_
      ),
      ZMT = case_when(
        gender == 'м' & age < 30 ~ round((`Token Motor Task` - 74.7) / 8.8, 2),
        gender == 'м' & age > 29 & age < 40 ~ round((`Token Motor Task` - 71.43) / 12.15, 2),
        gender == 'м' & age > 39 & age < 50 ~ round((`Token Motor Task` - 74.30) / 11.58, 2),
        gender == 'ж' & age < 30 ~ round((`Token Motor Task` - 68.57) / 9.36, 2),
        gender == 'ж' & age > 29 & age < 40 ~ round((`Token Motor Task` - 72.18) / 8.86, 2),
        gender == 'ж' & age > 39 & age < 50 ~ round((`Token Motor Task` - 71.6) / 12.64, 2),
        gender == 'ж' & age > 49 & age < 60 ~ round((`Token Motor Task` - 68.86) / 11.65, 2),
        TRUE ~ NA_real_
      ),
      ZVF = case_when(
        gender == 'м' & age < 30 ~ round((`Verbal Fluency` - 58.4) / 9.46, 2),
        gender == 'м' & age > 29 & age < 40 ~ round((`Verbal Fluency` - 56.48) / 14.04, 2),
        gender == 'м' & age > 39 & age < 50 ~ round((`Verbal Fluency` - 62.00) / 16.76, 2),
        gender == 'ж' & age < 30 ~ round((`Verbal Fluency` - 54.8) / 14.53, 2),
        gender == 'ж' & age > 29 & age < 40 ~ round((`Verbal Fluency` - 60.45) / 11.05, 2),
        gender == 'ж' & age > 39 & age < 50 ~ round((`Verbal Fluency` - 58.2) / 10.7, 2),
        gender == 'ж' & age > 49 & age < 60 ~ round((`Verbal Fluency` - 58.29) / 10.79, 2),
        TRUE ~ NA_real_
      ),
      ZSC = case_when(
        gender == 'м' & age < 30 ~ round((`Symbol Coding` - 61.95) / 8.06, 2),
        gender == 'м' & age > 29 & age < 40 ~ round((`Symbol Coding` - 61.24) / 10.73, 2),
        gender == 'м' & age > 39 & age < 50 ~ round((`Symbol Coding` - 54.35) / 6.61, 2),
        gender == 'ж' & age < 30 ~ round((`Symbol Coding` - 65.71) / 6.09, 2),
        gender == 'ж' & age > 29 & age < 40 ~ round((`Symbol Coding` - 60.86) / 8.74, 2),
        gender == 'ж' & age > 39 & age < 50 ~ round((`Symbol Coding` - 57.4) / 7.55, 2),
        gender == 'ж' & age > 49 & age < 60 ~ round((`Symbol Coding` - 50.1) / 10.24, 2),
        TRUE ~ NA_real_
      ),
      ZToL = case_when(
        gender == 'м' & age < 30 ~ round((`Tower of London` - 18.45) / 1.82, 2),
        gender == 'м' & age > 29 & age < 40 ~ round((`Tower of London` - 19.1) / 1.67, 2),
        gender == 'м' & age > 39 & age < 50 ~ round((`Tower of London` - 18.35) / 2.98, 2),
        gender == 'ж' & age < 30 ~ round((`Tower of London` - 17.67) / 1.93, 2),
        gender == 'ж' & age > 29 & age < 40 ~ round((`Tower of London` - 17.64) / 1.71, 2),
        gender == 'ж' & age > 39 & age < 50 ~ round((`Tower of London` - 17.4) / 1.76, 2),
        gender == 'ж' & age > 49 & age < 60 ~ round((`Tower of London` - 16.48) / 2.36, 2),
        TRUE ~ NA_real_
      )
    )
  
  data <- data %>% mutate(
    `Comp Z` = round(rowSums(select(., ZVM:ZToL))/ 3.96, 2))
  
  # Расчет пятифакторной модели шищофрении по PANSS [Lindenmayer, J. P., Bernstein-Hyman, R., & Grochowski, S. (1994). A new five factor model of schizophrenia. The Psychiatric quarterly, 65(4), 299–322. https://doi.org/10.1007/BF02354306]
  data <- data %>% mutate(
    negative = N2 + N4 + N6 + N3 + N1 + G16,
    excitment = P4 + G14 + P7 + G4,
    cognitive = P2 + G10 + N5 + G5 + G11,
    positive = P1 + G9 + P5 + P6,
    depression = G2 + G3 + G6 + G1 + G15
  )
  
  # Расчет эквивалентов суточных доз антипсихотиков к 100 мг перорального хлорпромазина [Leucht, S., Samara, M., Heres, S., & Davis, J. M. (2016). Dose Equivalents for Antipsychotic Drugs: The DDD Method. Schizophrenia bulletin, 42 Suppl 1(Suppl 1), S90–S94. https://doi.org/10.1093/schbul/sbv167]
  data <- data %>%
    mutate(`antipsychotic dose` = as.numeric(gsub(",", ".", `antipsychotic dose`)),
           CPZE = case_when(
             antipsychotic == "арипипразол" ~ `antipsychotic dose` / 5,
             antipsychotic == "галоперидол" ~ `antipsychotic dose` / 2.67,
             antipsychotic == "зуклопентиксол" ~ `antipsychotic dose` / 10,
             antipsychotic %in% c("рисперидон", "карипразин") ~ `antipsychotic dose` / 1.67, # [Németh, György et al. “Cariprazine versus risperidone monotherapy for treatment of predominant negative symptoms in patients with schizophrenia: a randomised, double-blind, controlled trial.” Lancet (London, England) vol. 389,10074 (2017): 1103-1113. doi:10.1016/S0140-6736(17)30060-0]
             antipsychotic == "кветиапин" ~ `antipsychotic dose` / 133.33,
             antipsychotic == "оланзапин" ~ `antipsychotic dose` / 3.33,
             antipsychotic == "палиперидон" ~ `antipsychotic dose` / 2.0,
             antipsychotic == "трифлуоперазин" ~ `antipsychotic dose` / 6.67,
             antipsychotic == "флупентиксол" ~ `antipsychotic dose` / 2.0,
             TRUE ~ NA_real_
           )) %>%
    mutate(CPZE = round(CPZE, 2),
           `antipsychotic dose` = as.factor(`antipsychotic dose`))
  
  # запись в файл
  write.xlsx(data, file = output, sheetName="data_filtered", append=TRUE)
  return(data)
}
