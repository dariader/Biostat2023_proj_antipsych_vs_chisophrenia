
library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)

preprocessing <- function(df) {
  data <- readxl::read_xlsx(df, sheet = 2)
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
  
  # все десятичные разделители делаем точками
  data[numeric_columns] <- lapply(data[numeric_columns], function(x) {
    as.numeric(gsub(",", ".", x))
  })
  # запись в файл
  write.xlsx(data_filtered, file = "../../scripts/_misc/Data_SAS_fixed.xlsx", sheetName="data", append=TRUE)
}
