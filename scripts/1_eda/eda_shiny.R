library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)
library(shiny)


data_filtered <- readxl::read_xlsx('../../scripts/_misc/Data_SAS_fixed.xlsx')

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
data_filtered[numeric_columns] <- lapply(data_filtered[numeric_columns], function(x) {
  as.numeric(gsub(",", ".", x))
})

# Переводим категориальные столбцы в факторы
factor_columns <- c("gender", "visit", "antipsychotic","antipsychotic dose", 
                    "course", "education", "smoke", "antipsychotic generation")

data_filtered[factor_columns] <- lapply(data_filtered[factor_columns], as_factor)
# Категориальные переменные
categorical_variables <- data_filtered %>% select_if(is.factor) %>% names()

# Нумерические переменные
numeric_variables <- data_filtered %>% select_if(is.numeric) %>% names()

# UI для приложения Shiny
ui <- fluidPage(
  tabsetPanel(
    tabPanel("Категориальные переменные",
             selectInput("cat_variable", "Выберите категориальную переменную", choices = categorical_variables),
             checkboxInput("split_by_visit", "Разбить по 'visit'", value = FALSE),
             plotOutput("cat_barplot")
    ),
    tabPanel("Числовые переменные",
             selectInput("num_variable", "Выберите числовую переменную", choices = numeric_variables),
             checkboxInput("split_by_visit_num", "Разбить по 'visit'", value = FALSE),
             plotOutput("num_histogram")
    )
  )
)

# Server для приложения Shiny
server <- function(input, output) {
  output$cat_barplot <- renderPlot({
    variable <- input$cat_variable
    split_by_visit <- input$split_by_visit
    
    gg <- ggplot(data_filtered, aes(x = !!as.symbol(variable))) +
      geom_bar() +
      labs(title = paste("Столбчатая диаграмма для", variable)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (split_by_visit) {
      gg <- gg + facet_wrap(~visit)
    }
    
    print(gg)
  })
  
  output$num_histogram <- renderPlot({
    variable <- input$num_variable
    split_by_visit_num <- input$split_by_visit_num
    
    gg <- ggplot(data_filtered, aes(x = !!as.symbol(variable))) +
      geom_histogram(fill = "skyblue", bins = 10) +
      labs(title = paste("Гистограмма для", variable)) +
      theme_minimal()
    
    if (split_by_visit_num) {
      gg <- gg + facet_wrap(~visit)
    }
    
    print(gg)
  })
}

# Запуск приложения Shiny
shinyApp(ui, server)

