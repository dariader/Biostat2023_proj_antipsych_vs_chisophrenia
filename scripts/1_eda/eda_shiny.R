library(tidyverse)
library(ggplot2)
library(writexl)
library(openxlsx)
library(shiny)
library(FactoMineR)
library(ggfortify)
library(plotly)


source('scripts/0_preprocessing/preprocessing.R')

data_filtered <- preprocessing('data/Data_SAS.xlsx','scripts/_misc/Data_SAS_fixed.xlsx')

# Переводим числовые столбцы в числовой формат
numeric_columns <- c("age", "disease duration","antipsychotic dose", "THF dose", "gait", "arm dropping",
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
factor_columns <- c("gender", "visit", "antipsychotic",
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
    ),
    tabPanel("PCA",
             selectInput("pca_variable", "Выберите переменную для группировки", choices = categorical_variables),
             checkboxInput("split_by_visit_pca", "Разбить по 'visit'", value = TRUE),
             plotOutput("pca")
    ),
    tabPanel("Correlation",
             plotlyOutput("corr"))
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
  #corrplot(corr_matrix)
  #fviz_eig(data.pca, addlabels = TRUE)
  #fviz_cos2(data.pca, choice = "var", axes = 1:2)

  output$pca <- renderPlot({
    variable <- input$pca_variable
    split_by_visit_num <- input$split_by_visit_pca
    data_num <- data_filtered[,c(numeric_columns,factor_columns)]
    data_num <- data_num[complete.cases(data_num),]
    data_num[factor_columns] <- lapply(data_filtered[factor_columns], as.integer)

    data.pca <- princomp(cor(data_num))
    # if (split_by_visit_num) {
    #   gg <- gg + facet_wrap(~visit)
    # }

    #print(gg)
  })

  output$corr <- renderPlotly({
    cor_matrix <- cor(data_filtered[, numeric_columns])

    # Create a correlation plot using plot_ly
      plot <- plot_ly(
      x = colnames(cor_matrix),
      y = colnames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      colorscale = "Vulcano",
      colorbar = list(title = "Correlation")
    )

    # Customize layout
    layout <- list(
      title = "Correlation Plot",
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )

    # Combine plot and layout
    cor_plot <- layout(plot, layout)

    # Show the plot
    cor_plot
  })

}

# Запуск приложения Shiny
shinyApp(ui, server)

