# Список библиотек для установки, если их нет
libraries_to_install <- c("tidyverse", "ggplot2", "writexl", "openxlsx", 
                          "shiny", "FactoMineR", "ggfortify", "plotly")

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
library(shiny)
library(FactoMineR)
library(ggfortify)
library(plotly)


eda_shiny <- function(data) {
  
  # Категориальные переменные
  categorical_variables <- data %>% select_if(is.factor) %>% names()
  
  # Нумерические переменные
  numeric_variables <- data %>% select_if(is.numeric) %>% names()
  
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
               radioButtons("plot_type", "Выберите тип графика:",
                            choices = c("Гистограмма" = "histogram", "Плотность" = "density", "Боксплот" = "boxplot"),
                            selected = "histogram"),
               plotOutput("num_plot")
      ),
      tabPanel("PCA",
               selectInput("pca_variable", "Выберите переменную для группировки", choices = categorical_variables),
               checkboxInput("split_by_visit_pca", "Разбить по 'visit'", value = TRUE),
               plotOutput("pca")
      ),
      tabPanel("Correlation",
               plotlyOutput("corr")
      ),
      tabPanel("Scatterplot",
               selectInput("scatter_x", "Выберите переменную по X", choices = numeric_variables),
               selectInput("scatter_y", "Выберите переменную по Y", choices = numeric_variables),
               checkboxInput("split_by_visit_scatter", "Разбить по 'visit'", value = FALSE),
               plotOutput("scatterplot")
      )
    )
  )
  
  # Server для приложения Shiny
  server <- function(input, output) {
    output$cat_barplot <- renderPlot({
      variable <- input$cat_variable
      split_by_visit <- input$split_by_visit
      
      gg <- ggplot(data, aes(x = !!as.symbol(variable))) +
        geom_bar() +
        labs(title = paste("Столбчатая диаграмма для", variable)) +
        theme_minimal()
      
      if (split_by_visit) {
        gg <- gg + facet_wrap(~visit)
      }
      
      print(gg)
    })
    
    output$num_plot <- renderPlot({
      variable <- input$num_variable
      split_by_visit_num <- input$split_by_visit_num
      plot_type <- input$plot_type
      
      gg <- ggplot(data, aes(x = !!as.symbol(variable))) +
        labs(title = paste("График для", variable))
      
      if (plot_type == "histogram") {
        gg <- gg + geom_histogram(fill = "skyblue", bins = 10)
      } else if (plot_type == "density") {
        gg <- gg + geom_density(fill = "skyblue")
      } else if (plot_type == "boxplot") {
        gg <- gg + geom_boxplot(fill = "skyblue") + coord_flip()
      }
      
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
      data_num <- data[,c(numeric_columns,factor_columns)]
      data_num <- data_num[complete.cases(data_num),]
      data_num[factor_columns] <- lapply(data[factor_columns], as.integer)
      
      data.pca <- princomp(cor(data_num))
      # if (split_by_visit_num) {
      #   gg <- gg + facet_wrap(~visit)
      # }
      
      #print(gg)
    })
    
    output$scatterplot <- renderPlot({
      x_variable <- input$scatter_x
      y_variable <- input$scatter_y
      split_by_visit_scatter <- input$split_by_visit_scatter
      
      gg <- ggplot(data, aes(x = !!as.symbol(x_variable), y = !!as.symbol(y_variable))) +
        geom_point(aes(col=gender, size=`antipsychotic generation`)) +
        geom_smooth(method="lm", col = 'black') +
        labs(title = paste("Scatterplot для", x_variable, "и", y_variable)) +
        theme_minimal()
      
      if (split_by_visit_scatter) {
        gg <- gg + facet_wrap(~visit)
      }
      
      correlation_coefficient <- cor(data[[x_variable]], data[[y_variable]])
      x_center <- mean(range(data[[x_variable]]))
      y_center <- mean(range(data[[y_variable]]))
      gg <- gg + annotate("text", x = x_center, y = y_center,
                          label = paste("r:", round(correlation_coefficient, 2)),
                          hjust = 0.5, vjust = 1, size = 6)
      
      print(gg)
    })
    
    output$corr <- renderPlotly({
      data_num <- data[,c(numeric_columns,factor_columns)]
      data_num[factor_columns] <- lapply(data[factor_columns], as.integer)
      cor_matrix <- cor(data_num)
      
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
}
