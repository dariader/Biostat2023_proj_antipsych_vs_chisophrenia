# Список библиотек для установки, если их нет
libraries_to_install <- c("tidyverse", "ggplot2", "writexl", "openxlsx", 
                          "shiny", "FactoMineR", "ggfortify", "plotly", "ggpubr", "RVAideMemoire")

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
library(ggpubr)
library(RVAideMemoire)

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
               plotlyOutput("cat_barplot")
      ),
      tabPanel("Числовые переменные",
               selectInput("num_variable", "Выберите числовую переменную", choices = numeric_variables),
               checkboxInput("split_by_visit_num", "Разбить по 'visit'", value = FALSE),
               radioButtons("plot_type", "Выберите тип графика:",
                            choices = c("Гистограмма" = "histogram", "Плотность" = "density", "Боксплот" = "boxplot"),
                            selected = "histogram"),
               plotlyOutput("num_plot")
      ),
      # tabPanel("PCA",
      #          selectInput("pca_variable", "Выберите переменную для группировки", choices = categorical_variables),
      #          checkboxInput("split_by_visit_pca", "Разбить по 'visit'", value = TRUE),
      #          plotOutput("pca")
      # ),
      tabPanel("Correlation",
               selectInput("select_corr", "Выберите визит", choices = c('all','1','2','difference')),
               selectInput("select_generation", "Выберите генерацию", choices = c('all','1','2','3')),
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
    output$cat_barplot <- renderPlotly({
    variable <- input$cat_variable
    split_by_visit <- input$split_by_visit

   gg <- ggplot(data, aes(x = !!as.symbol(variable), fill = !!as.symbol(variable))) +
        geom_bar() +
        labs(title = paste("Столбчатая диаграмма для", variable)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      
      if (split_by_visit) {
        gg <- gg + facet_wrap(~visit)
      }

    ggplotly(gg)
  })
    
    output$num_plot <- renderPlotly({
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
        gg <- ggplot(data) +
        labs(title = paste("График для", variable)) + geom_boxplot(aes(y = !!as.symbol(variable)), fill = "skyblue")
      }

      if (split_by_visit_num) {
        gg <- gg + facet_wrap(~visit)
      }

      if (split_by_visit_num & plot_type == "boxplot"){
        gg <-  ggplot(data, aes(x = visit, y = !!as.symbol(variable))) +
          labs(title = paste("График для", variable)) +
          geom_boxplot(fill = "skyblue")
      }


      ggplotly(gg)
    })

    output$scatterplot <- renderPlot({
      x_variable <- input$scatter_x
      y_variable <- input$scatter_y
      split_by_visit_scatter <- input$split_by_visit_scatter
      correlation_method <- "spearman"
      
      gg <- ggplot(data, aes(x = !!as.symbol(x_variable), y = !!as.symbol(y_variable))) +
        geom_point(aes(col=gender, size=`antipsychotic generation`)) +
        geom_smooth(method="lm", col = 'black') +
        labs(title = paste("Scatterplot для", x_variable, "и", y_variable)) +
        theme_minimal()
      x_center <- mean(range(data[[x_variable]]))
      y_center <- mean(range(data[[y_variable]]))
      
      if (split_by_visit_scatter) {
        gg <- gg + facet_wrap(~visit) +
          facet_grid(~visit) +
          geom_text(data = data %>% group_by(visit) %>%
                      summarize(cor = cor(!!as.symbol(x_variable), !!as.symbol(y_variable), method = correlation_method),
                                p_value = cor.test(!!as.symbol(x_variable), !!as.symbol(y_variable), method = correlation_method)$p.value,
                                ci_low <- spearman.ci(data[[x_variable]], data[[y_variable]])$conf.int[1],
                                ci_high <- spearman.ci(data[[x_variable]], data[[y_variable]])$conf.int[2]),
                    aes(label = paste("r: ", round(cor, 2), "\np-value: ", round(p_value, 4),
                                      paste("\nCI: [", round(ci_low, 2), ", ", round(ci_high, 2), "]"))),
                    x = x_center, y = y_center, hjust = 0.5, vjust = 0.5, size = 8) +
          annotate("text", x = Inf, y = -Inf, label = "*method: spearman", hjust = 1, vjust = 0, size = 5, color = "black")
      } else {
        cor_test_result <- cor.test(data[[x_variable]], data[[y_variable]], method = correlation_method)
        correlation_coefficient <- cor_test_result$estimate
        p_value <- cor_test_result$p.value
        ci_low <- spearman.ci(data[[x_variable]], data[[y_variable]])$conf.int[1]
        ci_high <- spearman.ci(data[[x_variable]], data[[y_variable]])$conf.int[2]
        gg <- gg + annotate("text", x = x_center, y = y_center,
                            label = paste("r: ", round(correlation_coefficient, 2), "\np-value: ", round(p_value, 4),
                                          paste("\nCI: [", round(ci_low, 2), ", ", round(ci_high, 2), "]")),
                            hjust = 0.5, vjust = 0.5, size = 8) +
          annotate("text", x = Inf, y = -Inf, label = "*method: spearman", hjust = 1, vjust = 0, size = 5, color = "black")
      }
      
      print(gg)
    })

    output$corr <- renderPlotly({
      select_corr <- input$select_corr
      select_gen <- input$select_generation
      # Define a regular expression pattern for items to exclude
      pattern_to_exclude <- "^(G|N|P)[0-9]+$"

      # Use grep to select items that do not match the pattern
      selected_items <- numeric_variables[!grepl(pattern_to_exclude, numeric_variables)]

      if(select_corr %in% 'all'){
        corr_data <- data
      }
      if(select_corr %in% '1'){
       corr_data <- data[data$visit==1,]
      }
       if(select_corr %in% '2'){
       corr_data <- data[data$visit==2,]
      }
      if(select_corr %in% 'difference'){
        # переменные которые не нужно вычитать
        add_df <- data %>%
          filter(visit == 1) %>%
          select(id, age, `disease duration`, `THF dose`, CPZE)
        selected_variables <- selected_items
        corr_data <- data %>%
      select(id, visit, all_of(selected_variables)) %>%
      group_by(id) %>%
      summarise(across(selected_variables, ~diff(.))) %>%
      mutate(age = add_df$age,
             `disease duration` = add_df$`disease duration`,
             `THF dose` = add_df$`THF dose`,
             CPZE = add_df$CPZE
             )
      }

    if(select_gen %in% '1'){
      corr_data <- corr_data[corr_data[,'antipsychotic generation'] == 1,]
    }
    if(select_gen %in% '2'){
      corr_data <- corr_data[corr_data[,'antipsychotic generation'] == 2,]
    }
    if(select_gen %in% '3'){
      corr_data <- corr_data[corr_data[,'antipsychotic generation'] == 3,]
    }

      data_num <- corr_data[,c(selected_items)]

      # Assuming cor_matrix is your correlation matrix and clusters is the result of hierarchical clustering
      cor_matrix <- cor(data_num)
      dendrogram <- hclust(dist(1 - cor_matrix))
      clusters <- cutree(dendrogram, k = 3)

      # Reorder the correlation matrix based on the clustering
      sorted_cor_matrix <- cor_matrix[order.dendrogram(as.dendrogram(dendrogram)),
                                      order.dendrogram(as.dendrogram(dendrogram))]
      color_intervals <- c(-5, -0.9, -0.3, 0.3, 0.9, 5)
      colors <- c("green", "white", "white", "white", "white","red")

      # Create a clustered heatmap using Plotly
      cor_plot <- plot_ly(z = sorted_cor_matrix,
                          colorscale = 'Viridis',  # You can choose a different color scale
                          x = colnames(sorted_cor_matrix),
                          y = rownames(sorted_cor_matrix),
                          type = "heatmap")


      # Show the plot
      cor_plot
    })
    
  }
  
  # Запуск приложения Shiny
  shinyApp(ui, server)
}
