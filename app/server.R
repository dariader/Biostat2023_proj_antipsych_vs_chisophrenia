function(input, output, session) {
  output$plot <- renderPlot({
    plot(data)
  })
  
  output$cat_barplot <- renderPlotly({
    variable <- input$cat_variable
    split_by_visit <- input$split_by_visit
    
    gg <- ggplot(data, aes(x = !!as.symbol(variable), fill = !!as.symbol(variable))) +
      geom_bar() +
      labs(title = paste("Barplot for", variable)) +
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
      labs(title = paste("Plot for", variable))
    
    if (plot_type == "histogram") {
      gg <- gg + geom_histogram(fill = "skyblue", bins = 10)
    } else if (plot_type == "density") {
      gg <- gg + geom_density(fill = "skyblue")
    } else if (plot_type == "boxplot") {
      gg <- ggplot(data) +
        labs(title = paste("Plot for", variable)) + geom_boxplot(aes(y = !!as.symbol(variable)), fill = "skyblue")
    }
    
    if (split_by_visit_num) {
      gg <- gg + facet_wrap(~visit)
    }
    
    if (split_by_visit_num & plot_type == "boxplot"){
      gg <-  ggplot(data, aes(x = visit, y = !!as.symbol(variable))) +
        labs(title = paste("Plot for", variable)) +
        geom_boxplot(fill = "skyblue")
    }
    
    ggplotly(gg)
  })
  
  output$difference <- renderPlotly({
    dif_plot <- difference_df %>%
      gather(variable, value, -id) %>%
      filter(!grepl("G[1-9]|N[1-7]|P[1-7]_dif$", variable)) %>%
      group_by(variable) %>%
      summarise(mean_value = mean(value, na.rm = TRUE)) %>%
      filter(grepl("_dif$", variable)) %>%
      mutate(variable = gsub("_dif$", "", variable)) %>%
      filter(variable %in% c("Total.score.PANSS", "tremor", "akathisia", "head.rotation", "Total.score.SAS", "ZVM", "ZTol", "ZMT", "ZVF", "Comp.Z")) %>% 
      mutate(variable = case_when(
        variable == "ZVM" ~ "Verbal Memory",
        variable == "ZMT" ~ "Motor Test",
        variable == "ZVF" ~ "Speech Fluency",
        variable == "ZTol" ~ "Tower of London",
        variable == "Comp.Z" ~ "Composite BACS Score",
        variable == "tremor" ~ "Tremor",
        variable == "akathisia" ~ "Akathisia",
        variable == "head.rotation" ~ "Head Rotation",
        variable == "Total.score SAS" ~ "Total score SAS",
        TRUE ~ variable
      )) %>%
      mutate(variable = factor(variable, levels = c("Total.score.PANSS", "Verbal Memory", "Motor Test", "Speech Fluency", "Tower of London", "Composite BACS Score", "Tremor", "Akathisia", "Head Rotation", "Total score SAS"))) %>%
      ggplot(aes(x = variable, y = mean_value, fill = factor(sign(mean_value)))) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = c("#6600FF", "#CC3333", "green"), name = "Sign", labels = c("Negative", "Zero", "Positive")) +
      labs(title = "Mean Deviation of Variables",
           x = "Variables",
           y = "Mean Deviation") +
      theme_minimal() +
      guides(fill = FALSE) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    ggplotly(dif_plot)
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
      # variables that should not be subtracted
      corr_data <- difference_df
    }
    
    #if(select_gen %in% '1'){
    #  corr_data <- corr_data[corr_data[,'antipsychotic.generation'] == 1,]
    #}
    #if(select_gen %in% '2'){
    #  corr_data <- corr_data[corr_data[,'antipsychotic.generation'] == 2,]
    #}
    #if(select_gen %in% '3'){
    #  corr_data <- corr_data[corr_data[,'antipsychotic.generation'] == 3,]
    #s}
    
    data_num <- corr_data[,c(selected_items)]
    
    # Assuming cor_matrix is your correlation matrix and clusters is the result of hierarchical clustering
    cor_matrix <- cor(data_num)
    dendrogram <- hclust(dist(1 - cor_matrix))
    clusters <- cutree(dendrogram, k = 3)
    
    # Reorder the correlation matrix based on the clustering
    sorted_cor_matrix <- cor_matrix[order.dendrogram(as.dendrogram(dendrogram)),
                                    order.dendrogram(as.dendrogram(dendrogram))]
    #color_intervals <- c(-5, -0.9, -0.3, 0.3, 0.9, 5)
    #colors <- c("green", "white", "white", "white", "white","red")
    
    # Create a clustered heatmap using Plotly
    cor_plot <- plot_ly(z = sorted_cor_matrix,
                        colorscale = 'Viridis',  # You can choose a different color scale
                        x = colnames(sorted_cor_matrix),
                        y = rownames(sorted_cor_matrix),
                        type = "heatmap")
    
    # Show the plot
    cor_plot
  })
  
  output$scatterplot <- renderPlot({
    x_variable <- input$scatter_x
    y_variable <- input$scatter_y
    split_by_visit_scatter <- input$split_by_visit_scatter
    correlation_method <- "spearman"
    gg <- ggplot(data, aes(x = !!as.symbol(x_variable), y = !!as.symbol(y_variable))) +
      geom_point(aes(col=gender)) + #, size=`antipsychotic generation`
      geom_smooth(method="lm", col = 'black') +
      labs(title = paste("Scatterplot for", x_variable, "and", y_variable)) +
      theme_minimal()
    x_center <- mean(range(data[[x_variable]]))
    y_center <- mean(range(data[[y_variable]]))
    
    if (split_by_visit_scatter) {
      gg <- gg + facet_grid(~visit) +
        geom_text(data = data %>% group_by(visit) %>%
                    summarize(cor = cor(!!as.symbol(x_variable), !!as.symbol(y_variable), method = correlation_method),
                              p_value = cor.test(!!as.symbol(x_variable), !!as.symbol(y_variable), method = correlation_method)$p.value,
                              ci_low = spearman.ci(data[[x_variable]], data[[y_variable]])$conf.int[1],
                              ci_high = spearman.ci(data[[x_variable]], data[[y_variable]])$conf.int[2]),
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
  
  
  output$summary <- renderPrint({
    summary(data)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(data)
  })
  
  # Реактивное значение для отслеживания выбранных предикторов
  selected_predictors <- reactiveVal(character(0))
  
  # Обработка изменений в выборе предикторов
  observe({
    selected_predictors(input$predictors)
  })
  
  # Создание динамического UI для выбора предикторов
  output$predictor_selector <- renderUI({
    selectInput("predictors", "Предикторы:",
                choices = c("", names(data)),
                selected = selected_predictors(),
                multiple = TRUE)
  })
  
  # Реактивный блок для проведения линейной регрессии
  regression_model <- reactive({
    if (length(selected_predictors()) == 0) {
      # Если не выбран ни один предиктор, строим модель по интерцепту
      formula_str <- as.formula(paste(input$response_var, "~ 1"))
    } else {
      # Добавляем взаимодействие, если выбран соответствующий чекбокс
      if (input$interaction_checkbox) {
        formula_str <- as.formula(paste(input$response_var, "~", paste(selected_predictors(), collapse = " * ")))
      } else {
        formula_str <- as.formula(paste(input$response_var, "~", paste(selected_predictors(), collapse = " + ")))
      }
    }
    lm(formula_str, data = data)
  })
  
  # Вывод результатов линейной регрессии с использованием tidy и kable
  output$regression_table <- renderTable({
    tidy_table <- tidy(regression_model())
    kable(tidy_table, "html", align = "c", escape = FALSE) %>%
      kable_styling("striped", full_width = FALSE) %>%
      row_spec(which(tidy_table$p.value < 0.05), background = "#99FF99")  # Задайте цвет для значимых строк
  }, sanitize.text.function = function(x) x)
  
  # Вывод R-квадрата
  output$r_squared_value <- renderText({
    r_squared <- summary(regression_model())$r.squared
    paste("R-squared: ", round(r_squared, 4))
  })
  
  # Вывод R-квадрата скорректированного
  output$adj_r_squared_value <- renderText({
    adj_r_squared <- summary(regression_model())$adj.r.squared
    paste("Adjusted R-squared: ", round(adj_r_squared, 4))
  })
  
  # Вывод F-статистики и p-value
  output$f_statistic <- renderText({
    regression_summary <- summary(regression_model())
    
    if ("fstatistic" %in% names(regression_summary)) {
      f_statistic <- regression_summary$fstatistic
      result <- paste("F-statistic: ", round(f_statistic[1], 4))
    } else {
      result <- " "
    }
    
    return(result)
  })
  
  output$p_value_f_statistic <- renderText({
    regression_summary <- summary(regression_model())
    
    if ("fstatistic" %in% names(regression_summary)) {
      p_value <- overall_p(regression_model())
      result <- paste("p-value: ", format(p_value, digits = 4))
    } else {
      result <- " "
    }
    
    return(result)
  })
  
  
}

# shinylive::export(appdir = "app", destdir = "docs")
# httpuv::runStaticServer("docs/", port=8008)