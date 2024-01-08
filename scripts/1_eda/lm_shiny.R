# Добавление библиотек
libraries_to_install <- c("shiny", "lmtest", "car", "broom", "here", "knitr")

# Подключение библиотек
install.packages(setdiff(libraries_to_install, rownames(installed.packages())))
library(shiny)
library(lmtest)
library(car)
library(broom)
library(here)
library(knitr)

lm_shiny <- function(data_path) {
  
  data_filtered <- data_path
  
  # Замена пробелов в названиях переменных подчеркиванием
  names(data_filtered) <- gsub(" ", "_", names(data_filtered))
  
  # UI-интерфейс приложения
  ui <- fluidPage(
    titlePanel("Линейная регрессия в Shiny"),
    
    # Вкладки
    tabsetPanel(
      tabPanel("Линейная регрессия",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("response_var", "Предсказываемая переменная:",
                               choices = names(data_filtered),
                               selected = grep("^Comp", names(data_filtered), value = TRUE)[1],
                               multiple = FALSE),
                   uiOutput("predictor_selector"),
                   checkboxInput("interaction_checkbox", "Взаимодействие факторов", value = FALSE),
                   actionButton("reset_button", "Сбросить предикторы")
                 ),
                 mainPanel(
                   tableOutput("regression_table"),
                   textOutput("r_squared_value"),
                   textOutput("adj_r_squared_value")
                 )
               )),
      tabPanel("Stepwise Regression",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("step_response_var", "Предсказываемая переменная:",
                               choices = names(data_filtered),
                               selected = grep("^Comp", names(data_filtered), value = TRUE)[1],
                               multiple = FALSE),
                   actionButton("add_all_button", "Добавить все факторы"),
                   actionButton("remove_all_button", "Убрать все факторы"),
                   hr(),
                   uiOutput("step_predictor_selector"),
                   actionButton("step_button", "Выполнить step()")
                 ),
                 mainPanel(
                   tableOutput("stepwise_table"),
                   textOutput("r_squared_value_stepwise"),
                   textOutput("adj_r_squared_value_stepwise")
                 )
               ))
    )
  )
  
  # Серверная часть приложения
  server <- function(input, output, session) {
    # Реактивное значение для отслеживания выбранных предикторов
    selected_predictors <- reactiveVal(character(0))
    
    # Обработка изменений в выборе предикторов
    observe({
      selected_predictors(input$predictors)
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
      lm(formula_str, data = data_filtered)
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
    
    # Создание динамического UI для выбора предикторов
    output$predictor_selector <- renderUI({
      selectInput("predictors", "Предикторы:",
                  choices = c("", names(data_filtered)),
                  selected = selected_predictors(),
                  multiple = TRUE)
    })
    
    # Обработка сброса предикторов при нажатии кнопки
    observeEvent(input$reset_button, {
      selected_predictors(character(0))
    })
    
    # Реактивное значение для отслеживания выбранных предикторов в step()
    selected_stepwise_predictors <- reactiveVal(names(data_filtered))
    
    # Обработка изменений в выборе предикторов в step()
    observe({
      selected_stepwise_predictors(input$step_predictors)
    })
    
    # Создание динамического UI для выбора предикторов в step()
    output$step_predictor_selector <- renderUI({
      selectInput("step_predictors", "Предикторы:",
                  choices = c("", setdiff(names(data_filtered), input$step_response_var)),
                  selected = selected_stepwise_predictors(),
                  multiple = TRUE)
    })
    
    # Обработка добавления всех факторов в step()
    observeEvent(input$add_all_button, {
      selected_stepwise_predictors(names(data_filtered))
      updateSelectInput(session, "step_predictors",
                        choices = c("", setdiff(names(data_filtered), input$step_response_var)),
                        selected = setdiff(names(data_filtered), input$step_response_var))
    })
    
    # Обработка удаления всех факторов из step()
    observeEvent(input$remove_all_button, {
      selected_stepwise_predictors(character(0))
      updateSelectInput(session, "step_predictors",
                        choices = c("", setdiff(names(data_filtered), input$step_response_var)),
                        selected = character(0))
    })
    
    # Обработка выполнения step()
    observeEvent(input$step_button, {
      selected_predictors <- selected_stepwise_predictors()
      
      # Проверка наличия выбранных факторов
      if (length(selected_predictors) == 0) {
        showModal(
          modalDialog(
            title = "Ошибка",
            "Выберите хотя бы один предиктор",
            easyClose = TRUE
          )
        )
        return()
      }
      
      formula_str <- as.formula(paste(input$step_response_var, "~", paste(selected_predictors, collapse = " + ")))
      
      # Функция для определения оттенка зеленого в зависимости от уровня значимости
      get_shade_of_green <- function(p_value) {
        if (p_value < 0.001) {
          return("#00FF00")  # Зеленый для уровня значимости < 0.001
        } else if (p_value < 0.01) {
          return("#66FF66")  # Светло-зеленый для уровня значимости < 0.01
        } else if (p_value < 0.05) {
          return("#99FF99")  # Средне-зеленый для уровня значимости < 0.05
        } else {
          return("#FFFFFF")  # Белый для всех остальных случаев
        }
      }
      
      step_model <- step(lm(formula_str, data = data_filtered))
      output$stepwise_table <- renderTable({
        tidy_table <- tidy(step_model)
        
        kable(tidy_table, "html", align = "c", escape = FALSE) %>%
          kable_styling("striped", full_width = FALSE) %>%
          row_spec(
            which(tidy_table$p.value < 0.001),
            background = "#00FF00"
          ) %>%
          row_spec(
            which(tidy_table$p.value >= 0.001 & tidy_table$p.value < 0.01),
            background = "#66FF66"
          ) %>%
          row_spec(
            which(tidy_table$p.value >= 0.01 & tidy_table$p.value < 0.05),
            background = "#99FF99"
          )
      }, sanitize.text.function = function(x) x)
      
      # Вывод R-квадрата
      output$r_squared_value_stepwise <- renderText({
        r_squared <- summary(step_model)$r.squared
        paste("R-squared: ", round(r_squared, 4))
      })
      
      # Вывод R-квадрата скорректированного
      output$adj_r_squared_value_stepwise <- renderText({
        adj_r_squared <- summary(step_model)$adj.r.squared
        paste("Adjusted R-squared: ", round(adj_r_squared, 4))
      })
    })
  }
  
  # Запуск Shiny-приложения
  shinyApp(ui = ui, server = server)
}