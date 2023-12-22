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
    
    # Обработка добавления всех факторов в step()
    observeEvent(input$add_all_button, {
      selected_stepwise_predictors(names(data_filtered))
    })
    
    # Обработка удаления всех факторов из step()
    observeEvent(input$remove_all_button, {
      selected_stepwise_predictors(character(0))
    })
    
    # Создание динамического UI для выбора предикторов в step()
    output$step_predictor_selector <- renderUI({
      selectInput("step_predictors", "Предикторы:",
                  choices = c("", names(data_filtered)),
                  selected = selected_stepwise_predictors(),
                  multiple = TRUE)
    })
    
    # Обработка выполнения step()
    observeEvent(input$step_button, {
      selected_predictors <- selected_stepwise_predictors()
      formula_str <- as.formula(paste(input$step_response_var, "~", paste(selected_predictors, collapse = " + ")))
      
      # Проверка наличия переменных с одним уровнем
      excluded_predictors <- character(0)
      for (predictor in selected_predictors) {
        if (length(unique(data_filtered[[predictor]])) <= 1) {
          excluded_predictors <- c(excluded_predictors, predictor)
        }
      }
      
      # Исключение переменных с одним уровнем из выбранных
      selected_predictors <- setdiff(selected_predictors, excluded_predictors)
      
      if (length(selected_predictors) == 0) {
        # Если не осталось выбранных предикторов, выдаем сообщение
        showModal(modalDialog(
          title = "Ошибка",
          "Все выбранные предикторы содержат только одно значение и не могут быть включены в модель.",
          easyClose = TRUE
        ))
        return(NULL)
      }
      
      formula_str <- as.formula(paste(input$step_response_var, "~", paste(selected_predictors, collapse = " + ")))
      
      step_model <- step(lm(formula_str, data = data_filtered))
      output$stepwise_table <- renderTable({
        tidy_table <- tidy(step_model)
        kable(tidy_table, "html", align = "c", escape = FALSE) %>%
          kable_styling("striped", full_width = FALSE) %>%
          row_spec(which(tidy_table$p.value < 0.05), background = "#99FF99")  # Задайте цвет для значимых строк
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
      
      # Если были исключены переменные, сообщаем об этом пользователю
      if (length(excluded_predictors) > 0) {
        showModal(modalDialog(
          title = "Исключенные предикторы",
          paste("Следующие предикторы содержат только одно значение и были исключены из модели:", paste(excluded_predictors, collapse = ", ")),
          easyClose = TRUE
        ))
      }
    })
  }
  
  # Запуск Shiny-приложения
  shinyApp(ui = ui, server = server)
}

# Пример использования с фиктивными данными
data_example <- data.frame(
  Comp_1 = rnorm(100),
  Comp_2 = rnorm(100),
  Y = rnorm(100)
)

lm_shiny(data_example)