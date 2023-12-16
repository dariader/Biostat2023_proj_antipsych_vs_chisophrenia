# Подключение библиотек
library(shiny)
library(lmtest)
library(car)
library(broom)
library(here)

# Ваши данные
path <- here("scripts/0_preprocessing", "preprocessing.R")
source(path)
path <- here("data", "Data_SAS.xlsx")
data_filtered <- preprocessing(path)

# Замена пробелов в названиях переменных подчеркиванием
names(data_filtered) <- gsub(" ", "_", names(data_filtered))

# UI-интерфейс приложения
ui <- fluidPage(
  titlePanel("Линейная регрессия в Shiny"),
  
  # Боковая панель с выбором предикторов и взаимодействия
  sidebarLayout(
    sidebarPanel(
      selectInput("response_var", "Предсказываемая переменная:",
                  choices = names(data_filtered),
                  selected = "Comp_Z",
                  multiple = FALSE),
      uiOutput("predictor_selector"),
      
      # Выбор взаимодействия
      checkboxInput("interaction_checkbox", "Взаимодействие факторов", value = FALSE),
      
      # Кнопка для сброса предикторов
      actionButton("reset_button", "Сбросить предикторы")
    ),
    
    # Основная панель с выводом регрессионной таблицы
    mainPanel(
      tableOutput("regression_table"),
      textOutput("r_squared_value"),
      textOutput("adj_r_squared_value")
    )
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
  
  # Вывод результатов линейной регрессии с использованием tidy
  output$regression_table <- renderTable({
    tidy_table <- tidy(regression_model())
    tidy_table
  })
  
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
}

# Запуск Shiny-приложения
shinyApp(ui = ui, server = server)