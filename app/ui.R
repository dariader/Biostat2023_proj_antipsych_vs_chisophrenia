library(markdown)

navbarPage("Biostat2023",
           navbarMenu("Simulated Data Description",
                      tabPanel("Table with simulated data",
                               # generated data overview
                               DT::dataTableOutput("table")
                      ),
                      tabPanel("Numerical data",
                               selectInput("num_variable", "Select numeric variable", choices = numeric_variables),
                               checkboxInput("split_by_visit_num", "Split by 'visit'", value = FALSE),
                               radioButtons("plot_type", "Select plot type:",
                                            choices = c("Histogram" = "histogram", "Density" = "density", "Boxplot" = "boxplot"),
                                            selected = "histogram"),
                               plotlyOutput("num_plot")
                      ),
                      tabPanel("Categorical data",
                               selectInput("cat_variable", "Select categorical variable", choices = categorical_variables),
                               checkboxInput("split_by_visit", "Split by 'visit'", value = FALSE),
                               plotlyOutput("cat_barplot")
                      ),
                      tabPanel("Difference between visits",
                               plotlyOutput("difference")
                      ),
                      
           ),
           navbarMenu("Association",
                      tabPanel("Interactive Heatmap",
                               # generated data overview
                               selectInput("select_corr", "Select visit", choices = c('all','1','2')), # ,'difference'
                               #selectInput("select_generation", "Select generation", choices = c('all','1','2','3')),
                               plotlyOutput("corr")
                      ),
                      tabPanel("Scatterplot",
                               selectInput("scatter_x", "Select variable for X-axis", choices = numeric_variables),
                               selectInput("scatter_y", "Select variable for Y-axis", choices = numeric_variables),
                               checkboxInput("split_by_visit_scatter", "Split by 'visit'", value = FALSE),
                               plotOutput("scatterplot")
                      ),
                     # tabPanel("Hypothesis 1",
                               
                     # ),
                    #  tabPanel("Hypothesis 2",
                               
                    #  )
           ),
           tabPanel("Linear models",
                    #verbatimTextOutput("summary")
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("response_var", "Предсказываемая переменная:",
                                    choices = names(data),
                                    selected = grep("^Comp", names(data), value = TRUE)[1],
                                    multiple = FALSE),
                        uiOutput("predictor_selector"),
                        checkboxInput("interaction_checkbox", "Взаимодействие факторов", value = FALSE),
                        #actionButton("reset_button", "Сбросить предикторы")
                      ),
                      mainPanel(
                        tableOutput("regression_table"),
                        textOutput("r_squared_value"),
                        textOutput("adj_r_squared_value"),
                        textOutput("f_statistic"),  
                        textOutput("p_value_f_statistic")  
                      )
                    )
           ),

)