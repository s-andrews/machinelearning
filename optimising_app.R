library(shiny)
library(shinyvalidate)
library(tidyverse)
library(tidymodels)

tidymodels_prefer()

split_data <- readRDS("data/split_data.rds")

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  titlePanel("Optimisation of Random Forest model"),
  br(),
  
  sidebarLayout(
    sidebarPanel(width = 6,
      fluidRow(
        column(
          width = 6, 
          numericInput(
            inputId = "number_of_trees_to_build", 
            label = "Number of trees",
            min = 2, 
            max = 200, 
            value = 100
          ),
          numericInput(
            inputId = "random_predictors_per_node", 
            label = "Random predictors per node", 
            value = 20,
            min = 2, 
            max = 90
          )
        ),
        column(
          width = 6,
          numericInput(
            inputId = "minimum_measures_per_node", 
            label = "Minimum measures per node", 
            value = 5,
            min = 1,
            max = 5
          ),
          actionButton(inputId = "create_model", label = "Run model")
        )
      ),
      #actionButton(inputId = "browser", label = "browser"),
      br(),
      verbatimTextOutput(outputId = "model_info1"),
      verbatimTextOutput(outputId = "model_info2")
    ), 
    mainPanel(width = 6,
      
      DT::dataTableOutput("test_original_data"),
      br(),
      DT::dataTableOutput("test_original_correct"),
      br(),
      DT::dataTableOutput("test_new_data"),
      br(),
      DT::dataTableOutput("test_new_correct")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$browser, browser())
  
  # Set up validator for numeric inputs ----
  iv <- InputValidator$new()
  iv$add_rule("number_of_trees_to_build", sv_between(2, 200))
  iv$add_rule("random_predictors_per_node", sv_between(2, 90))
  iv$add_rule("minimum_measures_per_node", sv_between(1, 20))
  iv$enable()
  
  # Create model ----  
  model <- reactive({
    
    req(iv$is_valid())
    
    rand_forest(
      trees = isolate(input$number_of_trees_to_build), 
      min_n = isolate(input$minimum_measures_per_node), 
      mtry = isolate(input$random_predictors_per_node)
    ) %>%
      set_mode("classification") %>%
      set_engine("ranger")
  }) %>%
    bindEvent(input$create_model)
  
  # Train the model ----
  model_fit <- reactive({
    
    fit(model(), cell_type_topred ~ ., data=training(split_data))
    
  })
  
  # Predictions ----
  training_predictions <- reactive({
    model_fit() %>%
      predict(new_data=training(split_data)) %>%
      bind_cols(training(split_data))
  })
  
  test_predictions <- reactive({
    model_fit() %>%
      predict(new_data=testing(split_data)) %>%
      bind_cols(testing(split_data))
  })
  
  
  # Test the model ----
  ## Training counts ----
  training_counts <- reactive({
    
    training_predictions() %>%
      group_by(.pred_class, cell_type_topred) %>%
      count() %>%
      ungroup() 
    
  })
  
  ## summary of training counts ----
  training_summary <- reactive({
    summarise_metrics(training_counts(), training_predictions())
  })
  
  ## Test counts ----
  test_counts <- reactive({
    
    test_predictions() %>%
      group_by(.pred_class, cell_type_topred) %>%
      count() %>%
      ungroup() 
  })
  
  ## Summary of test counts ----
  test_summary <- reactive({
    summarise_metrics(test_counts(), test_predictions())
  })
  
  # Output tables ----
  output$test_original_data <- DT::renderDataTable(
    counts_table(training_counts(), title = "Original training data") 
  )
  
  output$test_original_correct <- DT::renderDataTable({
    summary_metrics_table(training_summary(), title = "Summary of training data")
  })
  
  output$test_new_data <- DT::renderDataTable(
    counts_table(test_counts(), title = "New test data")
  )
  
  output$test_new_correct <- DT::renderDataTable(
    summary_metrics_table(test_summary(), title = "Summary of test data")
  )
  
  # Output text ----
  output$model_info1 <- renderPrint({
    model() %>% translate()
  })
  
  output$model_info2 <- renderPrint({
    model_fit() 
  })
  
}

shinyApp(ui, server)