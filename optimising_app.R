library(shiny)
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
          numericInput(inputId = "number_of_trees_to_build", label = "Number of trees", value = 100),
          numericInput(inputId = "random_predictors_per_node", label = "Random predictors per node", value = 20)
        ),
        column(
          width = 6,
          numericInput(inputId = "minimum_measures_per_node", label = "Minimum measures per node", value = 5),
          actionButton(inputId = "create_model", label = "Run model")
        )
      ),
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
  
  # Create model ----  
  model <- reactive({
    rand_forest(
      trees = input$number_of_trees_to_build, 
      min_n = input$minimum_measures_per_node, 
      mtry = input$random_predictors_per_node) %>%
        set_mode("classification") %>%
        set_engine("ranger")
  }) %>%
  bindEvent(input$create_model)
  
  # Train the model ----
  model_fit <- reactive({
    
    fit(model(), Development ~ ., data=training(split_data))
    
  })
  
  # Test the model ----
  ## Training counts ----
  training_counts <- reactive({
    
    model_fit() %>%
      predict(new_data=training(split_data)) %>%
      bind_cols(training(split_data)) %>%
      group_by(.pred_class, Development) %>%
      count() 
  })
  
  ## summary of training counts ----
  training_summary <- reactive({
    
    summarise_correct_counts(training_counts()) 
  })
  
  ## Test counts ----
  test_counts <- reactive({
    
    model_fit() %>%
      predict(new_data=testing(split_data)) %>%
      bind_cols(testing(split_data)) %>%
      group_by(.pred_class, Development) %>%
      count()
  })
  
  ## Summary of test counts ----
  test_summary <- reactive({
    
    summarise_correct_counts(test_counts())
  })
  
  # Output tables ----
  output$test_original_data <- DT::renderDataTable(
    training_counts(), caption = "Original training data", rownames = FALSE, options = list(dom = "t")
  )
  output$test_original_correct <- DT::renderDataTable(
    training_summary(), caption = "Summary of training data", rownames = FALSE, options = list(dom = "t")
  )
  output$test_new_data <- DT::renderDataTable(
    test_counts(), caption = "New test data", rownames = FALSE, options = list(dom = "t")
  )
  output$test_new_correct <- DT::renderDataTable(
    test_summary(), caption = "Summary of test data", rownames = FALSE, options = list(dom = "t")
  )
  
  # Output text ----
  output$model_info1 <- renderPrint({
    h2("Model information")
    model() %>% translate()
  })
  
  output$model_info2 <- renderPrint({
    model_fit() 
  })
  
}

shinyApp(ui, server)