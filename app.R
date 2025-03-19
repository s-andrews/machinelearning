Sys.setenv(TORCH_INSTALL=1)
Sys.setenv(TORCH_HOME=path.expand("libtorch/"))
library(torch)

library(shiny)
library(tidyverse)
library(tidymodels)
# I also needed to install the packages ranger, kknn, brulee
# an interactive ok for pytorch
tidymodels_prefer()

split_data <- readRDS("data/split_data.rds")

# column in dataset that contains the classification
pred_column <- "cell_type_topred"

# App for selecting machine learning algorithm
ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  titlePanel("Machine learning course"),
  br(),
  
  sidebarLayout(
    sidebarPanel(width = 6,
      verticalLayout(
        fluidRow(
          column(
            width = 6, 
            selectInput(
              inputId = "model_selector",
              label = NULL,
              choices = list(
                "Decision tree" = "decisiontree", 
                "Random forest" = "randomforest", 
                "Nearest neighbour" = "nneighbour",
                "Neural network" = "neuralnet"
              )
            )
          ),
          column(
            width = 6, 
            actionButton(inputId = "create_model", label = "Run model")
          )
        ),
        br(),
        verbatimTextOutput(outputId = "model_info1"),
        verbatimTextOutput(outputId = "model_info2")
      )#,
    # actionButton(inputId = "browser", label = "browser")
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


server <- function(input, output) {

  observeEvent(input$browser, browser())
  
  # Create model ----
  model <- reactive({

    switch(input$model_selector,
      decisiontree = get_decision_tree(),
      randomforest = get_random_forest(),
      nneighbour = get_nneighbour(),
      neuralnet = get_neuralnet()
    )
  }) %>%
    bindEvent(input$create_model)
  
  # Train the model ----
  model_fit <- reactive({
    fit(model(), cell_type_topred ~ ., data=training(split_data))
    #fit(model(), !!pred_column ~ ., data=training(split_data))
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
      #group_by(.pred_class, !!pred_column) %>%
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

# Run the application 
shinyApp(ui = ui, server = server)
