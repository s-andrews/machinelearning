library(shiny)
library(tidyverse)
library(tidymodels)
# I also needed to install the packages ranger, kknn, brulee
# an interactive ok for pytorch
tidymodels_prefer()

split_data <- readRDS("data/split_data.rds")

# App for selecting machine learning algorithm
ui <- fluidPage(

  titlePanel("Machine learning course"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "model_selector",
        label = "Select model",
        choices = list(
          "Decision tree" = "decisiontree", 
          "Random forest" = "randomforest", 
          "Nearest neighbour" = "nneighbour",
          "Neural network" = "neuralnet"
        )
      ),
      br(),
      actionButton(inputId = "create_model", label = "Run model")#,
      #actionButton(inputId = "browser", label = "browser")
    ),
  
    mainPanel(
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
  model <- eventReactive(input$create_model, {

    switch(input$model_selector,
      decisiontree = get_decision_tree(),
      randomforest = get_random_forest(),
      nneighbour = get_nneighbour(),
      neuralnet = get_neuralnet()
    )
    
  })
  
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
    
    training_counts() %>%
      mutate(
        correct = .pred_class==Development
      ) %>%
      group_by(correct) %>%
      summarise(
        n=sum(n)
      )
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
    
    test_counts() %>%
      mutate(
        correct = .pred_class==Development
      ) %>%
      group_by(correct) %>%
      summarise(
        n=sum(n)
      )
  })
  
  # Output tables ----
  output$test_original_data <- DT::renderDataTable(
    training_counts(), rownames = FALSE, options = list(dom = "t")
  )
  output$test_original_correct <- DT::renderDataTable(
    training_summary(), rownames = FALSE, options = list(dom = "t")
  )
  output$test_new_data <- DT::renderDataTable(
    test_counts(), rownames = FALSE, options = list(dom = "t")
  )
  output$test_new_correct <- DT::renderDataTable(
    test_summary(), rownames = FALSE, options = list(dom = "t")
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
