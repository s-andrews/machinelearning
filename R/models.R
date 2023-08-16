library(tidyverse)
library(tidymodels)
tidymodels_prefer()

get_decision_tree <- function(){

  decision_tree(tree_depth = 10) %>%
    set_mode("classification") %>%
    set_engine("rpart") 
}

get_random_forest <- function(){
  
  rand_forest(trees=100, min_n=5) %>%
    set_mode("classification") %>%
    set_engine("ranger") 
}

get_nneighbour <- function(){
  
  nearest_neighbor(neighbors = 20, weight_func = "triangular") %>%
    set_mode("classification") %>%
    set_engine("kknn") 
}

get_neuralnet <- function(){

  mlp(epochs = 1000, hidden_units = 200, penalty = 0.01, learn_rate = 0.01) %>% 
    set_engine("brulee", validation = 0) %>% 
    set_mode("classification") 
}

