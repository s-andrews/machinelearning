library(magrittr)

get_decision_tree <- function(){

  parsnip::decision_tree(tree_depth = 10) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("rpart") 
}

get_random_forest <- function(){
  
  parsnip::rand_forest(trees=100, min_n=5) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("ranger") 
}

get_nneighbour <- function(){
  
  parsnip::nearest_neighbor(neighbors = 20, weight_func = "triangular") %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("kknn") 
}

get_neuralnet <- function(){

  parsnip::mlp(epochs = 1000, hidden_units = 200, penalty = 0.01, learn_rate = 0.01) %>% 
    parsnip::set_engine("brulee", validation = 0) %>% 
    parsnip::set_mode("classification") 
}

