# column in dataset that contains the classification
pred_column <- "cell_type_topred"

# tbl is a tidymodels object with a .pred_class column
summarise_correct_counts <- function(tbl){
  
  tbl %>%
  dplyr::mutate(
    correct = .pred_class==cell_type_topred
  ) %>%
    dplyr::group_by(correct) %>%
    dplyr::summarise(
      n=sum(n)
    )
}

# count table of predictions
pivot_counts <- function(tbl){
  
  tbl %>%
    tidyr::pivot_wider(
      names_from=.pred_class,
      values_from=n,
      names_prefix = "predicted_"
    ) %>%
    #dplyr::rename(True_development=Development)
    dplyr::rename(True_pred=cell_type_topred)
}


# binding summary metrics into one tibble
summarise_metrics <- function(tbl_counts, tbl_pred){
  
  n_correct <- summarise_correct_counts(tbl_counts) %>%
    dplyr::rename(c(estimate_or_n=n, metric=correct))
  
  met1 <- tbl_pred %>%
    yardstick::metrics(cell_type_topred, .pred_class) 
  
  met2 <- tbl_pred %>%
    yardstick::sens(cell_type_topred, .pred_class)
  
  met3 <- tbl_pred %>%
    yardstick::spec(cell_type_topred, .pred_class)
  
  rbind(met1, met2, met3) %>%
    dplyr::select(-`.estimator`) %>%
    dplyr::rename(c(metric = `.metric`, estimate_or_n = `.estimate`)) %>%
    rbind(n_correct)
}

# counts table showing the true and false predictions
counts_table <- function(tbl, title){
  DT::datatable(
    pivot_counts(tbl), 
    caption = title, 
    rownames = FALSE, 
    options = list(dom = "t")
  )
}

# table to show the summarised metrics
summary_metrics_table <- function(tbl, title){
  DT::datatable(
    tbl, 
    caption = title, 
    rownames = FALSE, 
    options = list(dom = "t")
  ) %>%
    DT::formatRound(columns = "estimate_or_n", 3)
}




