
# tbl is a tidymodels object with a .pred_class column
summarise_correct_counts <- function(tbl){
  
  tbl %>%
  dplyr::mutate(
    correct = .pred_class==Development
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
    dplyr::rename(True_development=Development)
}


summarise_metrics <- function(tbl){
  
  n_correct <- summarise_correct_counts(tbl) %>%
    dplyr::rename(c(estimate_or_n=n, metric=correct))
  
  met1 <- tbl %>%
    yardstick::metrics(Development, .pred_class) 
  
  met2 <- tbl %>%
    yardstick::sens(Development, .pred_class)
  
  met3 <- tbl %>%
    yardstick::spec(Development, .pred_class)
  
  rbind(met1, met2, met3) %>%
    dplyr::select(-`.estimator`) %>%
    dplyr::rename(c(metric = `.metric`, estimate_or_n = `.estimate`)) %>%
    rbind(n_correct)
  
}

