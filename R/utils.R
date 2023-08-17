
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