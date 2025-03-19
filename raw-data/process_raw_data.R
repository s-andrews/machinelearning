library(tidyverse)
#library(tidymodels)
#tidymodels_prefer()


read_delim("raw-data/hca_model_data.txt") -> data

# The predicted variable needs to be factor
data %>%
  mutate(cell_type_topred=factor(cell_type_topred)) %>%
  select(cell_type_topred,everything()) -> data

# We want to randomly shuffle the rows so there is no structure
set.seed(123)
data %>%
  sample_frac() -> data

head(data)

# We remove the cell ids since we're not using those and then split the rest into test/training

data %>%
  select(-cell_id) %>%
  initial_split(prop=0.8) -> split_data

split_data

saveRDS(split_data, file = "data/split_data.rds")
