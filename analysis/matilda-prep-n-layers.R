###############################################################################
# Prep stuff for MATILDA `n_layers` for QAOA Parameters
#
# Author: Vivek Katial
# Created At: 2024-03-22 16:39:21.839379
###############################################################################

library(here)
library(tidyverse)
library(glue)

source(here("utils/matilda-utils.R"))

Sys.time()

# RUN_DATETIME <- as.POSIXct("2023-11-23 11:45:00 AEDT", tz = "UTC")

# Adding stuff for QAOA
d_runs <- read_csv("data/d_QAOA-Number-of-Layers_all_runs.csv") %>% 
  filter(
    status == "FINISHED"
  ) %>% 
  select(Instances = run_id, Source = params.instance_class, contains("params"), contains("metrics")) %>% 
  # Filter for instance size of 12
  filter(params.instance_size == 12) %>%
  # Clean up random complex numbers coming through
  rowwise() %>% 
  mutate(across(starts_with("params.") & is.character, extract_real))

d_clean_data <- d_runs %>%
  mutate(Instances = glue("{Instances}_{Source}")) %>%
  rename_at(vars(starts_with(glue("params."))),
            list(~ str_replace(., glue("params."), "feature_"))) %>%
  rename_at(vars(starts_with(glue("metrics.algo"))),
            list(~ str_replace(., glue("metrics.algo"), "algo"))) %>%
  mutate_if(is.logical, as.numeric) %>%
  select_if( ~ n_distinct(.) > 1) %>% 
  select_if( ~ n_distinct(.) > 1) %>%
  select(-feature_max_layers) %>%
  # select(-contains("feature_number_of_vertices")) %>%
  drop_na() %>%
  # Filter rows with `Inf` values
  filter_all(all_vars(!is.infinite(.)))

d_matilda <- d_clean_data %>% 
  select(Instances,
         Source,
         starts_with("feature"),
         starts_with("algo"))

# Test no missing values
testthat::expect_equal(d_matilda %>%
                         summarise_all(list( ~ sum(is.na(
                           .
                         )))) %>%
                         gather(var, n) %>%
                         pull(n) %>%
                         sum(),
                       0)

# Test that only numeric features are selected
testthat::test_that("Check if only numeric columns exist in df", {
  
  # Use dplyr functions to check if all columns are numeric
  result <- d_matilda %>%
    select_if(is.numeric) %>%
    ncol() == (ncol(d_matilda)-2)
  
  non_numeric_cols <- d_matilda %>%
    select(-Instances, -Source) %>% 
    select_if(negate(is.numeric)) %>%
    colnames()
  
  # Assert that the result is true
  testthat::expect_true(result, label = "All columns are numeric")
  
  if (length(non_numeric_cols) > 0) {
    cat("Non-numeric columns:", paste(non_numeric_cols, collapse = ", "), "\n")
  }
})

testthat::expect_equal(d_matilda %>% 
                         select(starts_with("feature")) %>%
                         summarise(across(everything(), n_distinct)) %>% 
                         gather(var, n_distinct) %>% 
                         filter(n_distinct == 1) %>% 
                         nrow(),
                       0,
                       info = "There are feature columns with no variance"
)



# Tests unique names
testthat::expect_equal(d_matilda %>%
                         count(Instances) %>%
                         pull(n) %>%
                         sum(),
                       d_matilda %>%
                         nrow(),
                       info = "Checking Unique Instances")





# Tests all feature columns have some variance

testthat::expect_equal(d_matilda %>% 
                         select(starts_with("feature")) %>%
                         summarise(across(everything(), n_distinct)) %>% 
                         gather(var, n_distinct) %>% 
                         filter(n_distinct == 1) %>% 
                         nrow(),
                       0,
                       info = "There are feature columns with no variance",
                       label = glue("Here are the issue columns")
)

# Tests feature columns dont have `Inf` values

testthat::test_that("No feature columns contain Inf values", {
  problematic_columns <- d_matilda %>% 
    select(starts_with("feature")) %>%
    summarise(across(everything(), ~any(is.infinite(.)))) %>%
    pivot_longer(everything(), names_to = "var", values_to = "contains_inf") %>%
    filter(contains_inf == TRUE) %>%
    pull(var)
  
  testthat::expect_true(length(problematic_columns) == 0,
                        info = glue("There are feature columns with Inf values: {toString(problematic_columns)}"))
})

# Test that atleast
testthat::expect_gt(d_matilda %>%
                      count(Source) %>%
                      nrow(),
                    1)

# Sample this df
d_matilda <- d_matilda[sample(1:nrow(d_matilda)), ]

algos_to_select <- c("algo_2_perf", "algo_5_perf", "algo_10_perf", "algo_15_perf", "algo_20_perf")

d_matilda %>% 
  select(Instances, Source, starts_with("feature"), starts_with("algo")) %>% 
  # Only select algo_1, algo_5, algo_10, algo_15 and algo_20
  select(Instances, Source, starts_with("feature"), algos_to_select) %>%
  mutate(Instances = glue("{Instances}_{Source}")) %>% 
  # Clean up source name to be tidy
  mutate(Source = stringr::str_to_title(str_replace_all(Source, "_", " "))) %>% 
  write_csv("data/d_matilda-n-layers-orig-all-size.csv")



d_matilda %>% 
  select(starts_with("algo")) %>% 
  gather(layer, feval) %>%
  # Extract layer number
  mutate(layer = str_extract(layer, "\\d+")) %>%
  filter(
    layer %in% c(2,5,10,15,20)
  ) %>% 
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = feval)) + 
  geom_boxplot(fill = "lightblue") + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  theme_Publication() + 
  labs(x = "Number of Layers", y = "# of Function Evaluations")


d_matilda %>% 
  select(starts_with("algo")) %>% 
  gather(layer, feval) %>%
  # Extract layer number
  mutate(layer = str_extract(layer, "\\d+")) %>%
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = log(feval))) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  theme_Publication() + 
  labs(x = "Number of Layers", y = "log(# of Function Evaluations)")

d_matilda %>% 
  select(starts_with("algo")) %>% 
  gather(layer, feval) %>%
  # Extract layer number
  mutate(layer = str_extract(layer, "\\d+")) %>%
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = feval)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  theme_Publication() + 
  labs(x = "Number of Layers", y = "# of Function Evaluations")

d_matilda %>% 
  select(Source, starts_with("algo")) %>% 
  gather(-Source, key=layer, val=feval) %>%
  # Extract layer number
  mutate(layer = str_extract(layer, "\\d+")) %>%
  # Remove '_' in Source and make title case
  mutate(Source = str_replace_all(Source, "_", " ") %>% str_to_title) %>%
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = feval)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  facet_wrap(~Source, scales="free_y") +
  theme_Publication() + 
  labs(x = "Number of Layers", y = "# of Function Evaluations")


d_matilda %>% 
  select(Source, starts_with("algo")) %>% 
  gather(-Source, key=layer, val=feval) %>%
  # Extract layer number
  mutate(layer = str_extract(layer, "\\d+")) %>%
  # Remove '_' in Source and make title case
  mutate(Source = str_replace_all(Source, "_", " ") %>% str_to_title) %>%
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = log(feval))) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  facet_wrap(~Source, scales="free_y") +
  theme_Publication() + 
  labs(x = "Number of Layers", y = "log(# of Function Evaluations)")


# Approximation Ratio -----------------------------------------------------

d_clean_data %>% 
  select(contains("approximation_ratio")) %>% 
  gather(key = "layer", value = "ar") %>%
  # Clean name
  mutate(layer = str_extract(layer, "\\d+")) %>% 
  mutate(layer = str_extract(layer, "\\d+")) %>%
  filter(ar > 0.5) %>% 
  filter(
    layer %in% c(2,5,10,15,20)
  ) %>% 
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = ar)) + 
  geom_boxplot(fill = "lightblue") + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  theme_Publication() + 
  labs(x = "Number of Layers", y = "Approximation Ratio") 

d_clean_data %>% 
  select(contains("approximation_ratio"), Source) %>% 
  gather(-Source, key = "layer", value = "ar") %>%
  # Clean name
  mutate(layer = str_extract(layer, "\\d+")) %>% 
  # Remove '_' in Source and make title case
  mutate(Source = str_replace_all(Source, "_", " ") %>% str_to_title) %>%
  filter(ar > 0.5) %>% 
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = ar)) +
  geom_boxplot(fill = "lightblue") + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  facet_wrap(~Source, scales="free_y") +
  theme_Publication() + 
  labs(x = "Number of Layers", y = "AR")


# P(Success) --------------------------------------------------------------

d_clean_data %>% 
  select(contains("probab")) %>% 
  gather(key = "layer", value = "p") %>%
  # Clean name
  mutate(layer = str_extract(layer, "\\d+")) %>% 
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = p)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  theme_Publication() + 
  labs(x = "Number of Layers", y = "P(Success)")

d_clean_data %>%
  select(contains("probab"), Source) %>% 
  gather(-Source, key = "layer", value = "p") %>%
  # Clean name
  mutate(layer = str_extract(layer, "\\d+")) %>% 
  # Remove '_' in Source and make title case
  mutate(Source = str_replace_all(Source, "_", " ") %>% str_to_title) %>%
  ggplot(aes(x = reorder(layer, as.numeric(layer)), y = p)) + 
  geom_boxplot() + 
  stat_summary(fun=mean, geom="line", aes(group=1), color="blue") + 
  stat_summary(fun=mean, geom="point", aes(group=1), color="blue", size=3, shape=1, stroke =1) + 
  facet_wrap(~Source, scales="free_y") +
  theme_Publication() + 
  labs(x = "Number of Layers", y = "P(Success)")

