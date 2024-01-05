###############################################################################
# Create ISA data for Instance Based Opts (with custom performance metric)
#
# Author: Vivek Katial
# Created At: 2024-01-01 20:03:42.62354
###############################################################################

library(tidyverse)
library(glue)
library(here)
source(here("utils/matilda-utils.R"))

# Set the threshold values for tiny and very high variance
tiny_threshold <- 1e-5
very_high_threshold <- 1e5
system_size = 12

# Adding stuff for QAOA
d_runs <- read_csv("data/d_QAOA-Instance-Based-Parameter-Optimization-with-Metrics.csv") %>% 
  filter(params.instance_size == system_size) %>% 
  rename(Source = Source...2) %>% 
  select(run_id, Source, contains("params"), contains("metric"), contains("algo"))

# Fix corrupt runs
d_corrupt_runs <- read_csv("data/corrupt-runs-with-Metrics.csv") %>% 
  # Filter only runs that dont have `NA` values
  filter_all(all_vars(!is.na(.)))

d_runs_combined <- left_join(d_runs, d_corrupt_runs, by = "run_id", suffix = c("_df1", "_df2"))

# Get the names of columns that have a '_df1' suffix
cols_df1 <- names(d_runs_combined)[str_detect(names(d_runs_combined), "_df1$")]

# Remove the '_df1' suffix to get the base column names
base_cols <- str_replace(cols_df1, "_df1$", "")

# Apply coalesce to each pair of columns
final_df <- d_runs_combined
for (col in base_cols) {
  col_df1 <- paste0(col, "_df1")
  col_df2 = paste0(col, "_df2")
  final_df <- final_df %>%
    mutate(!!sym(col) := coalesce(!!sym(col_df1), !!sym(col_df2)))
}

# Select only the original column names (without suffixes)
final_df <- final_df %>%
  select(-ends_with("_df2"), -ends_with("_df1")) %>% 
  filter(if_all(starts_with("algo_"), ~ !is.na(.))) %>% 
  filter(!params.is_distance_regular) %>% 
  rename(Instances = run_id)

# Get data on runs each algorithm -----------------------------------------

d_matilda <- final_df %>%
  rename_at(
    vars(starts_with(glue("params."))), 
    list( ~ str_replace(., glue("params."), "feature_"))
  ) %>% 
  mutate_if(is.logical, as.numeric) %>% 
  select(
    Instances,
    Source,
    contains("feature_"),
    base_cols[2:length(base_cols)]
  ) %>% 
  # Shift by absolute minimum to ensure all values for performance metric positive and remove NA
  drop_na() %>% 
  # Enable ONLY if algorithms should be positive
  # mutate_if(~is.numeric(.) && grepl("algo", names(.)), ~ . + abs(min(.))) %>% 
  # Enable if EVERYTHING should be positive
  mutate_if(~is.numeric(.), ~ . + abs(min(.))) %>% 
  # Normalis fevals
  select_if(~n_distinct(.) > 1) %>% 
  select(-contains("feature_acyclic")) %>% 
  select(-contains("feature_number_of_vertices")) %>%
  # Remove columns with very tiny, very high, and NaN value in varianc
  select(Instances, Source,starts_with("feature"),starts_with("algo"))


# Remove cols with no-variance
cols_to_remove <- d_matilda %>% 
  select(starts_with("feature")) %>%
  summarise(across(everything(), n_distinct)) %>% 
  gather(var, n_distinct) %>% 
  filter(n_distinct == 1) %>% 
  pull(var)


# Use dplyr functions to identify non-numeric columns
non_numeric_cols <- d_matilda %>%
  # remove selections for Instance and Source
  select(-Instances, -Source) %>% 
  select_if(negate(is.numeric)) %>%
  colnames()

# Print the names of non-numeric columns
if (length(non_numeric_cols) > 0) {
  cat("Non-numeric columns:", paste(non_numeric_cols, collapse = ", "), "\n")
  
  # Remove non-numeric columns from the data frame
  d_matilda <- d_matilda %>%
    select(-one_of(non_numeric_cols))
}


d_matilda <- d_matilda %>% 
  select(-cols_to_remove)

d_matilda %>% 
  count(Source)

# Test no missing values
testthat::expect_equal(d_matilda %>%
                         summarise_all(list( ~ sum(is.na(
                           .
                         )))) %>%
                         gather(var, n) %>%
                         pull(n) %>%
                         sum(),
                       0)

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
                       info = "There are feature columns with no variance"
)

# Test that atleast one source
testthat::expect_gt(d_matilda %>%
                      count(Source) %>%
                      nrow(),
                    1)

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

testthat::test_that("Check if all numeric columns are positive", {
  
  # Check if all numeric columns are positive
  numeric_cols <- d_matilda %>%
    select(-contains("algo_")) %>% 
    select_if(is.numeric) %>%
    sapply(function(x) all(x >= 0))
  
  # Assert that all numeric columns are positive
  testthat::expect_true(all(numeric_cols), label = "All numeric columns are positive")
})

# Sample this df
d_matilda <- d_matilda[sample(1:nrow(d_matilda)), ]

d_matilda %>% 
  select(Instances, Source, starts_with("feature"), starts_with("algo")) %>% 
  mutate(Instances = glue("{Instances}_{Source}")) %>% 
  write_csv("data/d_matilda.csv")
