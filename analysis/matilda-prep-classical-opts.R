###############################################################################
# Prep stuff for MATILDA Classical Optimisers for QAOA Parameters
#
# Author: Vivek Katial
# Created 2021-04-13 17:16:59
###############################################################################

library(here)
library(tidyverse)
library(glue)

source(here("utils/matilda-utils.R"))

Sys.time()

# RUN_DATETIME <- as.POSIXct("2023-11-23 11:45:00 AEDT", tz = "UTC")

# Adding stuff for QAOA
d_runs <- read_csv("data/d_QAOA-Classical-Optimization_all_runs.csv") %>% 
  filter(
    status == "FINISHED"
  ) %>% 
  select(Instances = run_id, Source = params.instance_class, contains("params"), contains("metrics")) %>% 
  # Clean up random complex numbers coming through
  rowwise() %>% 
  mutate(across(starts_with("params.") & is.character, extract_real))



d_matilda <- d_runs %>%
  mutate(Instances = glue("{Instances}_{Source}")) %>%
  rename_at(vars(starts_with(glue("params."))),
            list(~ str_replace(., glue("params."), "feature_"))) %>%
  rename_at(vars(starts_with(glue("metrics.algo"))),
            list(~ str_replace(., glue("metrics.algo"), "algo"))) %>%
  mutate_if(is.logical, as.numeric) %>%
  select_if( ~ n_distinct(.) > 1) %>%
  select(-contains("feature_acyclic")) %>%
  # select(-contains("feature_number_of_vertices")) %>%
  drop_na() %>%
  # Filter rows with `Inf` values
  filter_all(all_vars(!is.infinite(.))) %>%
  select(Instances,
         Source,
         starts_with("feature"),
         starts_with("algo")) %>% 
  # Remove algorithms that have similar performance or very poor performance
  select(
    -algo_AQGD,
    -algo_TNC,
    -algo_GradientDescent,
    -algo_SPSA,
    -algo_NFT
  )


# Remove cols with no-variance
cols_to_remove <- d_matilda %>% 
  select(starts_with("feature")) %>%
  summarise(across(everything(), n_distinct)) %>% 
  gather(var, n_distinct) %>% 
  filter(n_distinct == 1) %>% 
  pull(var)

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

d_matilda %>% 
  select(Instances, Source, starts_with("feature"), starts_with("algo")) %>% 
#   mutate(Instances = glue("{Instances}_{Source}")) %>% 
  # Clean up source name to be tidy
  mutate(Source = stringr::str_to_title(str_replace_all(Source, "_", " "))) %>% 
  write_csv("data/d_matilda_classical_opts.csv")

