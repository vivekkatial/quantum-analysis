###############################################################################
# Prep stuff for matilda
#
# Author: Vivek Katial
# Created 2021-04-13 17:16:59
###############################################################################

library(tidyverse)
library(glue)

Sys.time()

RUN_DATETIME <- as.POSIXct("2023-11-23 11:45:00 AEDT", tz = "UTC")
system_size = 8

# Adding stuff for QAOA
d_runs <- read_csv("data/d_QAOA-Parameter-layers-vanilla.csv") %>% 
  filter(
    start_time > RUN_DATETIME,
    status == "FINISHED"
  ) %>% 
  select(Instances = run_id, Source = params.instance_class, contains("params"), contains("metrics")) %>% 
  # Clean up random complex numbers coming through
  rowwise() %>% 
  mutate(across(starts_with("params.") & is.character, extract_real))



d_matilda <- d_runs %>%
  rename_at(
      vars(starts_with(glue("params."))), 
      list( ~ str_replace(., glue("params."), "feature_"))
    ) %>% 
  rename("algo_qaoa" = metrics.approximation_ratio,
         "algo_control" = metrics.analytical_approximation_ratio) %>% 
    mutate_if(is.logical, as.numeric) %>% 
    select_if(~n_distinct(.) > 1) %>% 
    select(-contains("feature_acyclic")) %>% 
    select(-contains("feature_number_of_vertices")) %>%
    drop_na() %>% 
    select(Instances, Source,starts_with("feature"),starts_with("algo"))


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
                       label = glue("Here are the issue columns {cols_to_check}")
)

# Test that atleast
testthat::expect_gt(d_matilda %>%
                      count(Source) %>%
                      nrow(),
                    1)

d_matilda %>% 
  filter(algo_control > 0) %>% 
  select(Instances, Source, starts_with("feature"), starts_with("algo")) %>% 
  janitor::clean_names() %>% 
  write_csv("data/d_matilda.csv")


