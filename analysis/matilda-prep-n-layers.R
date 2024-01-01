###############################################################################
# Prep stuff for matilda
#
# Author: Vivek Katial
# Created 2021-04-13 17:16:59
###############################################################################

library(tidyverse)
library(glue)

Sys.time()

RUN_DATETIME <- as.POSIXct("2023-01-23 11:45:00 AEDT", tz = "UTC")
system_size = 13

# Adding stuff for QAOA
d_runs <- read_csv("data/d_QAOA-Parameter-layers-vanilla.csv") %>% 
  filter(
    start_time > RUN_DATETIME
  ) %>% 
  select(Instances = run_id, Source = params.Source, contains("params"), contains("metrics.energy_QAOA_13")) %>% 
  select(-contains("size_7"), -metrics.energy_QAOA_13_8,-metrics.energy_QAOA_13_9, -metrics.energy_QAOA_13_10)

d_matilda <- d_runs %>%
  rename_at(
      vars(starts_with(glue("params."))), 
      list( ~ str_replace(., glue("params."), "feature_"))
    ) %>% 
    rename_at(
      vars(starts_with("metrics.energy_QAOA_13")),
      list(~ str_replace(.,"metrics.energy_QAOA_13" ,"algo_n_layers"))
    ) %>% 
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

d_matilda <- d_matilda[sample(1:nrow(d_matilda)), ]

d_matilda %>% 
  select(Instances, Source, starts_with("feature"), starts_with("algo")) %>% 
  write_csv("data/d_matilda.csv")


d_matilda %>% slice(120:236)

