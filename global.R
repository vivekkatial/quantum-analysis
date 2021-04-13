###############################################################################
# Entry point for app
#
# Author: Vivek Katial
# Created 2020-12-10 15:07:07
###############################################################################

library(tidyverse)
library(shiny)
library(echarts4r)


# Read in data
d_runs_raw <- read_rds("data/d_comparable.rds")

# Find feature names and clean them
feat_names_label <- d_runs_raw %>%
  names() %>%
  str_subset("f_") %>%
  str_remove("f_") %>%
  str_replace_all("_", " ") %>%
  stringi::stri_trans_toupper()

# Define feature names
feat_names <- d_runs_raw %>%
  names() %>%
  str_subset("f_")

# Create a named vector
feature_choices <- setNames(feat_names, feat_names_label)
