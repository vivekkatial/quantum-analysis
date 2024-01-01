###############################################################################
# Script here 
#
# Author: Vivek Katial
# Created 2022-11-25 16:28:22
###############################################################################


library(tidyverse)
library(here)

d_matilda <- read_csv(here("data/d_matilda.csv"))

d_matilda %>% 
  filter(!str_detect(Instances, "c2e4")) %>% 
  # View()
  write_csv("data/d_matilda.csv")


d_matilda 
