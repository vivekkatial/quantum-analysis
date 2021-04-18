###############################################################################
# Script to debug JSON file
#
# Author: Vivek Katial
# Created 2021-04-14 13:22:51
###############################################################################
# 
# library(igraph)
# library(jsonlite)

library(tidyverse)

d_res <- read_csv("~/Documents/InstanceSpace/trial/coordinates.csv")
d_raw <- read_csv("data/d_matilda.csv")

#try = c(rep("n=1",10), rep("n=2",10), rep("n=3",10))

d_res %>% 
  left_join(d_raw, by = c("Row"="Instances"))%>% 
  select(Row, z_1, z_2, Source) %>% 
  ggplot(aes(x = z_1, y = z_2, col = Source)) + 
  geom_point() + 
  theme_minimal()

d_probs <- read_csv("data/mlflow_instance.csv")


# Plot Probabilities ------------------------------------------------------


d_probs %>% 
  group_by(qubo_ind, layer) %>% 
  filter(
    energy == min(energy),
    qubo_ind == 0
    ) %>% 
  distinct(qubo_ind, layer, p_success, restart) %>% 
  mutate(restart = as.factor(restart)) %>% 
  ungroup() %>% 
  ggplot(aes(x = layer, y = p_success, group = restart)) + 
  geom_line(alpha = 0.39) + 
  theme_minimal() +
  labs(
    x = "Layer",
    y = "P(Success)"
  )

d_probs %>% 
  filter(qubo_ind == 0, restart == 1, layer %in% c(1,5,10)) %>% 
  arrange(probability) %>% 
  # select(state) %>% 
  # mutate(
  #   state = str_remove_all(state, "\\[|\\]"),
  #   state = str_remove_all(state, " "),
  #   digit = strtoi(state, base = 2)
  #   ) %>% 
  # arrange(digit)
  ggplot(aes(x = factor(-probability), y = probability)) + 
  geom_bar(stat="identity", na.rm = TRUE)+
  geom_hline(yintercept = 1/512, color="red") + 
  facet_wrap(~layer, ncol =1, scales = "free_x", drop = TRUE) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) + 
  labs(
    x = "State",
    y = "Probability"
  )


read_csv("data/d_vrp-qaoa.csv") %>% 
  filter(metrics.m_num_layers  == 13) %>% 
  gather(var, val) %>% 
  View()
