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
  # write_csv("/Users/vivekkatial/Documents/AQTED/qui/src/components/aqted/isa/plot.csv")
  ggplot(aes(x = z_1, y = z_2, col = Source)) + 
  geom_point() + 
  theme_light() + 
  labs(
    x = "Z1",
    y = "Z2",
    title = "Instances Projected onto 2D"
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

d_probs <- read_csv("data/mlflow_instance.csv")


# Plot Probabilities ------------------------------------------------------


d_probs %>% 
  group_by(qubo_ind, layer) %>% 
  filter(
    energy == min(energy)
    ) %>% 
  distinct(qubo_ind, layer, p_success, restart) %>% 
  mutate(restart = as.factor(restart)) %>%
  ungroup() %>% 
  group_by(layer, qubo_ind) %>% 
  mutate(mean_prob = mean(p_success, na.rm = T)) %>% 
  ungroup() %>% 
  filter(layer < 10) %>% 
  mutate(subtour = paste0("Subtour: ", qubo_ind+1)) %>% 
  ggplot(aes(x = layer, y = p_success, group = restart)) + 
  geom_line(alpha = 0.3, aes(col = restart)) + 
  geom_line(aes(y=mean_prob)) + 
  theme_light() +
  labs(
    x = "Layer",
    y = "P(Success)",
    title = "Probability vs Num Layers in QAOA Circuit"
  ) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks=c(1,5,10)) +
  facet_wrap(~subtour, ncol = 1, scales = "free")

d_probs %>% 
  filter(qubo_ind == 0, restart == 1, layer %in% c(1,5,10)) %>% 
  arrange(probability) %>%
  mutate(state = str_replace(state, "\\[", "\\|")) %>% 
  mutate(state = str_replace(state, "\\]", "\\>")) %>% 
  ggplot(aes(x = reorder(state, -probability), y = probability)) + 
  geom_bar(stat="identity", na.rm = TRUE)+
  geom_hline(yintercept = 1/16, color="red") + 
  facet_wrap(~layer, ncol =1, drop = TRUE) +
  theme_light() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
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
