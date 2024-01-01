###############################################################################
# Analysis of convergence
#
# Author: Vivek Katial
# Created 2021-09-16 18:01:47
###############################################################################

library(tidyverse)

d_offset_low <- read_csv("../HAQC/data/results.csv")
d_offset_high <- read_csv("../HAQC/data/results_large_offset.csv")

d_offset_low %>% 
  group_by(optimizer) %>% 
  mutate(restart = ifelse(n_eval==1, total_evals, F)) %>% 
  ggplot(aes(x = total_evals, y = value)) + 
  geom_line(size = 0.3, color = "navy") + # , color = "#800000") + 
  geom_vline(aes(xintercept = restart), linetype="dashed", size=0.15) + 
  geom_vline(xintercept = 500, color = "black", size = 0.75) +
  geom_hline(yintercept = -18.2, color="maroon") + 
  facet_wrap(~optimizer, ncol = 1) + 
  theme_minimal() + 
  labs(
    x = "Function Evaluations",
    y = "Energy"
  )


d_offset_high %>% 
  group_by(optimizer) %>% 
  mutate(restart = ifelse(n_eval==1, total_evals, F)) %>% 
  filter(total_evals<1000) %>% 
  ggplot(aes(x = total_evals, y = value)) + 
  geom_line(size = 0.3, color = "navy") + # , color = "#800000") + 
  geom_vline(aes(xintercept = restart), linetype="dashed", size=0.15) + 
  geom_vline(xintercept = 1000, color = "black", size = 0.75) +
  geom_hline(yintercept = -180, color="maroon") + 
  facet_wrap(~optimizer, ncol = 1) + 
  theme_minimal() + 
  labs(
    x = "Function Evaluations",
    y = "Energy"
  )

d_offset_high %>% 
  group_by(optimizer) %>% 
  summarise(
    min_energy_high_offset = min(value),
    gap_high_offset = 1-min(value)/-180
    ) %>% 
  arrange(gap_high_offset) %>% 
  left_join(
    d_offset_low %>% 
      group_by(optimizer) %>% 
      summarise(
        min_energy = min(value),
        gap = 1-min(value)/-18.6
      ) %>% 
      arrange(gap)
  ) %>% 
  rename(
    min_energy_low_offset = min_energy,
    gap_low_offset = gap
  )



