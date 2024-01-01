###############################################################################
# RQAOA vs QAOA
#
# Author: Vivek Katial
# Created 2022-02-17 10:41:11
###############################################################################

library(tidyverse)

d_results <- tribble(
  ~circuit, ~p, ~prob_success,
  "RQAOA", 1, 0.066,
  "QAOA", 1, 0.05,
  "RQAOA", 3, 0.13,
  "QAOA", 3, 0.04731,
  "RQAOA", 5, 0.13,
  "QAOA", 5, 0.0166015625,
  "RQAOA", 10,  0.13,
  "QAOA", 10,  0.009765625,
)


d_results %>% 
  mutate(p = as.factor(p)) %>% 
  ggplot(aes(x = p, y = prob_success, fill = circuit)) + 
  geom_col(position = "dodge", color = "black")  + 
  theme_minimal() + 
  ylim(c(0, 0.5)) + 
  geom_text(aes(label = prob_success), position = position_dodge(width = .9), vjust = -1) + 
  labs(
    x = "Number of Layers",
    y = "Probability of Success",
    title = "Experimental Results RQAOA vs QAOA",
    subtitle = "Minimum number of variables for RQAOA=3, QAOA with 1000 shots"
  )


d_results %>% 
  mutate(p = as.factor(p)) %>% 
  ggplot(aes(x = p, y = prob_success, fill = circuit)) + 
  geom_col(position = "dodge", color = "black")  + 
  theme_minimal() + 
  ylim(c(0, 0.5)) + 
  geom_text(aes(label = prob_success), position = position_dodge(width = .9), vjust = -1) + 
  labs(
    x = "Number of Layers",
    y = "Probability of Success",
    title = "Experimental Results RQAOA vs QAOA",
    subtitle = "Minimum number of variables for RQAOA=3, QAOA with 1000 shots"
  ) + 
  geom_line(aes(col = circuit, group = circuit))
