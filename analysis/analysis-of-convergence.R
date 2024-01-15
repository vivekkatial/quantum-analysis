###############################################################################
# Analysis of convergence
#
# Author: Vivek Katial
# Created 2021-09-16 18:01:47
###############################################################################
library(tidyverse)
library(latex2exp)

plot <- d_runs %>% 
  select(
    "Random Initialisation" = metrics.QAOA_random_initialisation_approximation_ratio,
    "Trotterised Quantum Annealing" = metrics.QAOA_tqa_initialisation_approximation_ratio,
    "Instance Class Optimised" = metrics.QAOA_instance_class_optimsed_approximation_ratio,
    "Three Regular Graph Optimised" = metrics.QAOA_three_regular_graph_optimised_approximation_ratio
    ) %>% 
  gather(key, val) %>% 
  ggplot(aes(x = val, col = key , fill = key)) + 
  geom_density(alpha = 0.1) + 
  theme_classic() + 
  geom_vline(xintercept = 0.7156, linetype="dashed") + 
  annotate("text", x = 0.8, y=8, label = TeX("$\\alpha$=0.7156")) + 
  labs(
    x = TeX("Approximation Ratio      $\\alpha$"),
    y = ""
  ) + 
  scale_color_manual(values = c(
    "#832A8C", "#EDB432", "#DD5724", "#0E6FBB" 
  )) + 
  scale_fill_manual(
    values = c(
      "#832A8C", "#EDB432", "#DD5724", "#0E6FBB" 
  )) + 
  theme(
    axis.text.y=element_blank(), 
    axis.ticks.y=element_blank(),
    legend.title = element_blank(),
    legend.position=c(.14,.88)
  ) 
  

plot



ggsave("approximation-ratio-by-each-initialisation-histograms.png", plot, width = 10, height = 6, dpi = 300)
