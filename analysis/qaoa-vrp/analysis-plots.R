###############################################################################
# Analysis of Results
#
# Author: Vivek Katial
# Created 2021-07-06 12:45:10
###############################################################################

# Results
d_matilda <- read_csv("data/d_matilda.csv")

# Plot burden
d_matilda %>% 
  select(starts_with("algo")) %>% 
  gather(var, n) %>% 
  pull(n) %>% 
  qplot(bins=30, color = I("white")) +
  theme_light() + 
  labs(
    x = "Quantum Burden",
    y = "n"
  )

# Plot of log(burden)
d_matilda %>% 
  select(starts_with("algo")) %>% 
  gather(var, n) %>% 
  pull(n) %>% 
  log() %>% 
  qplot(bins=30, color = I("white")) +
  theme_light() + 
  labs(
    x = "log(Quantum Burden)",
    y = "n"
  )

# By P
d_matilda %>%
  select(starts_with("algo")) %>% 
  gather(depth, n) %>% 
  mutate(
    depth = str_remove(depth, "algo_layer_"),
    depth = str_remove(depth, "_quantum_burden"),
    depth = as.numeric(depth)
  ) %>% 
  ggplot(aes(x = n)) + 
  geom_histogram(bins = 30, color = "white") + 
  facet_wrap(~depth) +
  facet_wrap(~depth, ncol = 1) +
  theme_light()

# By log(P)
d_matilda %>%
  select(starts_with("algo")) %>% 
  gather(depth, n) %>% 
  mutate(
    depth = str_remove(depth, "algo_layer_"),
    depth = str_remove(depth, "_quantum_burden"),
    depth = as.numeric(depth)
  ) %>% 
  ggplot(aes(x = log(n))) + 
  geom_histogram(bins = 30, color = "white") + 
  facet_wrap(~depth, ncol = 1) +
  theme_light() 

d_matilda %>%
  select(Instances, starts_with("algo")) %>% 
  gather(depth, n, -Instances) %>% 
  mutate(
    depth = str_remove(depth, "algo_layer_"),
    depth = str_remove(depth, "_quantum_burden"),
    depth = as.numeric(depth)
    ) %>% 
  ggplot(aes(x = depth, y = n, group = Instances)) + 
  geom_line(alpha = 0.3) + 
  theme_light() +
  theme(
    legend.position = "none"
  )
