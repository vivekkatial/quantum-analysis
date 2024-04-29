###############################################################################
# Custom Feature Selection and 
#
# Author: Vivek Katial
# Created At: 2024-03-21 10:18:13.020445
###############################################################################

# Read in processed X and plot distribution
  



is_outlier <- function(x, lower_pct = 0.01, upper_pct = 0.99) {
  lower_bound <- quantile(x, lower_pct, na.rm = TRUE)
  upper_bound <- quantile(x, upper_pct, na.rm = TRUE)
  !(x >= lower_bound & x <= upper_bound)
}

df <- d_matilda 

df <- df %>% 
  # Check all instances are connected
  filter(feature_connected == TRUE)

feats <- df %>%
  select(starts_with("feature")) %>% 
  names()

df %>% 
  select(starts_with("algo")) %>%
  # Plot the distribution of the algorithms
  gather(var, n) %>% 
  ggplot(aes(x= log(n))) + 
  geom_histogram(color="white") +
  facet_wrap(~var) + 
  theme_Publication()
  
  
  


read_csv("data/isa/qaoa-classical-opts-init/algorithm_bin.csv") %>% 
  select(-Row) %>% 
  # filter(!if_any(starts_with("feature"), ~is_outlier(., .01, .99)))  %>% 
  # Log transform the algorithms
  # mutate(across(starts_with("algo"), ~log(.))) %>% 
  # select(starts_with("feature")) %>%
  gather(var, n) %>%
  ggplot(aes(x = n)) +
  geom_histogram(color = "white") +
  facet_wrap(~var, scales="free")


# Custom Box-Cox transformation function
boxcox_transform <- function(x, lambda) {
  if (lambda == 0) {
    return(log(x))
  } else {
    return((x^lambda - 1) / lambda)
  }
}

# Function to find optimal lambda using Maximum Likelihood Estimation
find_optimal_lambda <- function(x) {
  # Ensure x contains only positive values for Box-Cox transformation
  if(min(x) <= 0) {
    x <- x + abs(min(x)) + 1e-5  # Shift x to be strictly positive
  }
  
  best_lambda <- NA
  best_log_likelihood <- -Inf
  lambda_sequence <- seq(-2, 2, by = 0.1)
  
  for(lambda in lambda_sequence) {
    transformed <- boxcox_transform(x, lambda)
    mu <- mean(transformed)
    sigma <- sd(transformed)
    if(sigma == 0) sigma <- 1e-6  # Prevent division by zero in dnorm
    log_likelihood <- sum(log(dnorm(transformed, mean = mu, sd = sigma)))
    
    if(log_likelihood > best_log_likelihood) {
      best_log_likelihood <- log_likelihood
      best_lambda <- lambda
    }
  }
  
  return(best_lambda)
}


# Apply to each column and store results
lambda_params <- sapply(
  df %>% select(starts_with("feat")) %>% select(-feature_connected, -feature_number_of_components), 
  find_optimal_lambda)

# Print the optimal lambda for each feature
print(lambda_params)