# Define fixed parameters
N <- 15
A <- 1000
near_zero_probability <- 0.001 # Probability assigned to a single near-zero group

# Function to calculate Pielou's Evenness (J')
calculate_J_prime <- function(abundances) {
  proportions <- abundances / sum(abundances)
  proportions[proportions == 0] <- 1e-12 
  shannon_entropy <- -sum(proportions * log(proportions))
  
  S <- N
  max_entropy <- log(S)
  
  j_prime <- shannon_entropy / max_entropy
  return(j_prime)
}

# ---------------------------------------------------

# Install and load required packages
# install.packages("gtools")
# install.packages("ggplot2")
library(gtools)
library(ggplot2)

# Set the number of simulations per P value
num_simulations_per_P <- 10000

# Create an empty data frame to store all results
all_results <- data.frame(J_prime = numeric(), P = factor())

# Outer loop to iterate through the number of near-zero groups (P)
for (P in 0:(N-1)) {
  
  # Inner loop for the simulations
  j_prime_values_for_P <- numeric(num_simulations_per_P)
  
  for (i in 1:num_simulations_per_P) {
    # Step 1: Create a probability vector with P near-zero groups
    prob_vector <- rep(0, N)
    
    if (P == 0) {
      prob_vector <- gtools::rdirichlet(1, rep(1, N))
    } else {
      near_zero_indices <- sample(1:N, P)
      prob_vector[near_zero_indices] <- near_zero_probability
      remaining_prob <- 1 - (P * near_zero_probability)
      non_near_zero_indices <- setdiff(1:N, near_zero_indices)
      prob_vector[non_near_zero_indices] <- gtools::rdirichlet(1, rep(1, N - P)) * remaining_prob
    }
    
    # Step 2: Simulate the abundance counts (composition)
    composition <- rmultinom(1, size = A, prob = prob_vector)
    
    # Step 3: Calculate J' and store the result
    j_prime_values_for_P[i] <- calculate_J_prime(composition)
  }
  
  # Append the results for this P to the main data frame
  new_data <- data.frame(J_prime = j_prime_values_for_P, P = factor(rep(P, num_simulations_per_P)))
  all_results <- rbind(all_results, new_data)
}

# ---------------------------------------------------

# Create a single faceted ggplot plot and save to file
ggplot(all_results, aes(x = J_prime)) +
  # Density curves for each P
  geom_density(aes(color = P), linewidth = 1) +
  # Overall density curve overlaid in black
  # geom_density(color = "black", linetype = "dashed", linewidth = 1, inherit.aes = FALSE,
               # data = all_results, aes(x = J_prime)) +
  labs(
    title = "Distribution of J' for Different Numbers of Near-Zero Groups",
    x = "Pielou's Evenness (H/H_max)",
    y = "Density"
  ) +
  theme_bw() +
  scale_color_discrete(name = "# 'extinct' Groups") +
  xlim(0, 1)

ggsave(here::here('data-raw',"evenness_test_density.png"), width = 10, height = 8)



ggplot(all_results, aes(x = P, y = J_prime))+
  geom_point()+
  labs(
    title = "J' as a function of the numbers of Near-Zero Groups",
    x = "# 'depleted' Groups",
    y = "Pielou's Evenness (H/H_max)"
  ) +
  theme_bw() +
  scale_color_discrete(name = "# 'extinct' Groups") +
  ylim(0, 1)
ggsave(here::here('data-raw',"evenness_test_depleted.png"), width = 10, height = 8)
