# Define fixed parameters
N <- 15
A <- 1000
near_zero_probability <- (1/N)*0.01

# Define your reference proportion
# This can be any probability vector that sums to 1.
# Example: a highly uneven distribution
set.seed(10)
reference_proportion <-  gtools::rdirichlet(1, rep(1, N))

# Check that the reference proportion is valid
if (abs(sum(reference_proportion) - 1) > 1e-9 || length(reference_proportion) != N) {
  stop("Invalid reference proportion: must sum to 1 and have length N.")
}

# Function to calculate Evenness relative to a reference proportion
calculate_relative_evenness <- function(proportions_ref, proportions_obs) {
  # Calculate observed proportions
  proportions_obs[proportions_obs == 0] <- 1e-12 
  
  # Calculate observed Shannon entropy
  shannon_entropy_obs <- -sum(proportions_obs * log(proportions_obs))
  
  # Calculate reference Shannon entropy
  shannon_entropy_ref <- -sum(proportions_ref * log(proportions_ref))
  
  # Calculate relative evenness (normalized by the reference entropy)
  # A value of 1 means observed evenness equals reference evenness.
  # A value > 1 means observed evenness is greater than the reference.
  relative_evenness <- shannon_entropy_obs / shannon_entropy_ref
  
  return(relative_evenness)
}

calculate_KL_divergence <- function(proportions_ref, proportions_obs) {
  # Handle cases where proportions might be zero
  proportions_ref[proportions_ref == 0] <- 1e-12 
  proportions_obs[proportions_obs == 0] <- 1e-12 
  
  # The KL formula is sum(P_ref * log2(P_ref / P_obs))
  # Note the use of log2() to ensure the result is in bits
  kl_divergence <- sum(proportions_ref * log2(proportions_ref / proportions_obs))
  
  return(kl_divergence)
}

# Function to calculate Jensen-Shannon (JS) divergence
calculate_JS_divergence <- function(proportions_ref, proportions_obs) {
  # Calculate the average distribution (M)
  proportions_M <- (proportions_ref + proportions_obs) / 2
  
  # Calculate the two KL divergence terms
  kl_ref_to_M <- calculate_KL_divergence(proportions_ref, proportions_M)
  kl_obs_to_M <- calculate_KL_divergence(proportions_obs, proportions_M)
  
  # JS divergence is the average of the two KL divergences
  js_divergence <- (kl_ref_to_M + kl_obs_to_M) / 2
  
  return(js_divergence)
}


# Function to calculate Jeffreys divergence
calculate_Jeffreys_divergence <- function(proportions_ref, proportions_obs) {
  # Calculate the two KL divergence terms
  kl_ref_to_obs <- calculate_KL_divergence(proportions_ref, proportions_obs)
  kl_obs_to_ref <- calculate_KL_divergence(proportions_obs, proportions_ref)
  
  # Jeffreys divergence is the sum of the two KL divergences
  jeffreys_divergence <- kl_ref_to_obs + kl_obs_to_ref
  
  return(jeffreys_divergence)
}
# ---------------------------------------------------

# Install and load required packages
# install.packages("gtools")
# install.packages("ggplot2")
library(gtools)
library(ggplot2)

# Set the number of simulations per P value
num_simulations <- 10000

# Create an empty data frame to store all results
all_results <- data.frame(Relative_Evenness = numeric(),
                          KL_divergence= numeric(),
                          JS_divergence= numeric(),
                          Jef_divergence = numeric(),
                          P = factor())

# Outer loop to iterate through the number of near-zero groups (P)
for (P in 0:(N-1)) {
  
  # Inner loop for the simulations
  relative_evenness_for_P <- numeric(num_simulations)
  KL_divergence_for_P <- numeric(num_simulations)
  JS_divergence_for_P = numeric(num_simulations)
  Jef_divergence_for_P =numeric(num_simulations)
  
  for (i in 1:num_simulations) {
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
    
  # Step 3: Calculate J' and store the result
    
    relative_evenness_for_P[i] <- calculate_relative_evenness(reference_proportion,prob_vector)
    KL_divergence_for_P[i] <- calculate_KL_divergence(reference_proportion,prob_vector)
    JS_divergence_for_P[i] <- calculate_JS_divergence(reference_proportion,prob_vector)
    Jef_divergence_for_P[i] <- calculate_Jeffreys_divergence(reference_proportion,prob_vector)
    
  }
  
  # Append the results for this P to the main data frame
  new_data <- data.frame(Relative_Evenness = relative_evenness_for_P,
                         KL_divergence = KL_divergence_for_P,
                         JS_divergence = JS_divergence_for_P,
                         Jef_divergence = Jef_divergence_for_P,
                         P = factor(rep(P, num_simulations)))
  all_results <- rbind(all_results, new_data)
}

# ---------------------------------------------------

# Create a single faceted ggplot plot and save to file
ggplot(all_results, aes(x = Relative_Evenness)) +
  # Density curves for each P
  geom_density(aes(color = P), linewidth = 1) +
  # Overall density curve overlaid in black
  # geom_density(color = "black", linetype = "dashed", linewidth = 1, inherit.aes = FALSE,
  # data = all_results, aes(x = J_prime)) +
  labs(
    title = "Distribution of J' for Different Numbers of Near-Zero Groups",
    x = "Relative Evenness (H/H_ref)",
    y = "Density"
  ) +
  theme_bw() +
  geom_vline(xintercept = 1)+
  scale_color_discrete(name = "# 'depleted' Groups") 
  

ggsave(here::here('figures',"relative_evenness_test_density.png"), width = 10, height = 8)

ggplot(all_results, aes(x = KL_divergence)) +
  # Density curves for each P
  geom_density(aes(color = P), linewidth = 1) +
  # Overall density curve overlaid in black
  # geom_density(color = "black", linetype = "dashed", linewidth = 1, inherit.aes = FALSE,
  # data = all_results, aes(x = J_prime)) +
  labs(
    title = "Distribution of KL-divergence for Different Numbers of Near-Zero Groups",
    x = "KL Divergence",
    y = "Density"
  ) +
  theme_bw() +
  geom_vline(xintercept = 1)+
  scale_color_discrete(name = "# 'depleted' Groups") 
  
ggsave(here::here('figures',"KL_divergence_test_density.png"), width = 10, height = 8)

ggplot(all_results, aes(x = JS_divergence)) +
  # Density curves for each P
  geom_density(aes(color = P), linewidth = 1) +
  # Overall density curve overlaid in black
  # geom_density(color = "black", linetype = "dashed", linewidth = 1, inherit.aes = FALSE,
  # data = all_results, aes(x = J_prime)) +
  labs(
    title = "Distribution of JS-divergence for Different Numbers of Near-Zero Groups",
    x = "JS Divergence",
    y = "Density"
  ) +
  theme_bw() +
  geom_vline(xintercept = 1)+
  scale_color_discrete(name = "# 'depleted' Groups") 

ggsave(here::here('figures',"JS_divergence_test_density.png"), width = 10, height = 8)

ggplot(all_results, aes(x = Jef_divergence)) +
  # Density curves for each P
  geom_density(aes(color = P), linewidth = 1) +
  # Overall density curve overlaid in black
  # geom_density(color = "black", linetype = "dashed", linewidth = 1, inherit.aes = FALSE,
  # data = all_results, aes(x = J_prime)) +
  labs(
    title = "Distribution of Jeffreys divergence for Different Numbers of Near-Zero Groups",
    x = "Jeffreys Divergence",
    y = "Density"
  ) +
  theme_bw() +
  # geom_vline(xintercept = 1)+
  scale_color_discrete(name = "# 'depleted' Groups") 

ggsave(here::here('figures',"jeffreys_divergence_test_density.png"), width = 10, height = 8)

# all_results_mean = all_results |>
#   group_by(P) |>
#   summarize(Relative_Evenness_median = median(Relative_Evenness,na.rm=T),
#             KL_divergence_median = median(KL_divergence,na.rm=T),
#             JS_divergence_median)
  
ggplot(all_results, aes(x = P, y = Relative_Evenness))+
  geom_boxplot()+
  labs(
    title = "J' as a function of the numbers of Near-Zero Groups",
    x = "# 'depleted' Groups",
    y = "Relative  Evenness (H/H_ref)"
  ) +
  theme_bw() +
  geom_hline(yintercept = 1)+
  scale_color_discrete(name = "# 'extinct' Groups") 
  
ggsave(here::here('figures',"relative_evenness_test_depleted.png"), width = 10, height = 8)


ggplot(all_results, aes(x = P, y = KL_divergence))+
  geom_boxplot()+
  labs(
    title = "KL-Divergence as a function of the numbers of Near-Zero Groups",
    x = "# 'depleted' Groups",
    y = "KL Divergence"
  ) +
  theme_bw() +
  geom_hline(yintercept = 1)+
  scale_color_discrete(name = "# 'extinct' Groups") 

ggsave(here::here('figures',"KL_divergence_test_depleted.png"), width = 10, height = 8)

ggplot(all_results, aes(x = P, y = JS_divergence))+
  geom_boxplot()+
  labs(
    title = "JS-Divergence as a function of the numbers of Near-Zero Groups",
    x = "# 'depleted' Groups",
    y = "JS Divergence"
  ) +
  theme_bw() +
  geom_hline(yintercept = 1)+
  scale_color_discrete(name = "# 'depleted' Groups") 

ggsave(here::here('figures',"JS_divergence_test_depleted.png"), width = 10, height = 8)

ggplot(all_results, aes(x = P, y = Jef_divergence))+
  geom_boxplot()+
  labs(
    title = "Jeffrey's Divergence as a function of the numbers of Near-Zero Groups",
    x = "# 'depleted' Groups",
    y = "Jeffrey's Divergence"
  ) +
  theme_bw() +
  geom_hline(yintercept = 1)+
  scale_color_discrete(name = "# 'depleted' Groups") 

ggsave(here::here('figures',"jeffreys_divergence_test_depleted.png"), width = 10, height = 8)
