# Define fixed parameters
N <- 10
A <- 1000
set.seed(10)
ref_prop <- gtools::rdirichlet(1, rep(1, N)) # Your reference proportion
near_zero_prop <- (1/N)*0.001 # Fixed small probability for near-zero groups

# Check that the reference proportion is valid
if (abs(sum(ref_prop) - 1) > 1e-9 || length(ref_prop) != N) {
  stop("Invalid reference proportion: must sum to 1 and have length N.")
}

# --- Utility Functions ---
# Helper function to calculate KL divergence
calculate_KL_divergence <- function(proportions_ref, proportions_obs) {
  proportions_ref[proportions_ref == 0] <- 1e-12 
  proportions_obs[proportions_obs == 0] <- 1e-12 
  kl_divergence <- sum(proportions_ref * log2(proportions_ref / proportions_obs))
  return(kl_divergence)
}

# Function to calculate JS divergence
calculate_JS_divergence <- function(proportions_ref, proportions_obs) {
  proportions_M <- (proportions_ref + proportions_obs) / 2
  kl_ref_to_M <- calculate_KL_divergence(proportions_ref, proportions_M)
  kl_obs_to_M <- calculate_KL_divergence(proportions_obs, proportions_M)
  js_divergence <- (kl_ref_to_M + kl_obs_to_M) / 2
  return(js_divergence)
}

# Function to calculate Jeffreys divergence
calculate_J_divergence <- function(proportions_ref, proportions_obs) {
  kl_ref_to_obs <- calculate_KL_divergence(proportions_ref, proportions_obs)
  kl_obs_to_ref <- calculate_KL_divergence(proportions_obs, proportions_ref)
  j_divergence <- kl_ref_to_obs + kl_obs_to_ref
  return(j_divergence)
}

# --- Unified Systematic Simulation ---
library(dplyr)
library(gtools)

# Define the simulation parameters for dominance
dom_prop_total_steps <- seq(0.5, 1.0, by = 0.1) # Total proportion for dominant groups

# Create an empty data frame to store all results
all_results <- data.frame(
  Composition_Key = character(),
  JS_Divergence = numeric(),
  J_Divergence = numeric(),
  stringsAsFactors = FALSE
)

for (num_dom in 0:N) {
  # Get all combinations of group identities for dominant groups
  dom_group_combos <- if (num_dom > 0) t(combn(1:N, num_dom)) else matrix(NA, nrow = 1, ncol = 0)
  
  # Loop over the number of near-zero groups
  for (num_near_zero in 0:N) {
    
    # Skip invalid combinations where group counts exceed N
    if (num_dom + num_near_zero > N) {
      next
    }
    
    # Determine the number of "normal" groups
    num_normal <- N - num_dom - num_near_zero
    
    # Get the pool of groups available for near-zero designation
    near_zero_groups_pool <- setdiff(1:N, dom_group_combos[1,]) # use the first row as a template
    
    # Add the check here: skip if the pool is too small for the number of near-zero groups
    if (length(near_zero_groups_pool) < num_near_zero) {
      next
    }
    
    # Get all combinations of group identities for near-zero groups
    near_zero_group_combos <- if (num_near_zero > 0) t(combn(near_zero_groups_pool, num_near_zero)) else matrix(NA, nrow = 1, ncol = 0)
    
    # Loop over all combinations of dominant and near-zero groups
    for (i in 1:nrow(dom_group_combos)) {
      dominant_groups <- if (num_dom > 0) dom_group_combos[i,] else integer(0)
      
      for (j in 1:nrow(near_zero_group_combos)) {
        near_zero_groups <- if (num_near_zero > 0) near_zero_group_combos[j,] else integer(0)
        
        normal_groups <- setdiff(1:N, c(dominant_groups, near_zero_groups))
        
        # Loop over the level of dominance
        for (dom_prop_total in dom_prop_total_steps) {
          
          # Skip if dominance level doesn't make sense for the group count
          if (num_dom == 0 && dom_prop_total != 0) next
          if (num_normal == 0 && dom_prop_total < 1 - (num_near_zero * near_zero_prop)) next
          
          # Initialize proportions
          composition_proportions <- rep(0, N)
          
          # Assign fixed proportion to near-zero groups
          if (num_near_zero > 0) {
            composition_proportions[near_zero_groups] <- near_zero_prop
          }
          
          # Distribute proportions to dominant groups
          if (num_dom > 0) {
            dom_props <- gtools::rdirichlet(1, rep(1, num_dom)) * dom_prop_total
            composition_proportions[dominant_groups] <- dom_props
          }
          
          # Distribute the rest to normal groups
          if (num_normal > 0) {
            remaining_for_normal <- 1 - sum(composition_proportions)
            normal_props <- gtools::rdirichlet(1, rep(1, num_normal)) * remaining_for_normal
            composition_proportions[normal_groups] <- normal_props
          }
          
          # Calculate and store results
          js_div <- calculate_JS_divergence(ref_prop, composition_proportions)
          j_div <- calculate_J_divergence(ref_prop, composition_proportions)
          
          comp_key <- paste(
            num_dom, num_near_zero,
            paste(sort(dominant_groups), collapse = "-"),
            paste(sort(near_zero_groups), collapse = "-"),
            round(dom_prop_total, 2),
            sep = "_"
          )
          
          all_results <- all_results %>%
            add_row(
              Composition_Key = comp_key,
              JS_Divergence = js_div,
              J_Divergence = j_div
            )
        }
      }
    }
  }
}

all_results = all_results |>
  tidyr::separate(
    Composition_Key,
    into = c("num_dom", "num_near_zero", "dominant_groups", "near_zero_groups", "dom_prop_total"),
    sep = "_",
    convert = TRUE
  )

saveRDS(all_results2,here::here('data-raw','divergence_testing.rds'))


ggplot(all_results, aes(x = JS_Divergence))+
  geom_histogram(binwidth = 0.01)+
  labs(
    x = 'JS_Divergence',
    y = 'Frequency',
    title = 'JS-Divergence for all permutations'
  )+
  theme_bw()
ggsave(here::here('figures','JS_divergence_hist_all.png'),width =10,height =8)
  
ggplot(all_results, aes(x = J_Divergence))+
  geom_histogram(binwidth = 0.1)+
  labs(
    x = 'J_Divergence',
    y = 'Frequency',
    title = "Jeffrey's Divergence for all permutations"
  )+
  theme_bw()
ggsave(here::here('figures','Jeffreys_divergence_hist_all.png'),width =10,height =8)

# --- Aggregate data for heatmap ---
# Group by num_dom and dom_prop_total and calculate the mean JS_Divergence
heatmap_data <- all_results %>%
  group_by(num_dom, num_near_zero, dom_prop_total) %>%
  summarise(
    Mean_JS_Divergence = mean(JS_Divergence, na.rm = TRUE),
    Mean_J_Divergence = mean(J_Divergence, na.rm=T),
    .groups = 'drop'
  )

# --- Create and save the heatmap ---
ggplot(heatmap_data, aes(x = factor(num_dom), y = factor(dom_prop_total), fill = Mean_JS_Divergence)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Mean JS Divergence") +
  facet_wrap(~ num_near_zero, labeller = as_labeller(function(x) paste0("P = ", x))) +
  labs(
    title = "JS Divergence Heatmap",
    subtitle = "Faceted by the number of near-zero groups (P)",
    x = "Number of Dominant Groups",
    y = "Total Proportion of Dominant Groups"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

ggsave(here::here('figures','faceted_js_divergence_heatmap.png'), width = 12, height = 8)

#Jeffrey's divergence
ggplot(filter(heatmap_data, dom_prop_total <1), aes(x = factor(num_dom), y = factor(dom_prop_total), fill = Mean_J_Divergence)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Mean Jeffrey's Divergence") +
  facet_wrap(~ num_near_zero, labeller = as_labeller(function(x) paste0("P = ", x))) +
  labs(
    title = "Jeffrey's Divergence Heatmap",
    subtitle = "Faceted by the number of near-zero groups (P)",
    x = "Number of Dominant Groups",
    y = "Total Proportion of Dominant Groups"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(size = 10, face = "bold")
  )

ggsave(here::here('figures','faceted_jeffreys_divergence_heatmap.png'), width = 12, height = 8)

all_results$dom_prop_total = factor(all_results$dom_prop_total)

ggplot(all_results, aes(x = JS_Divergence)) +
  geom_density(aes(color = dom_prop_total), linewidth = 1) +
  facet_grid(num_dom ~ num_near_zero, scales = "free") +
  labs(
    title = "Distribution of JS Divergence",
    subtitle = "Faceted by Number of Dominant and Near-Zero Groups",
    x = "JS Divergence",
    y = "Density"
  ) +
  theme_bw() +
  scale_color_discrete(name = "Dominance Proportion")
ggsave(here::here('figures','JS_divergence_faceted_density.png'),width = 12, height = 12)

ggplot(all_results, aes(x = J_Divergence)) +
  geom_density(aes(color = dom_prop_total), linewidth = 1) +
  facet_grid(num_dom ~ num_near_zero, scales = "free") +
  labs(
    title = "Distribution of Jeffrey's Divergence",
    subtitle = "Faceted by Number of Dominant and Near-Zero Groups",
    x = "Jeffrey's Divergence",
    y = "Density"
  ) +
  theme_bw() +
  scale_color_discrete(name = "Dominance Proportion")
ggsave(here::here('figures','Jeffreys_divergence_faceted_density.png'),width = 12, height = 12)

#Box plot
ggplot(all_results, aes(x = dom_prop_total, y = JS_Divergence)) +
  geom_boxplot(linewidth = 1) +
  facet_grid(num_dom~ num_near_zero, labeller = label_both) +
  labs(
    title = "Distribution of Jeffrey's Divergence",
    subtitle = "Faceted by Number of Dominant and Near-Zero Groups",
    x = "Dominance Proportion",
    y = "Jeffrey's Divergence"
  )  +
  theme_bw()
ggsave(here::here('figures','JS_divergence_faceted_boxplot.png'),width = 14, height = 14)

ggplot(filter(all_results, dom_prop_total != 1), aes(x = dom_prop_total, y = J_Divergence)) +
  geom_boxplot(linewidth = 1) +
  facet_grid(num_dom~ num_near_zero, labeller = label_both) +
  labs(
    title = "Distribution of Jeffrey's Divergence",
    subtitle = "Faceted by Number of Dominant and Near-Zero Groups",
    x = "Dominance Proportion",
    y = "Jeffrey's Divergence"
  )  +
  theme_bw()
  # scale_color_discrete(name = "Dominance Proportion")
ggsave(here::here('figures','Jeffreys_divergence_faceted_boxplot.png'),width = 14, height = 14)
