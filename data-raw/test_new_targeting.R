# ==============================================================================
# SCRIPT TO RUN SCENARIO ANALYSIS AND VISUALIZATION
# ==============================================================================

# --- Load Functions and Libraries ---
# This script assumes 'scenario_generation_functions.R' is in the same directory.
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# ------------------------------------------------------------------------------
# SETUP: DEFINE PARAMETERS AND REFERENCE WEIGHTS
# ------------------------------------------------------------------------------

# Define the number of groups.
N_groups <- 5

# Define the number of subgroups for each group.
subgroups_per_group <- c(3, 4, 2, 5, 3)

# Define the number of significant digits for rounding the final weights.
rounding_digits <- 4

# Generate reference weights for all subgroups.
reference_subgroup_weights <- lapply(subgroups_per_group, function(num_subgroups) {
  atlantiseof:::normalize(runif(num_subgroups))
})


# ------------------------------------------------------------------------------
# GENERATE SCENARIOS
# ------------------------------------------------------------------------------

# --- Case 1: Single Group Dominance ---
dominant_groups_to_test <- 1:N_groups
dominance_factors_to_test <- exp(seq(log(1E-2),log(20),length.out = 10))

params_case1 <- expand.grid(t = dominant_groups_to_test, d = dominance_factors_to_test)

params_case1$scenario_id <- 1:nrow(params_case1)

# The mapply call itself doesn't need to change. It processes rows in order.
case1_scenarios <- mapply(
  atlantiseof::generate_dominant_scenario,
  dominant_group_index = params_case1$t,
  dominance_factor = params_case1$d,
  MoreArgs = list(
    num_groups = N_groups,
    ref_sub_weights = reference_subgroup_weights,
    rounding_digits = rounding_digits
  ),
  SIMPLIFY = FALSE
)
names(case1_scenarios) <- params_case1$scenario_id


# --- Case 2: Random Sampling ---
num_random_scenarios <- 20

case2_scenarios <- lapply(1:num_random_scenarios, function(i) {
  atlantiseof::generate_random_scenario(
    num_groups = N_groups,
    ref_sub_weights = reference_subgroup_weights,
    rounding_digits = rounding_digits
  ) |> 
    dplyr::mutate(scenario_id = i)
})
names(case2_scenarios) <- paste0("RandomScenario_", 1:num_random_scenarios)



# DIAGNOSTIC CHECKS & VISUALIZATIONS
# ==============================================================================

# --- DIAGNOSTIC CHECKS ---
sample_scenario_name <- "1"
sample_scenario_data <- case1_scenarios[[sample_scenario_name]]
cat("\n--- Running Diagnostic Checks on Scenario:", sample_scenario_name, "---\n")

# Check 1: Sum of all subgroup weights is 1
all_sums_ok <- sapply(1:length(c(case1_scenarios, case2_scenarios)), function(df) {
  sum(case1_scenarios[[df]]$subgroup_weight)
  # isTRUE(all.equal(sum(case1_scenarios$subgroup_weight), 1))
})
cat("Check 1: Sum of all subgroup weights is 1 for all scenarios...", if(all_sums_ok) "PASSED" else "FAILED", "\n")

# Check 2: Dominant proportion
dominant_group <- params_case1[which(names(case1_scenarios) == sample_scenario_name), "t"]
dominance_factor <- params_case1[which(names(case1_scenarios) == sample_scenario_name), "d"]
p_t_actual <- sum(sample_scenario_data$subgroup_weight[sample_scenario_data$group == dominant_group])
p_nt_actual <- sum(sample_scenario_data$subgroup_weight[sample_scenario_data$group != dominant_group])
dominance_check_ok <- isTRUE(abs(p_t_actual- dominance_factor * p_nt_actual)<(10^-(rounding_digits-1)))
cat("Check 2: Dominant proportion (P_T = d * P_NT) is correct...", if(dominance_check_ok) "PASSED" else "FAILED", "\n")

# Check 3: Relative subgroup weights
group_to_check <- 2
relative_weights_scenario <- atlantiseof:::normalize(sample_scenario_data[sample_scenario_data$group == group_to_check, "subgroup_weight"])
relative_weights_reference <- reference_subgroup_weights[[group_to_check]]
subgroup_ratio_ok <- all(abs(relative_weights_scenario - relative_weights_reference) < 0.01)
cat("Check 3: Relative subgroup weights are maintained...", if(subgroup_ratio_ok) "PASSED" else "FAILED", "\n")


# --- VISUALIZATIONS ---

# 1. Visualize Group Proportions for a Single "Dominant" Scenario (Case 1)
group_weights_case1 <- aggregate(subgroup_weight ~ group, data = sample_scenario_data, sum)

plot1 <- ggplot(group_weights_case1, aes(x = factor(group), y = subgroup_weight, fill = factor(group))) +
  geom_col(show.legend = FALSE) +
  labs(
    title = paste("Case 1: Group Weights for Scenario", sample_scenario_name),
    subtitle = paste("Dominant Group:", dominant_group, "with d =", dominance_factor),
    x = "Group",
    y = "Total Group Weight (Proportion)"
  ) +
  theme_minimal()
print(plot1)


# 2. Visualize Group Proportions Across Multiple "Random" Scenarios (Case 2)
num_scenarios_to_plot <- 5
combined_case2_data <- do.call(rbind, lapply(1:num_scenarios_to_plot, function(i) {
  df <- case2_scenarios[[i]]
  df$scenario <- paste("Random", i)
  return(df)
}))

group_weights_case2 <- aggregate(subgroup_weight ~ group + scenario, data = combined_case2_data, sum)

plot2 <- ggplot(group_weights_case2, aes(x = scenario, y = subgroup_weight, fill = factor(group))) +
  geom_col(position = "stack") +
  labs(
    title = "Case 2: Group Weights Across Multiple Random Scenarios",
    subtitle = "Each bar represents a complete, randomly generated scenario",
    x = "Scenario",
    y = "Total Weight (Sum = 1)",
    fill = "Group"
  ) +
  theme_minimal()
print(plot2)

all_scenarios_df <- dplyr::bind_rows(case1_scenarios, .id = "scenario_id")
params_case1$scenario_id <- as.character(params_case1$scenario_id)
full_analysis_df <- dplyr::left_join(all_scenarios_df, params_case1, by = "scenario_id")

plot.df = full_analysis_df |> 
  dplyr::filter(group == t)

ggplot(plot.df, aes(x = d, group_weight))+
  geom_line()+
  ylim(0,1)
