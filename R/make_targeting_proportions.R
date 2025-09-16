#' Normalize a Numeric Vector to Sum to 1
#'
#' This is an internal helper function that takes a numeric vector and scales its
#' elements so that they sum to 1.
#'
#' @param x A numeric vector.
#' @return A numeric vector of the same length as x where the elements sum to 1.
normalize <- function(x) {
  # Handle the case of a zero-sum vector to avoid division by zero
  if (sum(x) == 0) return(x)
  return(x / sum(x))
}

#' Generate a Scenario with a Single Dominant Group
#'
#' This function creates a weighting scenario where one group (T) has a dominant
#' proportion (P_T) that is a multiple (d) of the sum of all other group
#' proportions (P_NT). The weights of the non-dominant groups are distributed
#' randomly. The function ensures the final subgroup weights are rounded to a
#' specified number of digits and sum exactly to 1.
#'
#' @param dominant_group_index The index (T) of the group to be dominant.
#' @param dominance_factor The scaling factor (d) for dominance, where P_T = d * P_NT.
#' @param num_groups The total number of groups (N).
#' @param ref_sub_weights A list where each element is a numeric vector of
#'   reference weights for a group's subgroups. These define the constant relative
#'   proportions within each group.
#' @param rounding_digits The number of decimal places to round the final weights to.
#' @return A data frame with columns: `group`, `subgroup`, `group_weight`, and `subgroup_weight`.
#' @export
#' @examples
#' ref_w <- list(c(0.2, 0.8), c(0.5, 0.5))
#' generate_dominant_scenario(
#'   dominant_group_index = 1,
#'   dominance_factor = 3,
#'   num_groups = 2,
#'   ref_sub_weights = ref_w,
#'   rounding_digits = 4
#' )
#' Generate a Scenario with a Single Dominant Group (Modified)
#'
#' This function creates a weighting scenario where one group has a dominant
#' proportion that is a multiple (`dominance_factor`) of the sum of all other group
#' proportions. This dominance is applied across multiple timesteps, preserving the
#' relative internal weighting of subgroups within each group at each timestep as
#' defined in the reference weights. The function ensures the final subgroup
#' weights are rounded and sum exactly to their target group weights.
#'
#' @param dominant_group_name A character string specifying the name of the group to be dominant.
#'   This name must exist in the `Group` column of the `group.mapping` data frame.
#' @param dominance_factor The scaling factor (d) for dominance, where the dominant
#'   group's weight ($P_T$) is d times the sum of all other group weights ($P_{NT}$).
#' @param group.mapping A data frame that maps subgroups to groups, with columns
#'   `Group` and `SubGroup`.
#' @param ref_sub_weights A data frame of reference weights for each subgroup at
#'   each timestep, with columns `SubGroup`, `Time`, and `Weight`.
#' @param rounding_digits The number of decimal places to round the final weights to.
#' @return A data frame with columns: `Time`, `Group`, `SubGroup`, `group_weight`,
#'   and `subgroup_weight`.
#' @export
#' @examples
#' # Example Data
#' group_map <- data.frame(
#'   Group = c("A", "A", "B", "B", "C"),
#'   SubGroup = c("A1", "A2", "B1", "B2", "C1")
#' )
#'
#' ref_weights <- data.frame(
#'   SubGroup = rep(c("A1", "A2", "B1", "B2", "C1"), each = 2),
#'   Time = rep(1:2, 5),
#'   # Reference weights that sum to 1.0 for each timestep
#'   Weight = c(0.1, 0.12, 0.4, 0.38, 0.2, 0.2, 0.2, 0.2, 0.1, 0.1) 
#' )
#'
#' generate_dominant_scenario_modified(
#'   dominant_group_name = "A",
#'   dominance_factor = 4,
#'   group.mapping = group_map,
#'   ref_sub_weights = ref_weights,
#'   rounding_digits = 4
#' )
generate_dominant_scenario_modified <- function(dominant_group_name, dominance_factor, group.mapping, ref_sub_weights, rounding_digits) {
  
  # --- 1. Input Validation and Setup ---
  # The dplyr package is required for this implementation.
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but is not installed.")
  }
  
  if (!dominant_group_name %in% group.mapping$Group) {
    stop("`dominant_group_name` not found in `group.mapping$Group`.")
  }
  
  all_groups <- unique(as.character(group.mapping$Group))
  num_groups <- length(all_groups)
  dominant_group_index <- which(all_groups == dominant_group_name)
  
  # --- 2. Generate New Dominant Group Weights ---
  # This logic is independent of time and applies to the groups as a whole.
  p_t <- dominance_factor / (1 + dominance_factor)
  p_nt <- 1 / (1 + dominance_factor)
  
  new_group_weights <- numeric(num_groups)
  new_group_weights[dominant_group_index] <- p_t
  
  non_dominant_indices <- (1:num_groups)[-dominant_group_index]
  if (num_groups > 1) {
    # Distribute the non-dominant proportion randomly among other groups
    random_nt_weights <- normalize(runif(num_groups - 1)) * p_nt
    new_group_weights[non_dominant_indices] <- random_nt_weights
  }
  
  new_group_weights_df <- data.frame(
    Group = all_groups,
    group_weight = new_group_weights,
    stringsAsFactors = FALSE
  )
  
  # --- 3. Calculate Relative Subgroup Proportions from Reference Data ---
  # This preserves the internal structure of each group at each timestep.
  ref_data <- dplyr::left_join(ref_sub_weights, group.mapping, by = "SubGroup") |>
    dplyr::group_by(Time, Group) |>
    dplyr::mutate(relative_weight = Weight / sum(Weight)) |>
    dplyr::ungroup()
  
  # --- 4. Calculate Final Unrounded Subgroup Weights ---
  # Apply the new group weights to the relative subgroup weights.
  final_data <- dplyr::left_join(ref_data, new_group_weights_df, by = "Group") |>
    dplyr::mutate(unrounded_sub_weight = group_weight * relative_weight)
  
  # --- 5. Apply Rounding and Correction ---
  # This must be done for each group at each timestep to ensure the sum is correct.
  adjust_rounding <- function(df, digits) {
    target_sum <- df$group_weight[1] # Target sum is the group's new weight
    
    rounded_weights <- round(df$unrounded_sub_weight, digits = digits)
    discrepancy <- target_sum - sum(rounded_weights)
    
    # If there's a discrepancy, add it to the value with the largest original weight
    # to minimize relative error.
    if (discrepancy != 0) {
      idx_to_adjust <- which.max(df$unrounded_sub_weight) 
      rounded_weights[idx_to_adjust] <- rounded_weights[idx_to_adjust] + discrepancy
    }
    
    df$subgroup_weight <- rounded_weights
    return(df)
  }
  
  # Split the data by Time and Group, apply the rounding function, and recombine.
  corrected_data <- final_data |>
    dplyr::group_by(Time, Group) |>
    dplyr::group_split() |>
    lapply(adjust_rounding, digits = rounding_digits) |>
    dplyr::bind_rows()
  
  # --- 6. Finalize Output ---
  result_df <- corrected_data |>
    dplyr::select(Time, Group, SubGroup, group_weight, subgroup_weight) |>
    dplyr::arrange(Time, Group, SubGroup)
  
  return(result_df)
}

#' Generate a Scenario with Randomly Sampled Group Weights (Modified)
#'
#' This function creates a weighting scenario where group weights are sampled
#' randomly from a uniform distribution. This random allocation is then applied
#' across multiple timesteps, preserving the relative internal weighting of
#' subgroups within each group at each timestep. The function ensures the final
#' subgroup weights are rounded and sum exactly to their target group weights.
#'
#' @param group.mapping A data frame that maps subgroups to groups, with columns
#'   `Group` and `SubGroup`.
#' @param ref_sub_weights A data frame of reference weights for each subgroup at
#'   each timestep, with columns `SubGroup`, `Time`, and `Weight`.
#' @param rounding_digits The number of decimal places to round the final weights to.
#' @return A data frame with columns: `Time`, `Group`, `SubGroup`, `group_weight`,
#'   and `subgroup_weight`.
#' @export
#' @examples
#' # Example Data
#' group_map <- data.frame(
#'   Group = c("A", "A", "B", "B", "C"),
#'   SubGroup = c("A1", "A2", "B1", "B2", "C1")
#' )
#'
#' ref_weights <- data.frame(
#'   SubGroup = rep(c("A1", "A2", "B1", "B2", "C1"), each = 2),
#'   Time = rep(1:2, 5),
#'   Weight = c(0.1, 0.12, 0.4, 0.38, 0.2, 0.2, 0.2, 0.2, 0.1, 0.1)
#' )
#'
#' generate_random_scenario_modified(
#'   group.mapping = group_map,
#'   ref_sub_weights = ref_weights,
#'   rounding_digits = 4
#' )

generate_random_scenario_modified <- function(group.mapping, ref_sub_weights, rounding_digits) {
  
  # --- 1. Input Validation and Setup ---
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but is not installed.")
  }
  
  all_groups <- unique(as.character(group.mapping$Group))
  num_groups <- length(all_groups)
  
  # --- 2. Generate New Random Group Weights ---
  # Group weights are sampled once and then applied across all timesteps.
  random_group_weights <- normalize(runif(num_groups))
  
  new_group_weights_df <- data.frame(
    Group = all_groups,
    group_weight = random_group_weights,
    stringsAsFactors = FALSE
  )
  
  # --- 3. Calculate Relative Subgroup Proportions from Reference Data ---
  # This preserves the internal structure of each group at each timestep.
  ref_data <- dplyr::left_join(ref_sub_weights, group.mapping, by = "SubGroup") |>
    dplyr::group_by(Time, Group) |>
    dplyr::mutate(relative_weight = Weight / sum(Weight)) |>
    dplyr::ungroup()
  
  # --- 4. Calculate Final Unrounded Subgroup Weights ---
  # Apply the new random group weights to the relative subgroup weights.
  final_data <- dplyr::left_join(ref_data, new_group_weights_df, by = "Group") |>
    dplyr::mutate(unrounded_sub_weight = group_weight * relative_weight)
  
  # --- 5. Apply Rounding and Correction ---
  # This must be done for each group at each timestep to ensure the sum is correct.
  adjust_rounding <- function(df, digits) {
    target_sum <- df$group_weight[1]
    
    rounded_weights <- round(df$unrounded_sub_weight, digits = digits)
    discrepancy <- target_sum - sum(rounded_weights)
    
    if (discrepancy != 0) {
      idx_to_adjust <- which.max(df$unrounded_sub_weight) 
      rounded_weights[idx_to_adjust] <- rounded_weights[idx_to_adjust] + discrepancy
    }
    
    df$subgroup_weight <- rounded_weights
    return(df)
  }
  
  # Split the data by Time and Group, apply the rounding function, and recombine.
  corrected_data <- final_data |>
    dplyr::group_by(Time, Group) |>
    dplyr::group_split() |>
    lapply(adjust_rounding, digits = rounding_digits) |>
    dplyr::bind_rows()
  
  # --- 6. Finalize Output ---
  result_df <- corrected_data |>
    dplyr::select(Time, Group, SubGroup, group_weight, subgroup_weight) |>
    dplyr::arrange(Time, Group, SubGroup)
  
  return(result_df)
}