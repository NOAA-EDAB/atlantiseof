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
generate_dominant_scenario <- function(dominant_group_index, dominance_factor, num_groups, ref_sub_weights, rounding_digits) {
  p_t <- dominance_factor / (1 + dominance_factor)
  p_nt <- 1 / (1 + dominance_factor)
  
  group_weights <- numeric(num_groups)
  group_weights[dominant_group_index] <- p_t
  
  non_dominant_indices <- (1:num_groups)[-dominant_group_index]
  if (num_groups > 1) {
    random_nt_weights <- normalize(runif(num_groups - 1)) * p_nt
    group_weights[non_dominant_indices] <- random_nt_weights
  }
  
  # --- CORRECTED SECTION: Calculate and Round Subgroup Weights per Group ---
  final_subgroup_weights_list <- lapply(1:num_groups, function(i) {
    # 1. Calculate unrounded weights for the current group
    unrounded_group_specific_weights <- group_weights[i] * ref_sub_weights[[i]]
    
    # 2. Round them
    rounded_group_specific_weights <- round(unrounded_group_specific_weights, digits = rounding_digits)
    
    # 3. Calculate discrepancy relative to the original group weight and adjust
    discrepancy <- group_weights[i] - sum(rounded_group_specific_weights)
    if (discrepancy != 0) {
      idx_to_adjust <- which.max(rounded_group_specific_weights)
      rounded_group_specific_weights[idx_to_adjust] <- rounded_group_specific_weights[idx_to_adjust] + discrepancy
    }
    
    return(rounded_group_specific_weights)
  })
  
  final_rounded_weights <- unlist(final_subgroup_weights_list)
  
  # --- CORRECTED SECTION: Build Final Data Frame ---
  subgroups_per_group_vec <- sapply(ref_sub_weights, length)
  group_ids <- rep(1:num_groups, times = subgroups_per_group_vec)
  subgroup_ids <- unlist(lapply(subgroups_per_group_vec, seq_len))
  
  base_df <- data.frame(
    group = group_ids,
    subgroup = subgroup_ids,
    subgroup_weight = final_rounded_weights
  )
  
  # Calculate final group weights from the sum of corrected subgroup weights
  group_weight_df <- aggregate(subgroup_weight ~ group, data = base_df, FUN = sum)
  colnames(group_weight_df)[2] <- "group_weight"
  
  # Merge and reorder columns
  final_df <- merge(base_df, group_weight_df, by = "group")
  final_df <- final_df[, c("group", "subgroup", "group_weight", "subgroup_weight")]
  
  return(final_df)
}

#' Generate a Scenario with Randomly Sampled Group Weights
#'
#' This function creates a weighting scenario where group weights are sampled
#' randomly from a uniform Dirichlet distribution. The only constraint is that
#' the sum of group weights is 1. The function ensures the final subgroup
#' weights are rounded to a specified number of digits and sum exactly to 1.
#'
#' @param num_groups The total number of groups (N).
#' @param ref_sub_weights A list where each element is a numeric vector of
#'   reference weights for a group's subgroups. These define the constant relative
#'   proportions within each group.
#' @param rounding_digits The number of decimal places to round the final weights to.
#' @return A data frame with columns: `group`, `subgroup`, `group_weight`, and `subgroup_weight`.
#' @export
#' @examples
#' ref_w <- list(c(0.2, 0.8), c(0.5, 0.5), c(0.1, 0.2, 0.7))
#' generate_random_scenario(
#'   num_groups = 3,
#'   ref_sub_weights = ref_w,
#'   rounding_digits = 4
#' )
generate_random_scenario <- function(num_groups, ref_sub_weights, rounding_digits) {
  group_weights <- normalize(runif(num_groups))
  
  # --- CORRECTED SECTION: Calculate and Round Subgroup Weights per Group ---
  final_subgroup_weights_list <- lapply(1:num_groups, function(i) {
    # 1. Calculate unrounded weights for the current group
    unrounded_group_specific_weights <- group_weights[i] * ref_sub_weights[[i]]
    
    # 2. Round them
    rounded_group_specific_weights <- round(unrounded_group_specific_weights, digits = rounding_digits)
    
    # 3. Calculate discrepancy relative to the original group weight and adjust
    discrepancy <- group_weights[i] - sum(rounded_group_specific_weights)
    if (discrepancy != 0) {
      idx_to_adjust <- which.max(rounded_group_specific_weights)
      rounded_group_specific_weights[idx_to_adjust] <- rounded_group_specific_weights[idx_to_adjust] + discrepancy
    }
    
    return(rounded_group_specific_weights)
  })
  
  final_rounded_weights <- unlist(final_subgroup_weights_list)
  
  # --- CORRECTED SECTION: Build Final Data Frame ---
  subgroups_per_group_vec <- sapply(ref_sub_weights, length)
  group_ids <- rep(1:num_groups, times = subgroups_per_group_vec)
  subgroup_ids <- unlist(lapply(subgroups_per_group_vec, seq_len))
  
  base_df <- data.frame(
    group = group_ids,
    subgroup = subgroup_ids,
    subgroup_weight = final_rounded_weights
  )
  
  # Calculate final group weights from the sum of corrected subgroup weights
  group_weight_df <- aggregate(subgroup_weight ~ group, data = base_df, FUN = sum)
  colnames(group_weight_df)[2] <- "group_weight"
  
  # Merge and reorder columns
  final_df <- merge(base_df, group_weight_df, by = "group")
  final_df <- final_df[, c("group", "subgroup", "group_weight", "subgroup_weight")]
  
  return(final_df)
}

