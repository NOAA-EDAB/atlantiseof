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
# Helper function to normalize a vector to sum to 1
normalize <- function(x) {
  if (sum(x, na.rm = TRUE) == 0) return(x)
  return(x / sum(x, na.rm = TRUE))
}

generate_dominant_scenario <- function(dominant_group_name, dominance_factor, group.mapping, ref_sub_weights, rounding_digits) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but is not installed.")
  }
  
  # --- 1. Calculate a SINGLE, STATIC set of group weights ---
  all_groups <- unique(as.character(group.mapping$Group))
  if (!dominant_group_name %in% all_groups) {
    stop("`dominant_group_name` not found in `group.mapping$Group`.")
  }
  
  num_groups <- length(all_groups)
  p_t <- dominance_factor / (1 + dominance_factor)
  p_nt <- 1 / (1 + dominance_factor)
  group_weights <- numeric(num_groups)
  names(group_weights) <- all_groups
  group_weights[dominant_group_name] <- p_t
  
  non_dominant_groups <- setdiff(all_groups, dominant_group_name)
  if (length(non_dominant_groups) > 0) {
    # Distribute the non-dominant proportion based on reference weights
    group_ref_totals <- ref_sub_weights %>%
      dplyr::left_join(group.mapping, by = "SubGroup", relationship = "many-to-many") %>%
      dplyr::filter(!is.na(Group)) %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(total_ref_weight = sum(Weight, na.rm = TRUE), .groups = "drop")
    
    non_dominant_ref_weights <- group_ref_totals %>%
      dplyr::filter(Group %in% non_dominant_groups)
    
    proportions <- normalize(non_dominant_ref_weights$total_ref_weight)
    proportional_nt_weights <- proportions * p_nt
    
    names(proportional_nt_weights) <- non_dominant_ref_weights$Group
    group_weights[non_dominant_groups] <- proportional_nt_weights[non_dominant_groups]
  }
  
  group_weights_df <- data.frame(
    Group = names(group_weights),
    group_weight = group_weights
  )
  
  # --- 2. Calculate and FIX relative weights per timestep ---
  final_data <- ref_sub_weights %>%
    dplyr::left_join(group.mapping, by = "SubGroup", relationship = "many-to-many") %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::left_join(group_weights_df, by = "Group") %>%
    dplyr::group_by(Time, Group) %>%
    dplyr::mutate(
      relative_weight = {
        group_total <- sum(Weight, na.rm = TRUE)
        if (group_total == 0) 0 else Weight / group_total
      }
    ) %>%
    dplyr::ungroup()
  
  # --- 3. Apply the simple, robust rounding method ---
  result_df <- final_data %>%
    dplyr::group_by(Time, Group) %>%
    dplyr::group_modify(function(df, key) {
      target_sum <- df$group_weight[1]
      unrounded <- target_sum * df$relative_weight
      
      if (sum(unrounded, na.rm = TRUE) == 0) {
        df$subgroup_weight <- 0
        return(df)
      }
      
      rounded <- round(unrounded, digits = rounding_digits)
      discrepancy <- target_sum - sum(rounded)
      
      if (abs(discrepancy) > (10^-(rounding_digits + 2))) {
        idx_to_adjust <- which.max(unrounded)
        if(length(idx_to_adjust) > 0) {
          rounded[idx_to_adjust] <- rounded[idx_to_adjust] + discrepancy
        }
      }
      
      df$subgroup_weight <- rounded
      return(df)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(Time, Group, SubGroup, group_weight, subgroup_weight) %>%
    dplyr::arrange(Time, Group, SubGroup)
  
  # --- 4. Final Tweak: Ensure total proportion sums to exactly 1 ---
  # This handles any minor floating-point dust left over from rounding.
  final_df <- result_df %>%
    dplyr::group_by(Time) %>%
    dplyr::mutate(
      discrepancy = 1 - sum(subgroup_weight),
      # Add discrepancy to the single largest subgroup to fix the sum
      subgroup_weight = if_else(subgroup_weight == max(subgroup_weight), subgroup_weight + discrepancy, subgroup_weight)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-discrepancy) # Clean up helper column
  
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


generate_random_scenario <- function(group.mapping, ref_sub_weights, rounding_digits) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but is not installed.")
  }
  
  # --- 1. Calculate Group Weights Using a WEIGHTED RANDOM Draw ---
  # This method is truly random but respects the proportions in the reference data.
  group_ref_totals <- ref_sub_weights %>%
    dplyr::left_join(group.mapping, by = "SubGroup", relationship = "many-to-many") %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::group_by(Group) %>%
    dplyr::summarise(total_ref_weight = sum(Weight, na.rm = TRUE), .groups = "drop")
  
  # Use the total reference weights as shape parameters (alphas) for a Gamma distribution.
  # Normalizing random draws from a Gamma distribution is equivalent to drawing
  # from a Dirichlet distribution, which is the correct way to get a random
  # vector that sums to 1.
  # A small value is added to the shape to avoid issues if a group's total weight is zero.
  alphas <- group_ref_totals$total_ref_weight + 0.0001
  random_draws <- rgamma(length(alphas), shape = alphas, scale = 1)
  
  # Normalize the random draws to get the final group weights
  random_group_weights <- normalize(random_draws)
  
  group_weights_df <- data.frame(
    Group = group_ref_totals$Group,
    group_weight = random_group_weights
  )
  
  # --- 2. Calculate relative weights per timestep ---
  final_data <- ref_sub_weights %>%
    dplyr::left_join(group.mapping, by = "SubGroup", relationship = "many-to-many") %>%
    dplyr::filter(!is.na(Group)) %>%
    dplyr::left_join(group_weights_df, by = "Group") %>%
    dplyr::group_by(Time, Group) %>%
    dplyr::mutate(
      relative_weight = {
        group_total <- sum(Weight, na.rm = TRUE)
        if (group_total == 0) 0 else Weight / group_total
      }
    ) %>%
    dplyr::ungroup()
  
  # --- 3. Apply the simple, robust rounding method ---
  result_df <- final_data %>%
    dplyr::group_by(Time, Group) %>%
    dplyr::group_modify(function(df, key) {
      if(is.na(df$group_weight[1])) {
        df$subgroup_weight <- 0
        return(df)
      }
      target_sum <- df$group_weight[1]
      unrounded <- target_sum * df$relative_weight
      if (sum(unrounded, na.rm = TRUE) == 0) {
        df$subgroup_weight <- 0
        return(df)
      }
      rounded <- round(unrounded, digits = rounding_digits)
      discrepancy <- target_sum - sum(rounded)
      if (abs(discrepancy) > (10^-(rounding_digits + 2))) {
        idx_to_adjust <- which.max(unrounded)
        if(length(idx_to_adjust) > 0) {
          rounded[idx_to_adjust] <- rounded[idx_to_adjust] + discrepancy
        }
      }
      df$subgroup_weight <- rounded
      return(df)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(Time, Group, SubGroup, group_weight, subgroup_weight) %>%
    dplyr::arrange(Time, Group, SubGroup)
  
  # --- 4. Final Tweak: Ensure total proportion sums to exactly 1 ---
  final_df <- result_df %>%
    dplyr::group_by(Time) %>%
    dplyr::mutate(
      discrepancy = 1 - sum(subgroup_weight),
      subgroup_weight = if_else(subgroup_weight == max(subgroup_weight), subgroup_weight + discrepancy, subgroup_weight)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-discrepancy)
  
  return(final_df)
}
