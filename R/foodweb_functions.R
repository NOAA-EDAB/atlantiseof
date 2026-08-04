# Functions for food web calculations in calc_foodweb() and elsewhere

#'Corrected Trophic Levels with assumed primary producers
#'Function to calculate trophic level with fixed primary producers (useful for age-structured food web)
#'
#'@param adj_matrix matrix. directed flow adjacency matrix of the food web with names (Rows = Prey, Cols = Predators)
#'@param basal_species vector. character vector of species that are basal (primary producers)
#'
#'@returns vector of trophic levels by species in the adj_matrix
#'
#'@export 
#'

calculate_trophic_levels_corrected <- function(adj_matrix, basal_species) {
  
  # 1. Transpose the matrix so Rows = Predators, Columns = Prey
  # This is required because the input adj_matrix has Rows = Prey, Cols = Predators[cite: 5].
  flow_mat <- t(adj_matrix)
  
  # 2. Calculate total consumption by each predator
  # Since Rows = Predators, rowSums calculates total diet.
  row_totals <- rowSums(flow_mat, na.rm = TRUE) 
  
  # 3. Normalize raw consumption flows into a Diet Composition (Proportion) matrix 'Q'
  # Prevent division by zero for species that don't eat, and sweep across rows (MARGIN = 1)
  Q <- sweep(flow_mat, 1, ifelse(row_totals > 0, row_totals, 1), "/")
  
  # Initialize a vector for trophic levels with NAs
  tl <- rep(NA, nrow(Q))
  names(tl) <- rownames(Q)
  
  # Manually set the trophic level of known basal species to 1
  basal_species_in_matrix <- intersect(basal_species, rownames(Q))
  tl[basal_species_in_matrix] <- 1
  
  # Identify non-basal species
  non_basal_species <- setdiff(rownames(Q), basal_species_in_matrix)
  
  if (length(non_basal_species) > 0) {
    # Create a sub-matrix of proportions between only non-basal species
    sub_Q <- Q[non_basal_species, non_basal_species, drop = FALSE]
    
    # Create a vector 'e' that represents the proportional contribution from basal species
    # Matrix math (Q %*% tl) now correctly multiplies predator diets by prey trophic levels
    e <- Q[non_basal_species, basal_species_in_matrix, drop = FALSE] %*% tl[basal_species_in_matrix]
    
    # Calculate the trophic levels for non-basal species using matrix inversion
    # TL = 1 + Q * TL -> (I - Q) * TL = 1 + e
    I <- diag(length(non_basal_species))
    tl_non_basal <- solve(I - sub_Q, rep(1, length(non_basal_species)) + e)
    
    # Fill in the final trophic levels vector
    tl[non_basal_species] <- tl_non_basal
  }
  
  return(tl)
}

#'Coherence
#'Function to calculate trophic coherence
#'@param adj_matrix matrix. directed flow adjacency matrix of the food web with names (Rows = Prey, Cols = Predators in mgN)
#'@param trophic_levels numeric. named vector of trophic levels by species in the adj_matrix returned from calculate_trophic_levels_corrected()
#'@param diet_threshold numeric. The minimum proportion of a predator's diet a prey must make up to be included in variance calculations (default 0.01)
#'
#'@returns numeric coherence value
#'
#'@export 
#'

calculate_coherence <- function(adj_matrix, trophic_levels, diet_threshold = 0.01) {
  # 1. Filter out NA trophic levels (e.g., disconnected nodes)
  valid_tl <- trophic_levels[!is.na(trophic_levels)]
  if (length(valid_tl) == 0) return(NA)
  
  # 2. Subset the adjacency matrix to only include valid species
  adj_matrix_valid <- adj_matrix[names(valid_tl), names(valid_tl), drop = FALSE]
  
  # 3. Calculate the variance of all valid trophic levels in the network
  var_T <- var(valid_tl, na.rm = TRUE)
  
  # If there is no variance in the network's trophic levels, coherence is undefined
  if (is.na(var_T) || var_T == 0) return(NA)
  
  # 4. Calculate the dietary variance for each individual predator (iterating over columns)
  var_prey_T <- apply(adj_matrix_valid, 2, function(pred_col) {
    
    # Calculate the predator's total diet in absolute units (mgN)
    total_diet <- sum(pred_col, na.rm = TRUE)
    
    # If the predator doesn't eat anything, its dietary variance is 0
    if (total_diet == 0) return(0)
    
    # Convert raw mgN flows into diet proportions
    diet_props <- pred_col / total_diet
    
    # Only keep prey that make up a meaningful proportion of the diet (e.g., > 1%)
    # This filters out the microscopic trace flows that inflate variance
    prey_tl <- valid_tl[diet_props > diet_threshold]
    
    # Calculate variance. A specialist (0 or 1 prey item) has 0 dietary variance.
    if (length(prey_tl) > 1) {
      return(var(prey_tl, na.rm = TRUE))
    } else {
      return(0)
    }
  })
  
  # 5. Sum the individual predator variances
  sum_var_prey_T <- sum(var_prey_T, na.rm = TRUE)
  
  # 6. Calculate total number of valid species
  S <- length(valid_tl)
  
  # 7. Calculate final trophic coherence
  coherence <- 1 - (sum_var_prey_T / (S * var_T))
  
  # Bound the result between 0 and 1
  return(max(0, min(1, coherence)))
}

#'Jaccard Similarity
#'Function to calculate the average unweighted Jaccard similarity
#'@param g igraph object. directed food web graph
#'
#'@returns numeric average jaccard similarity value
#'
#'@export 
#'
calculate_avg_jaccard <- function(g) {
  predators <- igraph::V(g)[igraph::degree(g, mode = "out") > 0] # Predators are nodes with outgoing links to prey
  num_predators <- length(predators)
  
  if (num_predators < 2) {
    return(NA) # Cannot calculate if there are fewer than 2 predators
  }
  
  # Get a binary matrix where rows are predators and columns are prey
  adj_matrix <- igraph::as_adjacency_matrix(g, sparse = FALSE, type = "both")
  
  # Ensure the matrix has correct dimensions before transposing
  if (nrow(adj_matrix) != ncol(adj_matrix)) {
    stop("Adjacency matrix is not square.")
  }
  
  # Transpose to get prey as rows and predators as columns, then subset for predators
  prey_pred_matrix <- adj_matrix[names(igraph::V(g))[igraph::degree(g, mode="in") > 0], names(igraph::V(g))[igraph::degree(g, mode="out") > 0]]
  
  # Calculate intersection sizes (A & B) using matrix multiplication
  intersections <- prey_pred_matrix %*% t(prey_pred_matrix)
  
  # Calculate union sizes (A U B)
  row_sums <- rowSums(prey_pred_matrix)
  unions <- outer(row_sums, row_sums, `+`) - intersections
  
  # Calculate Jaccard similarity for all pairs
  jaccard_matrix <- intersections / unions
  
  # Extract the upper triangle (excluding the diagonal) and compute the mean
  jaccard_scores <- jaccard_matrix[upper.tri(jaccard_matrix)]
  
  return(mean(jaccard_scores, na.rm = TRUE))
}

#'Weighted Jaccard Similarity (Ružička Similarity)
#'Function to calculate the average weighted Jaccard similarity
#'@param g igraph object. directed food web graph
#'
#'@returns numeric average weighted jaccard similarity value
#'
#'@export 
#'
calculate_avg_jaccard_weighted <- function(g) {
  predators <- igraph::V(g)[igraph::degree(g, mode = "out") > 0] # Predators are nodes with outgoing links to prey
  num_predators <- length(predators)
  
  if (num_predators < 2) {
    return(NA) # Cannot calculate if there are fewer than 2 predators
  }
  
  # Extract weighted adjacency matrix using diet proportions (prop.consumption)
  # Rows = Prey, Cols = Predators
  adj_matrix <- igraph::as_adjacency_matrix(g, attr = "prop.consumption", sparse = FALSE)
  
  # Subset for predators
  pred_names <- names(predators)
  prey_pred_matrix <- adj_matrix[, pred_names, drop = FALSE]
  
  jaccard_scores <- numeric()
  
  # Calculate Ružička Similarity (Weighted Jaccard) for all predator pairs
  for (i in 1:(num_predators - 1)) {
    for (j in (i + 1):num_predators) {
      diet_A <- prey_pred_matrix[, i]
      diet_B <- prey_pred_matrix[, j]
      
      # Sum of minimum overlaps / Sum of maximum overlaps
      intersection <- sum(pmin(diet_A, diet_B))
      union <- sum(pmax(diet_A, diet_B))
      
      if (union > 0) {
        jaccard_scores <- c(jaccard_scores, intersection / union)
      }
    }
  }
  
  return(mean(jaccard_scores, na.rm = TRUE))
}

#'Network Resilience
#' Optimized function to calculate resilience from the dominant eigenvalue
#'@param adj_matrix matrix. directed flow adjacency matrix of the food web with names
#'@param conversion_efficiency numeric. The assumed ecological transfer efficiency (default 0.1)
#'
#'@returns numeric resilience value
#'
#'@export 
#'
calculate_resilience <- function(adj_matrix, conversion_efficiency = 0.1) {
  num_species <- nrow(adj_matrix)
  
  if (num_species < 2) {
    return(NA)
  }
  
  # FIX: Avoid "Plankton Swamp" by normalizing flows relative to the nodes themselves, 
  # rather than the massive global maximum.
  
  # 1. Effect of Predator on Prey (Rows = Prey)
  # Proportion of a prey's total consumed biomass that goes to a specific predator
  row_totals <- rowSums(adj_matrix, na.rm = TRUE)
  pred_on_prey <- sweep(adj_matrix, 1, ifelse(row_totals > 0, row_totals, 1), "/")
  
  # 2. Effect of Prey on Predator (Cols = Predators)
  # Proportion of a predator's total diet that comes from a specific prey
  col_totals <- colSums(adj_matrix, na.rm = TRUE)
  prey_on_pred <- sweep(adj_matrix, 2, ifelse(col_totals > 0, col_totals, 1), "/")
  
  # Create a community matrix
  community_matrix <- matrix(0, nrow = num_species, ncol = num_species, dimnames = dimnames(adj_matrix))
  
  # Off-diagonal elements:
  # Negative effect of predator on prey (based on prey's outflow distribution)
  community_matrix[pred_on_prey > 0] <- -pred_on_prey[pred_on_prey > 0]
  
  # Positive effect of prey on predator (based on predator's diet distribution * efficiency)
  community_matrix[t(prey_on_pred) > 0] <- conversion_efficiency * t(prey_on_pred)[t(prey_on_pred) > 0]
  
  # Diagonal elements: assume constant self-regulation
  diag(community_matrix) <- -1
  
  # Compute eigenvalues
  eigenvalues <- eigen(community_matrix)$values
  
  # Get the dominant eigenvalue (largest real part)
  dominant_eigenvalue <- max(Re(eigenvalues))
  
  return(dominant_eigenvalue)
}