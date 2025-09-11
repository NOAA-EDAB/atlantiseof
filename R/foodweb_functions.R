# Functions for food web calculations in calc_foodweb() and elsewhere

#'Corrected Trophic Levels with assumed primary producers
#'Function to calculate trophic level with fixed primary producers (useful for age-structured food web)
#'
#'@param adj_matrix matrix. directed flow adjacency matrix of the food web with names
#'@param basal_species vector. character vector of species that are basal (primary producers)
#'
#'@returns vector of trophic levels by species in the adj_matrix
#'
#'@export 
#'

calculate_trophic_levels_corrected <- function(adj_matrix, basal_species) {
  # Initialize a vector for trophic levels with NAs
  tl <- rep(NA, nrow(adj_matrix))
  names(tl) <- rownames(adj_matrix)
  
  # Manually set the trophic level of known basal species to 1
  basal_species_in_matrix <- intersect(basal_species, rownames(adj_matrix))
  tl[basal_species_in_matrix] <- 1
  
  # Identify non-basal species
  non_basal_species <- setdiff(rownames(adj_matrix), basal_species_in_matrix)
  
  if (length(non_basal_species) > 0) {
    # Create a sub-matrix of only non-basal species
    sub_adj_matrix <- adj_matrix[non_basal_species, non_basal_species, drop = FALSE]
    
    # Create a vector 'e' that represents the contribution from basal species
    e <- adj_matrix[non_basal_species, basal_species_in_matrix, drop = FALSE] %*% tl[basal_species_in_matrix]
    
    # Calculate the trophic levels for non-basal species using matrix inversion
    I <- diag(length(non_basal_species))
    tl_non_basal <- solve(I - sub_adj_matrix, rep(1, length(non_basal_species)) + e)
    
    # Fill in the final trophic levels vector
    tl[non_basal_species] <- tl_non_basal
  }
  
  return(tl)
}

#'Coherence
#'Function to calculate trophic coherence
#'@param adj_matrix matrix. directed flow adjacency matrix of the food web with names
#'@param trophic_levels numeric. named vector of trophic levels by species in the adj_matrix returned from calculate_trophic_levels_corrected()
#'
#'@returns numeric coherence value
#'
#'@export 
#'

calculate_coherence <- function(adj_matrix, trophic_levels) {
  valid_tl <- trophic_levels[!is.na(trophic_levels)]
  if (length(valid_tl) == 0) return(NA)
  adj_matrix_valid <- adj_matrix[names(valid_tl), names(valid_tl), drop = FALSE]
  var_T <- var(valid_tl, na.rm = TRUE)
  if (is.na(var_T) || var_T == 0) return(NA)
  var_prey_T <- apply(adj_matrix_valid, 2, function(pred_col) {
    prey_tl <- valid_tl[pred_col > 0]
    if (length(prey_tl) > 0) {
      return(var(prey_tl, na.rm = TRUE))
    } else {
      return(0)
    }
  })
  sum_var_prey_T <- sum(var_prey_T, na.rm = TRUE)
  S <- length(valid_tl)
  coherence <- 1 - (sum_var_prey_T / (S * var_T))
  return(max(0, min(1, coherence)))
}

#'Jaccard Similarity
#'Function to calculate the average Jaccard similarity
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

#'Network Resilience
#' Optimized function to calculate resilience from the dominant eigenvalue
#'@param adj_matrix matrix. directed flow adjacency matrix of the food web with names
#'
#'@returns numeric resilience value
#'
#'@export 
#'
calculate_resilience <- function(adj_matrix) {
  num_species <- nrow(adj_matrix)
  
  if (num_species < 2) {
    return(NA)
  }
  
  # Get interaction strengths from the flow matrix (predator on prey effect)
  # Normalize by the max flow to keep values within a reasonable range
  interaction_strength <- adj_matrix / (max(adj_matrix, na.rm = TRUE) + 1e-9)
  
  # Create a community matrix
  community_matrix <- matrix(0, nrow = num_species, ncol = num_species, dimnames = dimnames(adj_matrix))
  
  # Off-diagonal elements:
  # effect of i on j = strength from j to i (as i is prey for j)
  # effect of j on i = -strength from i to j (as j is predator on i)
  
  # Populate the matrix in a vectorized way
  community_matrix[interaction_strength > 0] <- -interaction_strength[interaction_strength > 0]
  community_matrix[t(interaction_strength) > 0] <- t(interaction_strength)[t(interaction_strength) > 0]
  
  # Diagonal elements: assume constant self-regulation
  diag(community_matrix) <- -1
  
  # Compute eigenvalues
  eigenvalues <- eigen(community_matrix)$values
  
  # Get the dominant eigenvalue (largest real part)
  dominant_eigenvalue <- max(Re(eigenvalues))
  
  return(dominant_eigenvalue)
}