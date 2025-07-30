#Test correlations of indicators from reference run

library(ggplot2)
library(corrplot)

ref.state.file = here::here('data-raw','ref_eco_state_year.rds')
ref.state = readRDS(ref.state.file) |> 
  dplyr::select(-year)

#Plot and calculate correlation between indicators in ref.state
cor.mat = cor(ref.state)
print(round(cor.mat,2))
pairs(ref.state)

core_variables = c('bio.tot','catch.tot')
cor_matrix_mod <- abs(cor(ref.state))
core.indices = which(colnames(cor_matrix_mod) %in% core.vars)

# Visualize the correlation matrix (Correlogram)

corrplot::corrplot(cor.mat, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         main = "Correlation Matrix of Variables")

# Calculate the correlation matrix (using absolute values for cutoff)

for (idx in core.indices) {
  # Set the entire row and column corresponding to the core variable to a very low value
  # EXCEPT for the diagonal (self-correlation)
  cor_matrix_mod[idx, -idx] <- 1e-6 # Set row correlations to low
  cor_matrix_mod[-idx, idx] <- 1e-6 # Set column correlations to low
}

# Ensure the diagonal remains 1 (self-correlation)
diag(cor_matrix_mod) <- 1

print("\nModified Correlation Matrix (absolute values, core variables artificially lowered):")
print(round(cor_matrix_mod, 2))

corrplot::corrplot(cor_matrix_mod, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         main = "Modified Correlation Matrix (Core variables protected)")

# --- 4. Apply findCorrelation to the Modified Matrix ---
cutoff_value <- 0.7 # Example cutoff

highly_correlated_vars_to_remove_mod <- caret::findCorrelation(cor_matrix_mod, cutoff = cutoff_value, names = TRUE, exact = FALSE)

# --- 5. Verify and Create the Reduced Dataset ---
print("\nVariables identified for removal by caret::findCorrelation (with core variables protected):")
print(highly_correlated_vars_to_remove_mod)

# Ensure no core variables were mistakenly included in the removal list
if (any(highly_correlated_vars_to_remove_mod %in% core_variables)) {
  stop("Error: A core variable was identified for removal! Recheck logic.")
}

# Create the final reduced dataset
final_columns_to_keep <- setdiff(colnames(ref.state), highly_correlated_vars_to_remove_mod)
reduced_data_constrained <- ref.state[, final_columns_to_keep]

print("\nFinal Columns Kept:")
print(final_columns_to_keep)
print("\nDimensions of the final reduced dataset:")
print(dim(reduced_data_constrained))
print(head(reduced_data_constrained))

# --- Explanation of the Strategy ---
# 
# The `findCorrelation` function removes variables based on two criteria:
#   1.  **Pairwise correlation:** It finds the highest absolute correlation among *all* variable pairs.
# 2.  **Mean absolute correlation:** From that highly correlated pair, it removes the variable that has a higher *average* absolute correlation with all other variables in the matrix.
# 
# By setting the correlations of your `core_variables` with all *other* variables to a very small number (e.g., `1e-6`), you effectively make their "mean absolute correlation" extremely low. This ensures that even if a core variable is highly correlated with one specific non-core variable, its overall average correlation with *all* other variables will be so low that `findCorrelation` will always choose to remove the *non-core* variable from that pair.
# 
# This approach effectively "protects" your core variables from being flagged for removal by `findCorrelation`.