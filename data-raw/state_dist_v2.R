distance_to_surface_nloptr <- function(P, center, basis, bounded, a,
                                       direction = NULL,
                                       lb_unbounded = NULL, ub_unbounded = NULL,
                                       opts = list(xtol_rel = 1e-8, maxeval = 1000,
                                                   local_opts = list(algorithm = "NLOPT_LD_MMA", xtol_rel = 1e-8))) {
  # P, center: numeric vectors of length n.
  # basis: an n x n orthonormal matrix.
  # bounded: logical vector of length n. TRUE for coordinates that must satisfy the ellipsoidal constraint.
  # a: numeric vector of semi-axis lengths for the bounded coordinates (order corresponds to which(bounded)).
  # direction (optional): numeric vector of length n. For each coordinate with bounded == FALSE,
  #        +1 indicates that by default q[i] >= 0 and -1 indicates q[i] <= 0.
  # lb_unbounded (optional): numeric vector of length n. For each unbounded coordinate, if non-NA, this value is
  #        used as the lower bound.
  # ub_unbounded (optional): numeric vector of length n. For each unbounded coordinate, if non-NA, this value is
  #        used as the upper bound.
  #
  # The surface is defined in local coordinates q by:
  #   - For bounded coordinates (indices in B):  sum_{i in B} (q[i]/a[i])^2 = 1.
  #   - For unbounded coordinates (indices in U): q[i] is subject to
  #         if lb_unbounded is given then q[i] >= lb_unbounded[i],
  #         if ub_unbounded is given then q[i] <= ub_unbounded[i],
  #         otherwise if direction[i] == 1 then q[i] >= 0, or if direction[i] == -1 then q[i] <= 0.
  
  n <- length(P)
  
  # Transform the global point P into local coordinates:
  # p = basis^T (P - center)
  d <- P - center
  p <- as.vector(t(basis) %*% d)
  
  # Identify the bounded and unbounded indices.
  bounded_inds   <- which(bounded)
  unbounded_inds <- which(!bounded)
  
  # Define the objective as the squared Euclidean distance in local coordinates.
  eval_f <- function(q) {
    sum((q - p)^2)
  }
  
  # And its gradient: ∇f(q) = 2 (q - p)
  eval_grad_f <- function(q) {
    return(2 * (q - p))
  }
  
  
  # Define the equality constraint on the bounded coordinates:
  # We require: sum_{i in bounded} (q[i] / a[i])^2  - 1 = 0.
  eval_g_eq <- function(q) {
    if (length(bounded_inds) > 0) {
      constraint_val <- sum((q[bounded_inds] / a)^2) - 1
    } else {
      constraint_val <- 0
    }
    # Return as a numeric vector.
    return(c(constraint_val))
  }
  
  # Define the gradient of the equality constraint.
  # For i in bounded, the derivative is 2*q[i]/(a[i]^2); for the rest it is 0.
  eval_jac_g_eq <- function(q) {
    grad <- rep(0, n)
    if (length(bounded_inds) > 0) {
      grad[bounded_inds] <- 2 * q[bounded_inds] / (a^2)
    }
    return(grad)
  }
  
  
  
  # Set up default lower and upper bounds for all coordinates.
  lb <- rep(-Inf, n)
  ub <- rep(Inf, n)
  
  # Process the unbounded coordinates:
  for (i in unbounded_inds) {
    # If an explicit lower bound is provided, use it.
    if (!is.null(lb_unbounded) && !is.na(lb_unbounded[i])) {
      lb[i] <- lb_unbounded[i]
    } else if (!is.null(direction) && direction[i] == 1) {
      # Fallback: if direction is +1, set lower bound to 0.
      lb[i] <- 0
    }
    
    # If an explicit upper bound is provided, use it.
    if (!is.null(ub_unbounded) && !is.na(ub_unbounded[i])) {
      ub[i] <- ub_unbounded[i]
    } else if (!is.null(direction) && direction[i] == -1) {
      # Fallback: if direction is -1, set upper bound to 0.
      ub[i] <- 0
    }
  }
  
  # Construct an initial guess q0 in local coordinates.
  q0 <- rep(0, n)
  
  # For bounded coordinates, project p (if possible) onto the ellipsoidal surface.
  if (length(bounded_inds) > 0) {
    p_bounded <- p[bounded_inds]
    norm_ratio <- sum((p_bounded / a)^2)
    if (norm_ratio > 0) {
      scaling <- 1 / sqrt(norm_ratio)
      q0[bounded_inds] <- p_bounded * scaling
    } else {
      # If the bounded portion of p is nearly zero, choose an arbitrary point.
      q0[bounded_inds] <- rep(0, length(bounded_inds))
      q0[bounded_inds[1]] <- a[1]
    }
  }
  
  # For unbounded coordinates, use p if it is feasible; otherwise, use the respective bound.
  for (i in unbounded_inds) {
    # If there is an explicit lower bound, use max(lb[i], p[i]); if an explicit upper bound is given, use min(ub[i], p[i]).
    if (!is.null(lb_unbounded) && !is.na(lb_unbounded[i])) {
      q0[i] <- max(lb[i], p[i])
    } else if (!is.null(direction) && direction[i] == 1) {
      q0[i] <- max(0, p[i])
    } else if (!is.null(ub_unbounded) && !is.na(ub_unbounded[i])) {
      q0[i] <- min(ub[i], p[i])
    } else if (!is.null(direction) && direction[i] == -1) {
      q0[i] <- min(0, p[i])
    } else {
      # If no bound or direction is provided, default to p[i]
      q0[i] <- p[i]
    }
  }
  
 
  
  # Run the optimization.
  nlopt_res <- nloptr::nloptr(
    x0 = q0,
    eval_f = eval_f,
    eval_grad_f = eval_grad_f,
    eval_g_eq = eval_g_eq,
    eval_jac_g_eq = eval_jac_g_eq,
    lb = lb,
    ub = ub,
    opts = c(opts, list(algorithm = "NLOPT_LD_AUGLAG_EQ"))
  )
  
  # The optimal local coordinate vector.
  q_opt <- nlopt_res$solution
  3 <- sqrt(sum((q_opt - p)^2))
  
  # Map back to global coordinates.
  closest_point <- center + as.vector(basis %*% q_opt)
  
  return(list(distance = dist_opt,
              q_opt = q_opt,
              closest_point = closest_point,
              nlopt_result = nlopt_res))
}

### ---- Example Usage ----

# Consider a problem in ℝ³.
#
# Let the surface be defined in local coordinates q = (q₁, q₂, q₃) as follows:
#   - The first two coordinates are bounded by an ellipsoidal condition: (q₁/2)² + (q₂/1)² = 1.
#   - The third coordinate is unbounded but limited by q₃ ≥ 10.
#
# We use the standard basis and center at the origin.

P <- c(3, 0.5, 5, 3)           # A test point (global coordinates).
center <- c(0, 0, 0, 0)
basis <- diag(4)           # Standard basis in ℝ³.

# Define that q₁ and q₂ are bounded (ellipsoidal) and q₃ is unbounded.
bounded <- c(TRUE, TRUE, FALSE, FALSE)
a <- c(2, 1)  # Ellipsoidal semi-axis lengths for the bounded coordinates.

# For the unbounded coordinate (q₃):
#   Instead of the default q₃ >= 0, we require q₃ >= 10.
lb_unbounded <- c(NA, NA, 10, NA)  # Only coordinate 3 has a lower bound.
ub_unbounded <- c(NA, NA, NA, -10)  # No explicit upper bound is given.

direction = c(NA,NA,1, -1)
# (The 'direction' vector is not necessary when explicit bounds are provided; it will be ignored for q₃.)

result <- distance_to_surface_nloptr(P, center, basis, bounded, a,
                                     direction =direction,
                                     lb_unbounded = lb_unbounded,
                                     ub_unbounded = ub_unbounded)

cat("Estimated distance:", result$distance, "\n")
cat("Closest point on the surface:", result$closest_point, "\n")