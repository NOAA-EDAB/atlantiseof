#' Calculate the distance from model output to desired state
#'
#'@param ideal.state vector. Desired state of the model defined as as range from a point (length N)
#'@param ideal.center vector. Center of the ideal state (length N)
#'@param observed.state vector. Observed state of the model defined as a range from a point (length N)
#'
#'@return data.frame
#'
#'@export


make_state_distance = function(ideal.state, ideal.center, observed.state){
  
  #objective: minimize squared distance to P
  objective <- function(x) {
    sum((x - observed.state)^2)
  }
  
  # Equality constraint: point must lie on ellipsoid surface
  constraint_eq <- function(x) {
    sum(((x - ideal.center)/ideal.state)^2) - 1
  }
  
  grad_objective <- function(x) {
    2 * (x - observed.state)
  }
  
  # Gradient (Jacobian) of the equality constraint
  jac_constraint_eq <- function(x) {
    2 * (x - ideal.center) / (ideal.state^2)
  }
  # Initial guess: project P onto ellipsoid
  shifted <- observed.state - ideal.center
  scale_factor <- sqrt(sum((shifted / ideal.state)^2))
  x0 <- ideal.center + (shifted * ideal.state / scale_factor)
  
  # Set up optimization
  opts <- list("algorithm" = "NLOPT_LD_AUGLAG_EQ",
               "xtol_rel" = 1e-8,
               "maxeval" = 1000,
               "local_opts" = list("algorithm" = "NLOPT_LD_MMA", "xtol_rel" = 1e-8))
  
  res <- nloptr::nloptr(x0 = x0,
                eval_f = objective,
                eval_g_eq = constraint_eq,
                eval_grad_f = grad_objective,
                eval_jac_g_eq = jac_constraint_eq,
                opts = opts)
  
  # Results
  closest_point <- res$solution
  distance <- sqrt(sum((closest_point - observed.state)^2))

  return(list(
    closest_point = closest_point,
    distance = distance))
  
}

make_state_distance(  ideal.state = c(3, 2, 1),      # semi-axes
  ideal.center = c(1, 2, 3),    # center of ellipsoid
  observed.state = c(5, 2, 0)       # external point
)
