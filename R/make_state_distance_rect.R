#' Computes the distance between a current state and a desired state space
#' 
#' @param observed.state numeric vector based on the current state of the system (where N = the number of indicators)
#' @param desired.lower numeric vector of lower bounds for each indicator (length N)
#' @param desired.upper numeric vector of upper bounds for each indicator (length N)
#' 
#' @details If a dimension is unbounded, you can use -Inf for lower bounds and Inf for upper bounds.
#' 
#' @return A list containing: distance and closest point
#' 
#' @export

make_state_distance_rect <- function(observed.state, desired.lower, desired.upper) {
  
  # Ensure that lower/upper are numeric vectors of the same length as P.
  if(length(observed.state) != length(desired.lower) || length(observed.state) != length(desired.upper)){
    stop("observed.state, desired.lower, and desired.upper must have the same length.")
  }  
  
  # Convert NA in bounds to unbounded values.
  # NA in lower will be interpreted as -Inf
  # NA in upper will be interpreted as +Inf
  desired.lower[is.na(desired.lower)] <- -Inf
  desired.upper[is.na(desired.upper)] <- Inf
  
  # The closest point is the element-wise clamp of P to the available interval.
  # For each coordinate, take the maximum of (P, lower) and then the minimum with (upper).
  closest_point <- pmin(pmax(observed.state, desired.lower), desired.upper)
  
  # Compute the Euclidean distance between P and the clamped point.
  distance <- sqrt(sum((observed.state - closest_point)^2))
  
  return(list(distance = distance, closest_point = closest_point))
}

# make_state_distance_rect(observed.state = c(-0,0),
#                       desired.lower =  c(-10, -10),
#                       desired.upper = c(10, 10))

