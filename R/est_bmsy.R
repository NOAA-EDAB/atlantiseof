#' Estimates Bmsy from fitting a shaeffer model using biomass and catch time series
#' 
#' @param biomass numeric vector of biomass values
#' @param catch numeric vector of catch values
#' @param start_r numeric starting value for r in the non-linear least squares fit (default is 0.5)
#' @param start_k numeric starting value for carrying capacity K. If NULL, it will be set to max(biomass) * 1.5.
#' 
#' @details If a dimension is unbounded, you can use -Inf for lower bounds and Inf for upper bounds.
#' 
#' @return A list containing: distance and closest point
#' 
#' @export

est_bmsy = function(biomass,catch, start_r = 0.5, start_k = NULL){
  
    # Check inputs
    if(length(biomass) != length(catch)) {
      stop("Biomass and catch must have the same length!")
    }
    n <- length(biomass)
    if(n < 2) {
      print("Time series must have at least two observations.")
      return(list(r = NA,
                  k = NA,
                  MSY = NA,
                  B_msy = NA,
                  model = NA,
                  fit_data = NA))
    }
    
    # Calculate production: Production_t = (B[t+1] - B[t]) + catch[t]
    # We only use time steps 1..(n-1)
    production <- (diff(biomass)) + catch[-n]
    
    # Use biomass values for time steps 1 to (n-1)
    data_fit <- data.frame(biomass = biomass[-n], production = production)
    
    # Determine starting value for K if not provided
    if (is.null(start_k)) {
      start_k <- max(biomass) * 1.5
    }
    
    # Fit the surplus production model using non-linear least squares:
    # production ~ r * biomass * (1 - biomass / K)
    fit <- tryCatch({
      nls(production ~ r * biomass * (1 - biomass / k), 
          data = data_fit, 
          start = list(r = start_r, k = start_k))
    }, error = function(e) {
      # Model did not converge; return NA.
      return(NA)
    })
    
    # If the model fit failed, return NA estimates.
    if (is.na(fit)[1]) {
      return(list(r = NA,
                  k = NA,
                  MSY = NA,
                  B_msy = NA,
                  model = NA,
                  fit_data = data_fit))
    }
    
    params <- coef(fit)
    r_est <- params["r"]
    k_est <- params["k"]
    
    # Calculate MSY and biomass at MSY
    msy_est <- r_est *k_est / 4
    B_msy   <- k_est / 2
    
    # Return results in a list
    return(list(r = r_est,
                k = k_est,
                MSY = msy_est,
                B_msy = B_msy,
                model = fit,
                fit_data = data_fit))
    
  }