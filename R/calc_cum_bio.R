#'Cumulative Biomass Distribution
#'Calculates the cumulative biomass statistics from Libralato et al, 2019
#'
#'@param biomass.df dataframe. Biomass dataframe with the biomass of each group (species x year x biomass) returned from est_biomass_time
#'@param tl.df dataframe. Trophic level dataframe with the trophic levels of each group (species x year x TL) returned from est_trophic_level_time
#'@param show.plot logical. whether it should return plots as well as data
#'@param out.dir string. Output directory to save the results
#'
#'@return dataframe of steepness, biomass at inflection, and trophic level at inflection for each year, as well as fitted parameters
#'
#'@export

calc_cum_bio = function(bio.df,tl.df,show.plot=F,out.dir){
 
  #Define baro5 function
  baro5_function <- function(x_val, b1, b2, c_param, d_param, e_param) {
    log_x_minus_log_e <- log(x_val) - log(e_param)
    # Handle potential division by zero if b1 + b2 is very close to zero
    # This is a robust way to compute f_val for the baro5 function
    if (abs(b1 + b2) < 1e-10) { # A small tolerance
      f_val <- 0.5 # Default to 0.5 for symmetric case if b1 approx -b2
    } else {
      f_val <- 1 / (1 + exp((2 * b1 * b2 / (b1 + b2)) * log_x_minus_log_e))
    }
    y <- c_param + (d_param - c_param) / (1 + f_val * exp(b1 * log_x_minus_log_e) + (1 - f_val) * exp(b2 * log_x_minus_log_e))
    return(y)
  }

  
  #Read in data
  
  data = bio.df |> 
    dplyr::left_join(tl.df) |> 
    dplyr::filter(!is.na(trophicLevel))
  
  # saveRDS(data,here::here('data-raw','test_data.rds'))
  # data =readRDS(here::here('data-raw','test_data.rds'))

  #Map tl.bins to data and sum biomass by tl.bin
  
  data.cumb = data |> 
    dplyr::group_by(year,trophicLevel) |> 
    dplyr::summarise(biomass = sum(biomass,na.rm=T)) |> 
    dplyr::group_by(year) |>
    dplyr::arrange(trophicLevel) |>
    dplyr::mutate(
      biomass.total = sum(biomass,na.rm=T),
      biomass.cum = cumsum(biomass),
      biomass.cum.prop = biomass.cum/biomass.total
    )
   
  #Test plot for 1 year
  # yr1 = dplyr::filter(data.binned,year == 30) |> 
  #   dplyr::mutate(cum.biomass = cumsum(biomass) / sum(biomass, na.rm = TRUE))
  # ggplot2::ggplot(yr1,ggplot2::aes(x= trophicLevel, y = cum.biomass)) +
  #   ggplot2::geom_line()
  
  yrs = unique(data.cumb$year)
  i=1
  out.df = data.frame(year = yrs, steep = NA, bio_inf = NA, tl_inf = NA, b1_param =NA, b2_param = NA, c_param =NA,
                      d_param = NA, e_param = NA, stringsAsFactors = FALSE)
  for(i in 1:length(yrs)){
    this.data = dplyr::filter(data.cumb, year == yrs[i])
    # plot(biomass.cum.prop~trophicLevel, this.data,type='l')
    #fit to baro5 model
    baro5_fixed_d = drc::baro5(fixed = c(NA,NA,NA,1,NA))
    cumb.model = drc::drm(biomass.cum.prop~trophicLevel, data = this.data, fct = baro5_fixed_d)
    # summary(cumb.model)
    # plot(cumb.model)
    p_fitted <- coef(cumb.model)
    b1 <- p_fitted[1]
    b2 <- p_fitted[2]
    c_param <- p_fitted[3]
    d_param <- 1#p_fitted[4]
    e_param <- p_fitted[4]
    
    #Set bounds for derivative
    f_prime_wrapper <- function(x_val) {
      baro5_function(x_val, b1, b2, c_param, d_param, e_param)
    }
    
    # Calculate the second derivative over a sufficiently wide range for inspection
    x_range_full <- seq(min(this.data$trophicLevel), max(this.data$trophicLevel), length.out = 10000) 
    second_deriv_values <- sapply(x_range_full, function(x) numDeriv::hessian(f_prime_wrapper, x))
    
    # plot(x_range_full, second_deriv_values,type='l')
    
    # --- 3. Find the Inflection Point using the "Largest Jump" Method ---
    
    # Find all indices where the second derivative changes sign
    sign_changes <- which(diff(sign(second_deriv_values)) != 0)
    
    if (length(sign_changes) == 0) {
      stop("No inflection point (sign change) found.")
    }
    
    # Now, for each sign change, calculate the "jump" magnitude
    jump_magnitudes <- c()
    for (j in seq_along(sign_changes)) {
      idx <- sign_changes[j]
      # Magnitude is the difference between the value just before and just after the crossing
      jump_magnitudes[j] <- abs(second_deriv_values[idx] - second_deriv_values[idx + 1])
    }
    
    # Find the index of the largest jump
    largest_jump_index <- which.max(jump_magnitudes)
    
    # The inflection point is at the x-value of this largest jump
    inflection_x_index <- sign_changes[largest_jump_index]
    inflection_x <- x_range_full[inflection_x_index]
    
    # To get a more precise value, use `uniroot` in a tight interval around this point
    # The interval will be the two points bracketing the largest jump
    interval_for_uniroot <- c(x_range_full[inflection_x_index], x_range_full[inflection_x_index + 1])
    
    # Use uniroot to find the precise zero-crossing in this small interval
    inflection_x <- uniroot(splinefun(x_range_full, second_deriv_values),
                            interval = interval_for_uniroot)$root
    
    inflection_y <- baro5_function(inflection_x, b1, b2, c_param, d_param, e_param)
    slope_at_inflection <- numDeriv::grad(f_prime_wrapper, inflection_x)
    
    #write to out.df
    out.df$steep[i] = slope_at_inflection
    out.df$bio_inf[i] = inflection_y
    out.df$tl_inf[i] = inflection_x
    out.df$b1_param[i] = b1
    out.df$b2_param[i] = b2
    out.df$c_param[i] = c_param
    out.df$d_param[i] = d_param
    out.df$e_param[i] = e_param
    
    if(show.plot == T){
      plot_df <- data.frame(
        x_plot = x_range_for_deriv,
        y_plot = baro5_function(x_range_for_deriv, b1, b2, c_param, d_param, e_param)
      )

      ggplot2::ggplot(this.data, ggplot2::aes(x = trophicLevel, y = biomass.cum.prop)) +
        ggplot2::geom_point() + # Original data points
        ggplot2::geom_line(data = plot_df, ggplot2::aes(x = x_plot, y = y_plot), color = "blue", lwd = 1) + # Fitted curve
        ggplot2::geom_vline(xintercept = inflection_x, lty = "dashed", color = "red") + # Vertical line at inflection_x
        ggplot2::geom_point(ggplot2::aes(x = inflection_x, y = inflection_y), color = "red", size = 3, shape = 16)  # Inflection point
    }

  }
  
  #Save output
  if(!missing(out.dir)){
    saveRDS(out.df,paste0(out.dir, 'cumulative_biomass.rds'))
  }
  
  #Diagnostic plots
  
  # plot(steep~year,out.df,type='l')
  # plot(bio_inf~year,out.df,type='l')
  # plot(tl_inf~year,out.df,type='l')

  # plot(steep~tl_inf,out.df)
  # plot(bio_inf~tl_inf,out.df)
  # plot(steep~tl_inf,out.df)
  
  return(out.df)
  
}
