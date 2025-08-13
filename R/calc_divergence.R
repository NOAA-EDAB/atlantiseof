#' @description Calculates the compositional divergence metrics from atlantis output
#'
#'@param atl.dir string. Atlantis output directory with the biomass of each group (species x year x biomass) returned from est_biomass_time
#'@param fgs.file string. Atlantis functional groups file
#'@param ref.prop dataframe. Reference proportion dataframe with the reference proportions of each group (species x year x prop) returned from est_ref_prop_time
#'@param show.plot logical. whether it should return plots as well as data
#'
#'@return dataframe of divergence metrics over time 
#'
#'@export

# ref.prop = rep(1/89,89)
# ref.prop = bio.prop.mat[1,]

calc_divergence = function(biomass.df, ref.prop, show.plot){
  
  #utility functions
  calculate_KL_divergence = function(reference, comparison){
    
    reference[reference <= 0] <- 1E-12 # Avoid division by zero
    comparison[comparison <= 0] <- 1E-12 # Avoid division by zero
    
    kl_divergence = sum(reference * log2(reference / comparison))
     
    return(kl_divergence)
  }
  
  calculate_JS_divergence = function(reference, comparison){
    
    # Calculate the average distribution (M)
    M = (reference + comparison) / 2
    
    # Calculate the two KL divergence terms
    kl_ref_to_M = calculate_KL_divergence(reference, M)
    kl_comparison_to_M = calculate_KL_divergence(comparison, M)
    
    # JS divergence is the average of the two KL divergences
    js_divergence = (kl_ref_to_M + kl_comparison_to_M) / 2
    
    return(js_divergence)
  }
  
  calculate_Jeffreys_divergence = function(reference, comparison){
    
    # Calculate the two KL divergence terms
    kl_ref_to_comparison = calculate_KL_divergence(reference, comparison)
    kl_comparison_to_ref = calculate_KL_divergence(comparison, reference)
    
    # Jeffreys divergence is the sum of the two KL divergences
    jeffreys_divergence = kl_ref_to_comparison + kl_comparison_to_ref
    
    return(jeffreys_divergence)
  }
  
  #Read fgs file
  fgs = read.csv(fgs.file) |> 
    dplyr::filter(IsTurnedOn == 1)
  
  # Calculate the proportions for each year
  bio.orig = read.table(paste0(atl.dir,'neus_outputBiomIndx.txt'), header = TRUE)
  
  bio.time = bio.orig$Time  
  
  bio.mat = bio.orig |> 
    dplyr::select(dplyr::all_of(fgs$Code), -Time) |> 
    as.matrix()

  bio.tot = rowSums(bio.mat, na.rm = TRUE)
    
  bio.prop.mat = bio.mat / bio.tot
  bio.prop.mat[is.na(bio.prop.mat)] <- 1E-12
  bio.prop.mat[bio.prop.mat == 0] <- 1E-12 # Avoid division by zero
  
  # Calculate the divergence metrics
  kl_divergence = apply(bio.prop.mat, 1, function(x) calculate_KL_divergence(ref.prop, x))
  js_divergence = apply(bio.prop.mat, 1, function(x) calculate_JS_divergence(ref.prop, x))
  jeffreys_divergence = apply(bio.prop.mat, 1, function(x) calculate_Jeffreys_divergence(ref.prop, x))
   
  out.df = data.frame(
    Time = bio.time,
    KL_divergence = kl_divergence,
    JS_divergence = js_divergence,
    Jeffreys_divergence = jeffreys_divergence
  )
  
  if(show.plot){
    
    plot.df = out.df |> 
      tidyr::gather(key = "Divergence_Type", value = "Value", -Time)
    
   ggplot2::ggplot(plot.df, ggplot2::aes(x = Time, y = Value)) +
     ggplot2::geom_line() +
      ggplot2::facet_wrap(~ Divergence_Type, nrow = 3,scales = "free_y") +
      
      ggplot2::labs(title = "Divergence Metrics Over Time",
                    x = "Year",
                    y = "Divergence Value") +
      ggplot2::theme_minimal()
    

  }
    
  return(out.df)
}
