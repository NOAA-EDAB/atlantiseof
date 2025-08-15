#' @description calculates food web indices from detailed diet output
#'
#'@param atl.dir character string. Path to Atlantis output directory
#'@param param.dir character string. Path to Atlantis parameter directory
#'@param dietSource Character String. Whether to use realized diets (diet), detailedDiet (detdiet) or parameter files (param)
#'@param fgs.file Character String. Name of groups.csv file
#'@param plot logical. whether it should return plots as well as data
#'@param out.dir character string. Path to output directory for plots
#'@param figure.dir character string. Path to directory for figures
#'
#'@return list of metric timeseries, metric status, food web, and ecosystem status
#'
#'@export
#'

calc_foodweb = function(atl.dir, dietSource, show.plot = F, figure.dir = NA, out.dir = NA){
  
  # tictoc::tic()
  
  #Read functional groups file
  fgs = read.csv(fgs.file) |> 
    dplyr::select(Code, Name, LongName, GroupType)
  
  #Define primary producers from fgs
  basal_species_list <- fgs$Name[which(fgs$GroupType %in% c('LG_PHY','SM_PHY','MICROPHYTOBENTHOS','SEAGRASS','PHYTOBEN'))]
  
  
  #Read in diet data
  if(dietSource == 'detdiet'){
    dietFile = 'neus_outputDetDiet_processed.gz'
    
  }else if(dietSource == 'diet'){
    dietFile = paste0(atl.dir,'neus_outputDietCheck.txt')
    
  }else if(dietSource == 'prm'){
    dietFile = paste0(param.dir, 'at_biology.prm')
    
  }else{
    stop('dietSource must be one of detdiet, diet, or param')
  }
  
  diet.prop = atlantiseof::get_diet_prop(param.dir,atl.dir,fgs = fgs.file, dietFile = dietFile,dietSource  = dietSource)
  
  #If agecl in column names append
  if(any(grepl('agecl', colnames(diet.prop)))){
    
    #make age-structured network using agemat
    age.mat = atlantiseof::get_age_mat(bio.file = paste0(param.dir, 'at_biology.prm')) |> 
      dplyr::left_join(fgs)
    
    food_web_df = diet.prop |> 
      dplyr::left_join(age.mat, by = c('pred' = 'Name')) |> 
      dplyr::mutate(age.mat = ifelse(is.na(age.mat),'adult',age.mat),
                    pred.stage = ifelse(agecl>= age.mat, 'adult','juvenile'))
    
  }else{
    
    food_web_df = diet.prop |> 
      dplyr::left_join(fgs, by = c('pred' = 'Name')) |> 
      dplyr::mutate(pred.stage = 'adult')
    
    print('no predator age classes provided. all predators considered adults')
    
  }
  
  # Create a unique predator node name by combining predator and age
  # This accounts for the different life stages of the predators
  food_web_df <- food_web_df |> 
    dplyr::mutate(predator_node = paste(pred, pred.stage, sep = "_"),
                  prey_node = paste0(prey,'_adult')) |> 
    dplyr::group_by(time,predator_node,prey_node) |> 
    dplyr::summarise(consumption = sum(consumption,na.rm=T)) |> 
    dplyr::group_by(time, predator_node) |> 
    dplyr::mutate(consumption.total = sum(consumption,na.rm=T)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(prop.consumption = consumption / consumption.total) |> 
    dplyr::select(time,predator_node,prey_node, consumption,prop.consumption) 
  
  # Initialize an empty list to store results for each time point
  time_series_results <- list()
  
  # Loop through each unique time point
  for (t in unique(food_web_df$time)) {
    # Subset data for the current time point
    df_subset <- subset(food_web_df, time == t)
    
    
    # --- Create the igraph object and calculate indices ---
    
    # Create the igraph object
    # The network is directed from prey_node to predator.
    # We use the unique predator_node to account for age.
    # The 'prop.consumption' is used as an edge weight.
    g_prop <- igraph::graph_from_data_frame(d = df_subset[, c("prey_node", "predator_node", "prop.consumption")], directed = TRUE)
    g_undirected = igraph::as_undirected(g_prop, mode = "collapse", edge.attr.comb = 'sum')
    g_consumed = igraph::graph_from_data_frame(d = df_subset[, c("prey_node", "predator_node", "consumption")],directed = TRUE)
    
    
    g_membership_undirected = igraph::cluster_louvain(g_undirected, weights = igraph::E(g_undirected)$prop.consumption)
    g_membership_directed = igraph::cluster_infomap(g_prop, e.weights = igraph::E(g_prop)$prop.consumption)

    # Connectance: The number of links (L) divided by the maximum possible links (S^2)
    num_species <- igraph::vcount(g_consumed)
    num_links <- igraph::ecount(g_consumed)
    connectance <- num_links / (num_species^2)
    
    # Centrality: Measure of species' influence.
    # We calculate the mean values for the network time-series summary.
    mean_in_degree <- mean(igraph::degree(g_consumed, mode = "in")) # Average number of prey_node a species has
    mean_out_degree <- mean(igraph::degree(g_consumed, mode = "out")) # Average number of predators a species has
    mean_betweenness <- mean(igraph::betweenness(g_prop)) # Average number of shortest paths passing through a species
    
    # New metric: Network Betweenness Centralization
    network_betweenness_centralization <- igraph::centr_betw(g_prop)$centralization
    
    # Modularity: Measures the extent to which the network is partitioned into communities
    modularity_undirected <- igraph::modularity(g_undirected, igraph::membership(g_membership_undirected))
    modularity_directed <- igraph::modularity(g_prop, igraph::membership(g_membership_directed))
    
    # Redundancy: A proxy for this is the inverse of the standard deviation of in-degrees.
    # A lower standard deviation suggests a more even distribution of prey_node, indicating
    # higher redundancy. We use a proxy here as a direct redundancy index is complex.
    in_degrees <- igraph::degree(g_consumed, mode = "in")
    redundancy_proxy <- 1 / (sd(in_degrees) + 1e-6) # Added small value to avoid division by zero
    
    # print(paste0(t,'-simple metrics'))
    # Ascendancy, Capacity, and Coherence: These require a flow matrix (who eats how much)
    # We'll use the 'consumed_amount' for this.
    all_species <- unique(c(df_subset$prey_node, df_subset$predator_node))
    flow_matrix <- matrix(0, nrow = length(all_species), ncol = length(all_species),
                          dimnames = list(all_species, all_species))
    
    # Populate the flow matrix
    for (i in 1:nrow(df_subset)) {
      prey_sp <- as.character(df_subset$prey_node[i])
      pred_sp <- as.character(df_subset$predator_node[i])
      flow <- df_subset$consumption[i]
      
      if (prey_sp %in% all_species && pred_sp %in% all_species) {
        flow_matrix[prey_sp, pred_sp] <- flow_matrix[prey_sp, pred_sp] + flow
      }
    }
    
    asc_results <- NetIndices::AscInd(flow_matrix) |> as.data.frame()
    ascendancy_val <- asc_results$Ascendency[1]
    capacity_val <- asc_results$Capacity[1]
    overhead_val <- asc_results$Overhead[1]
    rel_ascendancy_val <- ascendancy_val / capacity_val
    
    # print(paste0(t,'-ascendancy metrics'))
    # Use the custom function to calculate coherence
    all_species_in_time_step <- unique(c(as.character(df_subset$prey_node), as.character(df_subset$predator_node)))
    basal_species_for_current_time <- intersect(basal_species_list, all_species_in_time_step)
    trophic_levels <- atlantiseof::calculate_trophic_levels_corrected(flow_matrix, basal_species = basal_species_for_current_time)
    coherence_val <- atlantiseof::calculate_coherence(flow_matrix, trophic_levels)
    
    # print(paste0(t,'-coherence metrics'))
    # New metric: Average Jaccard Similarity
    avg_jaccard_similarity <- atlantiseof::calculate_avg_jaccard(g_consumed)
    
    resilience_eigenvalue <- tryCatch(atlantiseof::calculate_resilience(flow_matrix), error = function(e) NA)
    
    # print(paste0(t,'-jaccard metrics'))
    # Store all results for the current time point
    time_series_results[[as.character(t)]] <- data.frame(
      time = t,
      connectance = connectance,
      mean_in_degree = mean_in_degree,
      mean_out_degree = mean_out_degree,
      mean_betweenness = mean_betweenness,
      network_betweenness_centralization = network_betweenness_centralization,
      modularity_directed = modularity_directed,
      modularity_undirected = modularity_undirected,
      redundancy_proxy = redundancy_proxy,
      avg_jaccard_similarity = avg_jaccard_similarity,
      ascendancy = ascendancy_val,
      capacity = capacity_val,
      coherence = coherence_val,
      overhead = overhead_val, 
      rel_ascendancy = rel_ascendancy_val,
      resilience_eigenvalue = resilience_eigenvalue
    )
    print(t)
  }
  
  # Combine all the stored data frames into a single one
  final_df <- do.call(rbind, time_series_results) |> 
    dplyr::mutate(reactive_acendancy = ascendancy/capacity)
  
  
  
  if(show.plot){
    
    #faceted ggplot of timeseries of each indicator
    long_df <- final_df |> 
      tidyr::pivot_longer(
        cols = -time,
        names_to = "metric",
        values_to = "value"
      )
    
    # Create the faceted plot
    ggplot2::ggplot(long_df, ggplot2::aes(x = time, y = value, group = metric)) +
      ggplot2::geom_line(color = "#0072B2", size = 1) +
      ggplot2::geom_point(color = "#0072B2", size = 2) +
      ggplot2::facet_wrap(~ metric, scales = "free_y", ncol = 3) +
      ggplot2::labs(
        title = "Food Web Metrics Over Time",
        x = "Time",
        y = "Metric Value"
      ) +
      ggplot2::theme_bw()+
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
        strip.background = ggplot2::element_rect(fill = "#0072B2", color = "white"),
        strip.text = ggplot2::element_text(color = "white", face = "bold")
      )
    ggplot2::ggsave(paste0(out.dir, '/foodweb_metrics_timeseries.png'), plot = p, width = 12, height = 8)
    
      
    
    
    corr.df = cor(final_df[,-1])
    ggcorrplot::ggcorrplot(corr.df, method = 'circle', hc.order =T, type = 'lower')+
      ggplot2::theme_bw()+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) 
    ggplot2::ggsave(paste0(out.dir, '/foodweb_corrplot.png'), width = 12, height = 10)
        
  }
  
  #calculates the status of metrics
  # Re-written get_metric_status function for better oscillation detection
  get_metric_status <- function(values, time, name) {
    # Check if time series has enough data points
    if (length(values) < 5) {
      return("NA")
    }
    
    # Time series decomposition to check for a seasonal component
    ts_data <- ts(values, frequency = 4) # Assuming 4 time points per cycle for simplicity
    tryCatch({
      decomposed <- stl(ts_data, s.window = "periodic", na.action = na.exclude)
      # Check if the seasonal variance is a significant portion of the total variance
      seasonal_var <- var(decomposed$time.series[, "seasonal"], na.rm = TRUE)
      total_var <- var(values, na.rm = TRUE)
      if (!is.na(seasonal_var) && !is.na(total_var) && total_var > 0 && (seasonal_var / total_var) > 0.5) {
        return("Oscillating")
      }
    }, error = function(e) {
      # If STL fails (e.g., due to short series), fall back to other checks
      message(paste("STL failed for", name, ":", e$message))
    })
    
    # Rest of the logic (stable, increasing, decreasing) remains the same
    if (sd(values, na.rm = TRUE) / mean(values, na.rm = TRUE) < 0.1) {
      return("Stable")
    }
    model <- lm(values ~ time)
    slope <- summary(model)$coefficients[2, 1]
    p_value <- summary(model)$coefficients[2, 4]
    p_value_threshold <- 0.05
    if (p_value < p_value_threshold) {
      if (slope > 0) {
        return("Increasing")
      } else {
        return("Decreasing")
      }
    }else{
      return("Stable")  
    }
  }

  # Helper function to determine ecosystem status based on metric statuses
  # Refined determine_ecosystem_status function to use simplified statuses
  determine_ecosystem_status <- function(statuses) {
    if (statuses$ascendancy == "Decreasing" || statuses$ascendancy == "Decreasing Sharply" ||
        statuses$connectance == "Decreasing" || statuses$connectance == "Decreasing Sharply" ||
        statuses$resilience_eigenvalue == "Increasing" || statuses$resilience_eigenvalue == "Increasing Sharply") {
      return("Disrupted & Collapsing")
    }
    if (statuses$connectance == "Decreasing" && statuses$overhead == "Decreasing" && statuses$ascendancy == "Stable" &&
        (statuses$resilience_eigenvalue == "Increasing" || statuses$resilience_eigenvalue == "Stable")) {
      return("Shrinking but Streamlined")
    }
    if (statuses$connectance == "Increasing" && (statuses$coherence == "Decreasing" || statuses$coherence == "Low") && (statuses$modularity == "Decreasing" || statuses$modularity == "Low")) {
      return("Growing but Disorganized")
    }
    if ((statuses$connectance == "Increasing" || statuses$connectance == "Increasing Sharply") &&
        (statuses$ascendancy == "Increasing" || statuses$ascendancy == "Increasing Sharply") &&
        (statuses$modularity == "Increasing" || statuses$modularity == "Stable") &&
        (statuses$resilience_eigenvalue == "Decreasing" || statuses$resilience_eigenvalue == "Stable")) {
      return("Developing & Resilient")
    }
    if (statuses$connectance == "Stable" && statuses$modularity == "Stable" && statuses$ascendancy == "Increasing" &&
        (statuses$resilience_eigenvalue == "Decreasing" || statuses$resilience_eigenvalue == "Stable")) {
      return("Mature & Productive")
    }
    if (statuses$connectance == "Stable" && (statuses$modularity == "Decreasing" || statuses$modularity == "Stable") && statuses$overhead == "Decreasing" &&
        (statuses$resilience_eigenvalue == "Stable" || statuses$resilience_eigenvalue == "Increasing")) {
      return("Stressed & Vulnerable")
    }
    if (statuses$connectance == "Stable" && statuses$ascendancy == "Stable" && statuses$modularity == "Stable" && statuses$resilience_eigenvalue == "Stable") {
      return("Mature & Stable")
    }
    if (any(statuses == "Oscillating")) {
      return("Cyclical & Pulsing")
    }
    return("Unknown State")
  }
  
  # Apply the status functions to each metric in the final data frame
  data_timerange = dplyr::filter(final_df,(time/365) %in% timeRange)
  metric_statuses <- sapply(names(data_timerange)[-1], function(col_name) {
    get_metric_status(data_timerange[[col_name]], data_timerange$time, col_name)
  })
  
  # Convert the results to a data frame
  metric_statuses_df <- as.data.frame(t(as.data.frame(metric_statuses)))
  names(metric_statuses_df) <- names(data_timerange)[-1]
  
  # Determine the overall ecosystem status
  ecosystem_status <- determine_ecosystem_status(as.list(metric_statuses_df))
  
  # Create the final output data frame
  ecosystem_status_summary <- data.frame(Ecosystem_Status = ecosystem_status, metric_statuses_df)

  #Calculate normalized slopes on all metrics and return
  calculate_normalized_slope_and_mean <- function(values, time_vector) {
    # Create a data frame, remove NA values
    data <- data.frame(time = time_vector, value = values)
    data <- na.omit(data)
    
    # Return NA if not enough data points for a slope calculation
    if (nrow(data) < 2) {
      return(list(normalized_slope = NA, mean_value = NA))
    }
    
    # Calculate the linear model
    model <- lm(value ~ time, data = data)
    
    # Extract the slope
    slope <- coef(model)[2]
    
    p_value <- summary(model)$coefficients[2, 4]
    r2 = summary(model)$r.squared
    
    # Calculate the mean value of the time series
    mean_value <- mean(data$value, na.rm = TRUE)
    
    # Normalize the slope
    normalized_slope <- NA # Default to NA
    if (!is.na(mean_value) && mean_value != 0) {
      normalized_slope <- slope / mean_value
    }
    
    return(list(normalized_slope = normalized_slope, mean_value = mean_value, p_value = p_value, r2 = r2))
  }
  
  # Use sapply to apply the function to each metric column
  results_list <- sapply(names(data_timerange)[-1], function(col_name) {
    calculate_normalized_slope_and_mean(final_df[[col_name]], final_df$time)
  }, simplify = FALSE)
  
  # Convert the list of results into a data frame
  metric_summary_df <- data.frame(
    metric = names(results_list),
    normalized_slope = sapply(results_list, "[[", "normalized_slope"),
    mean_value = sapply(results_list, "[[", "mean_value"),
    p_value = sapply(results_list, "[[", "p_value"),
    r2 = sapply(results_list, "[[", "r2")
  ) |> 
    dplyr::left_join(tidyr::pivot_longer(ecosystem_status_summary, cols = everything(), names_to = "metric", values_to = "status"))
 

  
  out.ls = list(
    metric_slope = metric_summary_df,
    ecoystem_status = ecosystem_status_summary$ecosystem_status,
    food_web_df = food_web_df,
    energy_flow_df = g_consumed,
    metric_ts = final_df
  )
  
  # tictoc::toc() 
  return(out.ls)
  
}