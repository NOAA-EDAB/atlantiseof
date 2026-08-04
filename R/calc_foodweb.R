#'Food web indices
#'calculates food web indices from detailed diet output
#'
#'@param atl.dir character string. Path to Atlantis output directory
#'@param param.dir character string. Path to Atlantis parameter directory
#'@param dietSource Character String. Whether to use realized diets (diet), detailedDiet (detdiet) or parameter files (param)
#'@param fgs.file Character String. Name of groups.csv file
#'@param plot logical. whether it should return plots as well as data
#'@param out.dir character string. Path to output directory for plots
#'@param figure.dir character string. Path to directory for figures
#'@param bySpecies logical. If TRUE, outputs a dataframe with metrics calculated for each species/node. Defaults to FALSE.
#'
#'@return list of metric timeseries, metric status, food web, and ecosystem status. If bySpecies is TRUE, returns a single dataframe of metrics.
#'
#'@export
#'

calc_foodweb = function(atl.dir,fgs.file,param.dir, dietSource,timeRange, show.plot = F, figure.dir = NA, out.dir = NA, bySpecies = FALSE, ...){
  
  # --- START: DATA LOADING SECTION ---
  fgs = read.csv(fgs.file) |> 
    dplyr::select(Code, Name, LongName, GroupType,NumCohorts)
  
  biomass.file = list.files(path = atl.dir, pattern = 'biomass_age.rds', full.names = T, recursive = T)
  biomass.invert.file = list.files(path = atl.dir, pattern = 'biomass_age_invert.rds', full.names = T, recursive = T)
  
  age.mat = atlantiseof::get_age_mat(bio.file = list.files(path =param.dir, pattern= 'at_biology.prm',full.names = T)) |> 
    dplyr::left_join(fgs) |> 
    dplyr::mutate(age.mat = as.numeric(age.mat))
  
  calc.keystone <- FALSE
  if (length(biomass.file) > 0 & length(biomass.invert.file) > 0) {
    if (length(biomass.file) > 1) {
      warning(paste("Multiple biomass files found. Using the first one:", basename(biomass.file[1])))
    }
    biomass_df <- readRDS(biomass.file[1])
    biomass_invert_df = readRDS(biomass.invert.file[1]) |> 
      dplyr::mutate(agecl = 1) 
    
    biomass_age = rbind(biomass_df, biomass_invert_df) |> 
      dplyr::left_join(fgs, by = c('species' = 'LongName')) |> 
      dplyr::left_join(age.mat) |> 
      dplyr::mutate(age.mat = ifelse(is.na(age.mat), 10, age.mat),
                    pred.stage = ifelse(agecl >= age.mat, 'adult','juvenile')) |> 
      dplyr::group_by(time, Code, species, Name, pred.stage) |> 
      dplyr::summarise(biomass = sum(atoutput, na.rm=T), .groups = 'drop') |> 
      dplyr::mutate(time = time * 365,
                    node = paste(Name, pred.stage, sep = "_"))
    
    calc.keystone <- TRUE
    print("Biomass files found and processed. Keystone indices will be calculated.")
  } else {
    warning("Biomass files ('biomass_age.rds', 'biomass_age_invert.rds') not found. Keystone indices will not be calculated.")
  }
  # --- END: DATA LOADING SECTION ---
  
  basal_species_list <- fgs$Name[which(fgs$GroupType %in% c('LG_PHY','SM_PHY','MICROPHYTOBENTHOS','SEAGRASS','PHYTOBEN'))]
  
  if(dietSource == 'detdiet'){
    dietFile = list.files(path = atl.dir, pattern = 'neus_outputDetDiet_processed.gz', full.names = T)
  }else if(dietSource == 'diet'){
    dietFile = list.files(path = atl.dir, pattern = 'neus_outputDietCheck.txt',full.names = T)
  }else if(dietSource == 'prm'){
    dietFile = list.files(path = param.dir, pattern = 'at_biology.prm',full.names = T)
  }else{
    stop('dietSource must be one of detdiet, diet, or param')
  }
  
  diet.prop = atlantiseof::get_diet_prop(param.dir,atl.dir,fgs = fgs.file, dietFile = dietFile,dietSource  = dietSource)
  
  # 1. Identify which species have age structure
  age_structured_species <- fgs$Name[fgs$NumCohorts > 1]
  
  if(any(grepl('agecl', colnames(diet.prop)))){
    food_web_df = diet.prop |> 
      dplyr::left_join(age.mat, by = c('pred' = 'Name')) |> 
      dplyr::mutate(age.mat = ifelse(is.na(age.mat), 10, age.mat),
                    pred.stage = ifelse(agecl >= age.mat, 'adult','juvenile'))
  }else{
    food_web_df = diet.prop |> 
      dplyr::left_join(fgs, by = c('pred' = 'Name')) |> 
      dplyr::mutate(pred.stage = 'adult')
    print('no predator age classes provided. all predators considered adults')
  }
  
  # 2. Conditionally append suffixes
  food_web_df <- food_web_df |> 
    dplyr::mutate(
      # Only append stage to predator if it has cohorts
      predator_node = ifelse(pred %in% age_structured_species, 
                             paste(pred, pred.stage, sep = "_"), 
                             pred),
      # Only append 'adult' to prey if it has cohorts (assuming we only track adult prey for now based on your old logic, adjust if needed)
      prey_node = ifelse(prey %in% age_structured_species, 
                         paste0(prey, '_adult'), 
                         prey)
    ) |> 
    dplyr::group_by(time,predator_node,prey_node) |>
    dplyr::summarise(consumption = sum(consumption,na.rm=T), .groups = 'drop') |> 
    dplyr::group_by(time, predator_node) |> 
    dplyr::mutate(consumption.total = sum(consumption,na.rm=T)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(prop.consumption = consumption / consumption.total) |> 
    dplyr::select(time,predator_node,prey_node, consumption,prop.consumption) 
  
  time_series_results <- list()
  
  for (t in unique(food_web_df$time)) {
    df_subset <- subset(food_web_df, time == t)
    
    g_prop <- igraph::graph_from_data_frame(d = df_subset[, c("prey_node", "predator_node", "prop.consumption")], directed = TRUE)
    g_undirected = igraph::as_undirected(g_prop, mode = "collapse", edge.attr.comb = 'sum')
    g_consumed = igraph::graph_from_data_frame(d = df_subset[, c("prey_node", "predator_node", "consumption")],directed = TRUE)
    
    all_nodes <- igraph::V(g_consumed)$name
    flow_matrix_staged <- igraph::as_adjacency_matrix(g_consumed, attr = "consumption", sparse = FALSE)
    
    # --- Calculate Standard Food Web Metrics (UNWEIGHTED/BINARY) ---
    g_membership_undirected = igraph::cluster_louvain(g_undirected, weights = igraph::E(g_undirected)$prop.consumption)
    g_membership_directed = igraph::cluster_infomap(g_prop, e.weights = igraph::E(g_prop)$prop.consumption)
    
    connectance <- igraph::ecount(g_consumed) / (igraph::vcount(g_consumed)^2)
    in_degrees <- igraph::degree(g_consumed, mode = "in")
    out_degrees <- igraph::degree(g_consumed, mode = "out")
    betweenness <- igraph::betweenness(g_prop)
    network_betweenness_centralization <- igraph::centr_betw(g_prop)$centralization
    modularity_undirected <- igraph::modularity(g_undirected, igraph::membership(g_membership_undirected))
    modularity_directed <- igraph::modularity(g_prop, igraph::membership(g_membership_directed))
    redundancy_proxy <- 1 / (sd(in_degrees) + 1e-6)
    avg_jaccard_similarity <- atlantiseof::calculate_avg_jaccard(g_consumed)
    
    # --- Calculate Effective Replacements (WEIGHTED) ---
    # 1. Effective In-Degree (Shannon Entropy of Diet -> exp(H) = "Effective number of prey")
    eff_in_degree_df <- df_subset |>
      dplyr::group_by(predator_node) |>
      dplyr::summarise(
        h = -sum(prop.consumption * log(prop.consumption + 1e-16), na.rm = TRUE),
        eff_in = exp(h),
        .groups = "drop"
      )
    eff_in_degrees <- stats::setNames(rep(0, length(all_nodes)), all_nodes)
    eff_in_degrees[eff_in_degree_df$predator_node] <- eff_in_degree_df$eff_in
    
    # 2. Effective Out-Degree (Shannon Entropy of Outflow -> exp(H) = "Effective number of predators")
    out_flow_df <- df_subset |>
      dplyr::group_by(prey_node) |>
      dplyr::mutate(prop.outflow = consumption / sum(consumption, na.rm = TRUE)) |>
      dplyr::summarise(
        h = -sum(prop.outflow * log(prop.outflow + 1e-16), na.rm = TRUE),
        eff_out = exp(h),
        .groups = "drop"
      )
    eff_out_degrees <- stats::setNames(rep(0, length(all_nodes)), all_nodes)
    eff_out_degrees[out_flow_df$prey_node] <- out_flow_df$eff_out
    
    # 3. Weighted Connectance (Total effective links / possible links)
    connectance_weighted <- sum(eff_in_degrees) / (length(all_nodes)^2)
    
    # 4. Weighted Redundancy
    redundancy_proxy_weighted <- 1 / (sd(eff_in_degrees) + 1e-6)
    
    # 5. Weighted Betweenness
    edge_distances <- 1 / (igraph::E(g_prop)$prop.consumption + 1e-12)
    betweenness_weighted <- igraph::betweenness(g_prop, weights = edge_distances)
    
    # 6. Weighted Jaccard Similarity (Ružička Similarity)
    avg_jaccard_similarity_weighted <- atlantiseof::calculate_avg_jaccard_weighted(g_prop)
    
    # --- Other Base Metrics ---
    asc_results <- NetIndices::AscInd(flow_matrix_staged) |> as.data.frame()
    ascendancy_val <- asc_results$Ascendency[1]
    capacity_val <- asc_results$Capacity[1]
    overhead_val <- asc_results$Overhead[1]
    rel_ascendancy_val <- ascendancy_val / capacity_val
    
    basal_species_for_current_time <- intersect(basal_species_list, all_nodes)
    trophic_levels <- atlantiseof::calculate_trophic_levels_corrected(flow_matrix_staged, basal_species = basal_species_for_current_time)
    coherence_val <- atlantiseof::calculate_coherence(flow_matrix_staged, trophic_levels)
    
    resilience_eigenvalue <- tryCatch(atlantiseof::calculate_resilience(flow_matrix_staged), error = function(e) NA)
    
    # --- Keystone and Impact calculations ---
    keystone_results <- NULL
    if (calc.keystone && nrow(flow_matrix_staged) > 1 && sum(flow_matrix_staged, na.rm=TRUE) > 0) {
      
      flow_df_long <- tibble::as_tibble(flow_matrix_staged, rownames = "prey_node") |>
        tidyr::pivot_longer(cols = -prey_node, names_to = "predator_node", values_to = "flow") |>
        dplyr::filter(flow > 0) |>
        dplyr::mutate(
          prey_species = gsub("_(adult|juvenile)$", "", prey_node),
          predator_species = gsub("_(adult|juvenile)$", "", predator_node)
        )
      species_flow_df <- flow_df_long |>
        dplyr::group_by(prey_species, predator_species) |>
        dplyr::summarise(total_flow = sum(flow, na.rm = TRUE), .groups = 'drop')
      all_unique_species <- unique(c(species_flow_df$prey_species, species_flow_df$predator_species))
      n_spp <- length(all_unique_species)
      species_flow_matrix <- matrix(0, nrow = n_spp, ncol = n_spp, dimnames = list(all_unique_species, all_unique_species))
      row_idx <- match(species_flow_df$prey_species, all_unique_species)
      col_idx <- match(species_flow_df$predator_species, all_unique_species)
      species_flow_matrix[cbind(row_idx, col_idx)] <- species_flow_df$total_flow
      
      current_biomass_species_df <- biomass_age[abs(biomass_age$time - t)<=1, ] |>
        dplyr::group_by(Name) |>
        dplyr::summarise(biomass = sum(biomass, na.rm = TRUE), .groups = 'drop')
      
      species_biomass <- data.frame(Name = all_unique_species) |>
        dplyr::left_join(current_biomass_species_df, by = "Name")
      total_ecosystem_biomass <- sum(species_biomass$biomass, na.rm = TRUE)
      
      if (!is.na(total_ecosystem_biomass) && total_ecosystem_biomass > 0) {
        relative_biomass <- species_biomass$biomass / total_ecosystem_biomass
        names(relative_biomass) <- species_biomass$Name
        
        mti_matrix <- NULL
        total_consumption_by_predator <- colSums(species_flow_matrix)
        G <- species_flow_matrix
        for (j in 1:ncol(G)) { if (total_consumption_by_predator[j] > 0) { G[, j] <- G[, j] / total_consumption_by_predator[j] } else { G[, j] <- 0 } }
        
        total_predation_on_prey <- rowSums(species_flow_matrix)
        F_mat <- species_flow_matrix
        for (i in 1:nrow(F_mat)) { if (total_predation_on_prey[i] > 0) { F_mat[i, ] <- F_mat[i, ] / total_predation_on_prey[i] } else { F_mat[i, ] <- 0 } }
        
        Q <- G - t(F_mat)
        I <- diag(nrow(Q))
        mti_candidate <- try(solve(I - Q) - I, silent = TRUE)
        
        if (!inherits(mti_candidate, "try-error")) {
          mti_matrix <- mti_candidate
          colnames(mti_matrix) <- rownames(mti_matrix) <- rownames(species_flow_matrix)
        } else {
          warning(paste("MTI calculation failed for time step", t, "- matrix is singular."))
        }
        
        if (!is.null(mti_matrix)) {
          mti_no_diag <- mti_matrix
          diag(mti_no_diag) <- 0
          species_rti <- colSums(abs(mti_no_diag))
          
          impact_terms <- lapply(names(species_rti), function(sp) {
            impact_exerted_neg <- sum(abs(mti_matrix[mti_matrix[, sp] < 0, sp]))
            impact_received_pos <- sum(mti_matrix[sp, mti_matrix[sp, ] > 0])
            data.frame(species = sp, 
                       species_impact_ks2 = impact_exerted_neg + impact_received_pos,
                       species_impact_ks3 = impact_exerted_neg)
          }) |> dplyr::bind_rows()
          
          keystone_results <- data.frame(species = names(species_rti), relative_total_impact = species_rti) |>
            dplyr::left_join(impact_terms, by = "species") |>
            dplyr::mutate(
              rel_biomass = relative_biomass[species],
              keystone_idx1 = log(relative_total_impact * (1 - rel_biomass)),
              keystone_idx2 = log(species_impact_ks2 * (1 - rel_biomass)),
              keystone_idx3 = log(species_impact_ks3 * (1 - rel_biomass))
            )
          keystone_results[is.infinite(keystone_results$keystone_idx1), "keystone_idx1"] <- NA
          keystone_results[is.infinite(keystone_results$keystone_idx2), "keystone_idx2"] <- NA
          keystone_results[is.infinite(keystone_results$keystone_idx3), "keystone_idx3"] <- NA
        }
      }
    }
    
    # --- Store results ---
    if (bySpecies) {
      results_df <- data.frame(
        time = t,
        node = all_nodes,
        in_degree = in_degrees[all_nodes],
        eff_in_degree = eff_in_degrees[all_nodes],
        out_degree = out_degrees[all_nodes],
        eff_out_degree = eff_out_degrees[all_nodes],
        betweenness = betweenness[all_nodes],
        betweenness_weighted = betweenness_weighted[all_nodes],
        connectance = connectance,
        connectance_weighted = connectance_weighted,
        network_betweenness_centralization = network_betweenness_centralization,
        modularity_directed = modularity_directed,
        modularity_undirected = modularity_undirected,
        redundancy_proxy = redundancy_proxy,
        redundancy_proxy_weighted = redundancy_proxy_weighted,
        avg_jaccard_similarity = avg_jaccard_similarity,
        avg_jaccard_similarity_weighted = avg_jaccard_similarity_weighted,
        ascendancy = ascendancy_val,
        capacity = capacity_val,
        coherence = coherence_val,
        overhead = overhead_val, 
        rel_ascendancy = rel_ascendancy_val,
        resilience_eigenvalue = resilience_eigenvalue
      ) |>
        dplyr::mutate(species = gsub("_(adult|juvenile)$", "", node))
      
      if (!is.null(keystone_results)) {
        results_df <- results_df |> dplyr::left_join(keystone_results, by = "species")
      } else {
        results_df$relative_total_impact <- NA_real_
        results_df$keystone_idx1 <- NA_real_
        results_df$keystone_idx2 <- NA_real_
        results_df$keystone_idx3 <- NA_real_
      }
      time_series_results[[as.character(t)]] <- results_df
      
    } else {
      results_df <- data.frame(
        time = t,
        connectance = connectance,
        connectance_weighted = connectance_weighted,
        mean_in_degree = mean(in_degrees),
        mean_eff_in_degree = mean(eff_in_degrees),
        mean_out_degree = mean(out_degrees),
        mean_eff_out_degree = mean(eff_out_degrees),
        mean_betweenness = mean(betweenness),
        mean_betweenness_weighted = mean(betweenness_weighted),
        network_betweenness_centralization = network_betweenness_centralization,
        modularity_directed = modularity_directed,
        modularity_undirected = modularity_undirected,
        redundancy_proxy = redundancy_proxy,
        redundancy_proxy_weighted = redundancy_proxy_weighted,
        avg_jaccard_similarity = avg_jaccard_similarity,
        avg_jaccard_similarity_weighted = avg_jaccard_similarity_weighted,
        ascendancy = ascendancy_val,
        capacity = capacity_val,
        coherence = coherence_val,
        overhead = overhead_val, 
        rel_ascendancy = rel_ascendancy_val,
        resilience_eigenvalue = resilience_eigenvalue
      )
      
      if(!is.null(keystone_results) && nrow(keystone_results) > 0){
        safe_which_max <- function(x) if(all(is.na(x))) NA_integer_ else which.max(x)
        rti_idx <- safe_which_max(keystone_results$relative_total_impact)
        ksi1_idx <- safe_which_max(keystone_results$keystone_idx1)
        ksi2_idx <- safe_which_max(keystone_results$keystone_idx2)
        ksi3_idx <- safe_which_max(keystone_results$keystone_idx3)
        
        results_df$keystone_spp_rti  <- keystone_results$species[rti_idx]
        results_df$keystone_spp_ksi1 <- keystone_results$species[ksi1_idx]
        results_df$keystone_spp_ksi2 <- keystone_results$species[ksi2_idx]
        results_df$keystone_spp_ksi3 <- keystone_results$species[ksi3_idx]
      } else {
        results_df$keystone_spp_rti  <- NA_character_
        results_df$keystone_spp_ksi1 <- NA_character_
        results_df$keystone_spp_ksi2 <- NA_character_
        results_df$keystone_spp_ksi3 <- NA_character_
      }
      time_series_results[[as.character(t)]] <- results_df
    }
    print(t)
  }
  
  final_df <- do.call(rbind, time_series_results)
  
  # --- Final Return & Post-Processing ---
  if (bySpecies) {
    return(final_df)
  } else {
    if(show.plot){
      numeric_cols <- sapply(final_df, is.numeric)
      long_df <- final_df[, numeric_cols] |> 
        tidyr::pivot_longer(cols = -time, names_to = "metric", values_to = "value")
      
      p <- ggplot2::ggplot(long_df, ggplot2::aes(x = time, y = value, group = metric)) +
        ggplot2::geom_line(color = "#0072B2", size = 1) +
        ggplot2::geom_point(color = "#0072B2", size = 2) +
        ggplot2::facet_wrap(~ metric, scales = "free_y", ncol = 3) +
        ggplot2::labs(title = "Food Web Metrics Over Time", x = "Time", y = "Metric Value") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
                       axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
                       strip.background = ggplot2::element_rect(fill = "#0072B2", color = "white"),
                       strip.text = ggplot2::element_text(color = "white", face = "bold"))
      ggplot2::ggsave(paste0(out.dir, '/foodweb_metrics_timeseries.png'), plot = p, width = 12, height = 8)
      
      corr.df = cor(final_df[, numeric_cols], use = "complete.obs")
      ggcorrplot::ggcorrplot(corr.df, method = 'circle', hc.order =T, type = 'lower')+
        ggplot2::theme_bw()+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) 
      ggplot2::ggsave(paste0(out.dir, '/foodweb_corrplot.png'), width = 12, height = 10)
    }
    
    get_metric_status <- function(values, time, name) {
      if (length(values) < 5) { return("NA") }
      ts_data <- ts(values, frequency = 4)
      tryCatch({
        decomposed <- stl(ts_data, s.window = "periodic", na.action = na.exclude)
        seasonal_var <- var(decomposed$time.series[, "seasonal"], na.rm = TRUE)
        total_var <- var(values, na.rm = TRUE)
        if (!is.na(seasonal_var) && !is.na(total_var) && total_var > 0 && (seasonal_var / total_var) > 0.5) {
          return("Oscillating")
        }
      }, error = function(e) { message(paste("STL failed for", name, ":", e$message)) })
      if (sd(values, na.rm = TRUE) / (mean(values, na.rm = TRUE)+1E-12) < 0.1) { return("Stable") }
      model <- lm(values ~ time)
      slope <- summary(model)$coefficients[2, 1]
      p_value <- summary(model)$coefficients[2, 4]
      if (p_value < 0.05) { if (slope > 0) { return("Increasing") } else { return("Decreasing") } } else { return("Stable") }
    }
    
    # Intentionally preserving unweighted 'connectance' dependency so existing status logic doesn't break
    determine_ecosystem_status <- function(statuses) {
      if (any(c(statuses$ascendancy, statuses$connectance) == "Decreasing") || statuses$resilience_eigenvalue == "Increasing") { return("Disrupted & Collapsing") }
      if (statuses$connectance == "Decreasing" && statuses$overhead == "Decreasing" && statuses$ascendancy == "Stable") { return("Shrinking but Streamlined") }
      if (statuses$connectance == "Increasing" && any(c(statuses$coherence, statuses$modularity_directed) == "Decreasing")) { return("Growing but Disorganized") }
      if (any(c(statuses$connectance, statuses$ascendancy) == "Increasing") && any(c(statuses$modularity_directed, statuses$resilience_eigenvalue) %in% c("Increasing", "Stable"))) { return("Developing & Resilient") }
      if (statuses$connectance == "Stable" && statuses$modularity_directed == "Stable" && statuses$ascendancy == "Increasing") { return("Mature & Productive") }
      if (statuses$connectance == "Stable" && statuses$overhead == "Decreasing") { return("Stressed & Vulnerable") }
      if (all(c(statuses$connectance, statuses$ascendancy, statuses$modularity_directed, statuses$resilience_eigenvalue) == "Stable",na.rm = T)) { return("Mature & Stable") }
      if (any(unlist(statuses) == "Oscillating")) { return("Cyclical & Pulsing") }
      return("Unknown State")
    }
    
    numeric_final_df <- final_df[, sapply(final_df, is.numeric)]
    data_timerange = dplyr::filter(numeric_final_df,(time/365) %in% timeRange)
    metric_statuses <- sapply(names(data_timerange)[-1], function(col_name) get_metric_status(data_timerange[[col_name]], data_timerange$time, col_name))
    metric_statuses_df <- as.data.frame(t(as.data.frame(metric_statuses)))
    if(is.vector(metric_statuses)) names(metric_statuses_df) <- names(data_timerange)[-1]
    ecosystem_status <- determine_ecosystem_status(as.list(metric_statuses_df))
    ecosystem_status_summary <- data.frame(Ecosystem_Status = ecosystem_status, metric_statuses_df)
    
    calculate_normalized_slope_and_mean <- function(values, time_vector) {
      data <- na.omit(data.frame(time = time_vector, value = values))
      if (nrow(data) < 2) { return(list(normalized_slope = NA, mean_value = NA, p_value = NA, r2 = NA)) }
      model <- lm(value ~ time, data = data)
      slope <- coef(model)[2]
      p_value <- summary(model)$coefficients[2, 4]
      r2 = summary(model)$r.squared
      mean_value <- mean(data$value, na.rm = TRUE)
      normalized_slope <- if (!is.na(mean_value) && mean_value != 0) slope / mean_value else NA
      return(list(normalized_slope = normalized_slope, mean_value = mean_value, p_value = p_value, r2 = r2))
    }
    
    results_list <- sapply(names(data_timerange)[-1], function(col_name) calculate_normalized_slope_and_mean(final_df[[col_name]], final_df$time), simplify = FALSE)
    metric_summary_df <- data.frame(
      metric = names(results_list),
      normalized_slope = sapply(results_list, "[[", "normalized_slope"),
      mean_value = sapply(results_list, "[[", "mean_value"),
      p_value = sapply(results_list, "[[", "p_value"),
      r2 = sapply(results_list, "[[", "r2")
    ) |> 
      dplyr::left_join(tidyr::pivot_longer(ecosystem_status_summary, cols = dplyr::everything(), names_to = "metric", values_to = "status"))
    
    out.ls = list(
      metric_slope = metric_summary_df,
      ecoystem_status = ecosystem_status_summary$Ecosystem_Status,
      food_web_df = food_web_df,
      energy_flow_df = g_consumed,
      metric_ts = final_df
    )
    return(out.ls)
  }
}