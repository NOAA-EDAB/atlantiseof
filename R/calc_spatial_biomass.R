#' Calculate Spatial Biomass Gini Index
#'
#' This function reads spatially explicit Atlantis biomass output and calculates 
#' the spatial Gini coefficient. It can calculate this per species or aggregated
#' across all (or a subset) of species to measure ecosystem-level hotspotting.
#'
#' @param atl.dir Character string. Path to Atlantis output directory containing 'biomass_box.rds'
#' @param param.dir Character string. Path to Atlantis parameter directory
#' @param fgs.file Character string. Path to the functional groups CSV file
#' @param aggregate_total Logical. If TRUE, sums biomass across all selected species 
#'   per polygon before calculating a single aggregated Gini index. Defaults to FALSE.
#' @param keep_groups Character vector. Optional list of species codes (matching the 
#'   `Code` column in fgs) to include in the calculation. If NULL, keeps all.
#'
#' @return A data frame containing the spatial Gini index per timestep (and per species if not aggregated)
#'
#' @importFrom dplyr filter group_by summarise left_join select drop_na mutate
#' @importFrom tidyr complete
#' @importFrom stats na.omit
#'
#' @export
calc_spatial_biomass = function(atl.dir, param.dir, fgs.file, aggregate_total = FALSE, keep_groups = NULL){
  
  # 1. Load Functional Groups
  fgs = read.csv(fgs.file)
  
  # 2. Locate and Load Biomass Box Data
  bio.box.file = list.files(atl.dir, pattern = 'biomass_box.rds', recursive = TRUE, full.names = TRUE)
  bio.box.invert.file = list.files(atl.dir, pattern = 'biomass_box_invert.rds', recursive = TRUE, full.names = TRUE)
  
  if(length(bio.box.file) == 0) {
    stop("Error: 'biomass_box.rds' not found in the specified atl.dir")
  }
  if(length(bio.box.invert.file) == 0) {
    stop("Error: 'biomass_box_invert.rds' not found in the specified atl.dir")
  }
  
  bio.box = readRDS(bio.box.file[1])
  bio.box.invert = readRDS(bio.box.invert.file[1])
  
  bio.box = dplyr::bind_rows(bio.box,bio.box.invert)
  
  # 3. Filter by keep_groups if provided (using fgs$Code)
  if (!is.null(keep_groups)) {
    message("Filtering biomass data to specified keep_groups (matching fgs$Code)...")
    
    # Map the provided Codes to the LongNames present in the bio.box species column
    target_species <- fgs$LongName[fgs$Code %in% keep_groups]
    
    bio.box <- bio.box |> 
      dplyr::filter(species %in% target_species)
    
    if (nrow(bio.box) == 0) {
      stop("Error: No data remaining after filtering by keep_groups. Check your group Codes.")
    }
  }
  
  # 4. Aggregate Biomass if requested
  if (aggregate_total) {
    message("Aggregating biomass across species per polygon...")
    bio.box <- bio.box |>
      dplyr::group_by(time, polygon) |>
      dplyr::summarise(atoutput = sum(atoutput, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(species = "Total_Biomass")
  }
  
  # 5. Define Vectorised Gini Function
  gini_vector <- function(x) {
    x <- stats::na.omit(x)
    n <- length(x)
    
    # If there's 1 or 0 boxes, or total biomass is exactly 0, Gini is 0
    if (n < 2 || sum(x) == 0) return(0)
    
    x_sorted <- sort(x)
    
    # G = (2 * sum(i * x_i) / (n * sum(x_i))) - (n + 1)/n
    G <- (2 * sum(seq_len(n) * x_sorted) / (n * sum(x_sorted))) - (n + 1) / n
    return(G)
  }
  
  # 6. Extract all unique dynamic polygons to ensure complete spatial grids
  all_polygons <- unique(bio.box$polygon)
  
  # 7. Calculate Spatial Gini
  message("Calculating spatial Gini index...")
  
  species_spatial_gini <- bio.box |>
    # CRITICAL: Fill missing polygons with 0 biomass.
    # If a species goes locally extinct in a box, Atlantis might drop the row.
    # We need explicit 0s for the inequality math to work correctly.
    tidyr::complete(species, time, polygon = all_polygons, fill = list(atoutput = 0)) |>
    dplyr::group_by(time, species) |>
    dplyr::summarise(
      biomass.spatial.gini = gini_vector(atoutput),
      .groups = "drop"
    )
  
  # 8. Join functional group metadata
  if (aggregate_total) {
    # If aggregated, assign generic grouping labels to avoid NA drops downstream
    species_spatial_gini <- species_spatial_gini |>
      dplyr::mutate(
        Code = "Total",
        LongName = "Total_Biomass",
        Name = "Total_Biomass",
        GroupType = "Aggregate"
      )
  } else {
    species_spatial_gini <- species_spatial_gini |>
      dplyr::left_join(
        fgs |> dplyr::select(Code, LongName, Name, GroupType), 
        by = c("species" = "LongName")
      ) |>
      # Clean up any potential NAs from non-matching species names
      tidyr::drop_na(Name)
  }
  
  return(species_spatial_gini)
}