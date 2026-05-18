# ============================================================================ #
# 1. BASE DIRECTORIES & RUN PARAMETERS
# ============================================================================ #

# Base root paths (Update these when changing machines/drives)
github_root   <- "Z:/atlantiseof/currentVersion/"
atl_root      <- "C:/Users/joseph.caracappa/Documents/Data/base_run_eof/"
run_data_root <- "Z:/dropoff/Joseph.Caracappa/eof_targeting_1/analysis/"
out_dir <- "Z:/atlantiseof/data/"

# Run specific parameters
run.prefix <- "eof_targeting_1"
dietSource <- "detdiet"
timeRange  <- 30:52
out.name   <- "ref_"
desired.p  <- c(0.025, 0.975)
start.year <- 1964

# ============================================================================ #
# 2. DERIVED DIRECTORIES & FILE PATHS
# ============================================================================ #

# Directories
param.dir      <- github_root
atl.dir        <- atl_root
ref.run.dir    <- atl.dir
data.dir       <- run_data_root

# Input files
fgs.file       <- file.path(param.dir, "neus_groups.csv")
setup.file     <- paste0("Z:/atlantiseof/", run.prefix, "_setup.csv")

# Files routed through the `here` package (relative to project root)
group.index    <- here::here("data-raw", "neus_species_index.csv")
ref.state.file <- here::here("data-raw", "ref_eco_state.rds")
survdat.file   <- here::here("data-raw", "survey_lenagewgt.rds")
keystone.file  <- here::here("data-raw", "ref_foodweb_keystone_summary.csv")

# Output files used across functions
distance.file  <- file.path(out_dir, paste0(run.prefix, "_distance.rds"))
threshold.file <- file.path(out_dir, paste0(run.prefix, "_eof_threshold.rds"))
ref.ind.file   <- file.path(out_dir, paste0(run.prefix, "_run_eco_ind.rds"))
pca.file       <- file.path(out_dir, "pca_result.rds")

# Figure directories
figure.dir        <- here::here("figures")
plot.distance.dir <- out_dir # Routed here in original script
plot.pca.dir      <- here::here("figures", "eof_thresholds_3", "")

# ============================================================================ #
# 3. DATA LOADING
# ============================================================================ #

setup_data <- read.csv(setup.file)
survdat.data    <- readRDS(survdat.file)

# ============================================================================ #
# 4. WORKFLOW EXECUTION
# ============================================================================ #

# Calculate Reference Thresholds
ref.thresh <- atlantiseof::est_link_threshold(
  param.dir  = param.dir,
  atl.dir    = atl.dir,
  dietSource = dietSource,
  TL         = NA,
  TE         = c(0.1, 0.15),
  alpha      = c(0.15, 0.2),
  year       = 2000:2010
)
saveRDS(ref.thresh, paste0(out_dir, 'eof_thresholds.rds'))
ref.thresh =readRDS(paste0(out_dir, 'eof_thresholds.rds'))

range(ref.thresh$threshold, na.rm = TRUE)*9

# Make Reference and Desired States
atlantiseof::make_reference_state(
  param.dir, atl.dir, group.index, fgs.file, dietSource,timeRange =  20:60, out.name, desired.p, survdat.data = survdat.data
)

targeting_run_prefix = 'eof_targeting_1'
uniform_run_prefix = 'catch_thresholds_eof_3'
atlantiseof::make_scenario_dataset(targeting_run_prefix = targeting_run_prefix,
                                   uniform_run_prefix = uniform_run_prefix,
                                   targeting_run_root = paste0("Z:/dropoff/Joseph.Caracappa/",targeting_run_prefix,"/analysis/" ),
                                   uniform_run_root = paste0("Z:/dropoff/Joseph.Caracappa/",uniform_run_prefix,"/analysis/" ),
                                   uniform_setup_file = 'Z:/atlantiseof/catch_thresholds_eof_3_setup.csv',
                                   targeting_setup_file = 'Z:/atlantiseof/eof_targeting_1_setup.csv',
                                   ref_state_year_file = here::here("data-raw", "ref_eco_state_year.rds"),
                                   out_dir = "Z:/atlantiseof/data/" )

atlantiseof::prune_covariates(data_file = paste0(out_dir, "atlantis_EOF_scenario_data.rds"),
                              def_file = here::here('data-raw','indicator_defs.csv'),
                              out_file = paste0(out_dir,'atlantis_EOF_scenario_data_pruned.rds'),
                              cor_threshold = 0.7)
                                

atlantiseof::calc_scenario_distances(pruned_data_file =paste0(out_dir,'atlantis_EOF_scenario_data_pruned.rds'),
                                     out_dir = out_dir)
# 
# atlantiseof::make_desired_state_distance(
#   param.dir, atl.dir, dietSource, ref.state.file, data.dir, out_dir, run.prefix, setup.file,debug =T
# )
# 
# # Plotting
# atlantiseof::plot_distance_metrics(
#   figure.dir      = plot.distance.dir,
#   distance.file   = distance.file,
#   thresholds.file = threshold.file,
#   ref.state.file  = ref.state.file
# )
# 
# atlantiseof::plot_ind_pca(
#   data.dir       = out_dir,
#   ref.ind.file   = ref.ind.file,
#   ref.state.file = ref.state.file,
#   figure.dir     = plot.pca.dir
# )
# 
# # Load output objects for inspection
# loaded_thresholds <- readRDS(threshold.file)
# loaded_pca        <- readRDS(pca.file)
# 
# # Calculate Foodweb Metrics
# foodweb <- atlantiseof::calc_foodweb(
#   atl.dir    = atl.dir,
#   param.dir  = param.dir,
#   dietSource = dietSource,
#   fgs.file   = fgs.file,
#   timeRange  = timeRange,
#   bySpecies  = TRUE,
#   show.plot  = FALSE
# ) 
# 
# # Summarize and Save Foodweb Data
# foodweb.mean <- foodweb |>
#   dplyr::filter(time >= (20 * 365)) |> 
#   dplyr::group_by(species) |> 
#   dplyr::summarise(
#     relative_total_impact = mean(relative_total_impact, na.rm = TRUE),
#     keystone_idx1         = mean(keystone_idx1, na.rm = TRUE),
#     keystone_idx2         = mean(keystone_idx2, na.rm = TRUE),
#     keystone_idx3         = mean(keystone_idx3, na.rm = TRUE)
#   ) |> 
#   dplyr::mutate(
#     relative_total_impact_scaled = relative_total_impact / max(relative_total_impact)
#   )
# 
# write.csv(foodweb.mean, keystone.file, row.names = FALSE)
