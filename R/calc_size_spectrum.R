#' Calculate Normalized Biomass Size Spectrum (NBSS) Slope
#'
#' This function extracts abundance and weight data from Atlantis outputs, 
#' calculates total biomass per group, bins the ecosystem into logarithmic 
#' weight classes, isolates the descending limb, and calculates the NBSS slope.
#'
#' @param atl.dir Character. Path to Atlantis output directory
#' @param param.dir Character. Path to Atlantis parameter directory
#' @param fgs.file Character. Path to the functional groups CSV file
#' @param aggregate_spatial Logical. If TRUE, aggregates biomass system-wide 
#'   across all polygons before calculating a global slope. If FALSE, calculates 
#'   the NBSS slope per polygon. Defaults to TRUE.
#' @param log10_bin_width Numeric. The width of the weight bins in log10 space. 
#'   Defaults to 0.5 (half an order of magnitude).
#'
#' @return A data frame containing the size spectrum slope (`size.spectrum.slope`), 
#'   R-squared (`r2`), and intercept (`intercept`) per time step.
#'
#' @importFrom dplyr select rename left_join mutate filter bind_rows group_by summarise arrange all_of
#' @importFrom ncdf4 nc_open ncvar_get ncatt_get nc_close
#' @importFrom rbgm bgmfile
#' @importFrom atlantistools load_nc get_boundary load_box
#' @importFrom tidyr drop_na
#' @importFrom stats lm coef summary
#'
#' @export
calc_size_spectrum = function(atl.dir, param.dir, fgs.file,
                              aggregate_spatial = TRUE, log10_bin_width = 0.5){
  
  message("Extracting and formatting Atlantis output data...")
  fgs = read.csv(fgs.file)
  
  #get N for inverts
  init.file = list.files(param.dir,'neus_init.nc',recursive = T, full.names = T)
  main.nc.file = list.files(atl.dir, 'neus_output.nc', recursive = T, full.names = T)
  bgm.file = list.files(param.dir, '.bgm',full.names = T)
  
  invert.code = fgs$Code[which(fgs$NumCohorts ==1 & fgs$IsTurnedOn == 1)]
  invert.name = fgs$Name[which(fgs$NumCohorts ==1 & fgs$IsTurnedOn == 1)]
  invert.age.code = fgs$Code[which(fgs$NumCohorts ==2 & fgs$IsTurnedOn == 1)]
  invert.age.name = fgs$Name[which(fgs$NumCohorts ==2 & fgs$IsTurnedOn == 1)]
  invert.code.all = c(paste0('j',invert.age.code), invert.age.code, invert.code)
  
  vert.code  = fgs$Code[which(fgs$NumCohorts > 2 & fgs$IsTurnedOn == 1)]
  vert.name  = fgs$Name[which(fgs$NumCohorts > 2 & fgs$IsTurnedOn == 1)]
  
  #Get N1 and N2 for age structured inverts
  main.nc = ncdf4::nc_open(main.nc.file)
  nc.names = names(main.nc$var)
  invert.age.nc.names = paste0(invert.age.name,'_N',1:2)
  
  box.area = rbgm::bgmfile(bgm.file)$boxes |> 
    dplyr::select(.bx0, area) |> 
    dplyr::rename(polygon = '.bx0')
  
  box.dz = readRDS(list.files(atl.dir,'nominal_dz.rds',recursive = T, full.names = T)) |> 
    dplyr::mutate(layer = layer) |> 
    dplyr::filter(layer != 5) |> 
    dplyr::rename(dz = 'atoutput')
  
  box.vol = box.area |> 
    dplyr::left_join(box.dz, by = 'polygon') |> 
    dplyr::mutate(volume = area * dz) |> 
    dplyr::select(polygon, layer, volume)
  
  #Make static array of box x layer with volume as variable
  get_invert_age_N = function(name,agecl){
    var = paste0(name,'_N',agecl)
    var.dat = ncdf4::ncvar_get(main.nc, var)
    var.units = ncdf4::ncatt_get(main.nc,var)$units
    var.dim = dim(var.dat)
    nc.times = main.nc$dim$t$vals/86400
    
    if(var.units %in% c('mg N m-3','mg N2 m-3')){
      #remove bottom layer
      var.dat = var.dat[-var.dim[1],,]
      dimnames(var.dat) = list(
        layer =  (nrow(var.dat):1)-1,
        polygon = (1:ncol(var.dat)) -1,
        time = nc.times
      )
    } else if(var.units == 'mg N m-2'){
      var.dat = var.dat[var.dim[1],,]
      dimnames(var.dat) = list(
        layer =  var.dim[1],
        polygon = (1:ncol(var.dat)) -1,
        time = nc.times
      )
    }
    var.df = as.data.frame.table(var.dat) |> 
      dplyr::mutate(layer = as.integer(as.character(layer)),
                    polygon = as.integer(as.character(polygon)),
                    time = as.integer(as.character(time))) |> 
      dplyr::rename(atoutput = 'Freq') |> 
      dplyr::mutate(species=name,agecl = agecl)
    
    return(var.df)
  }
  
  #combinations of invert.age.name and agecl 1,2
  invert.age.combs = expand.grid(name = invert.age.name, agecl = 1:2)
  invert.age.ls = list()
  for(i in 1:nrow(invert.age.combs)){
    invert.age.ls[[i]] = get_invert_age_N(invert.age.combs$name[i], invert.age.combs$agecl[i])
  }
  invert.age.n =  dplyr::bind_rows(invert.age.ls) |> 
    dplyr::filter(atoutput>0)
  
  # Close NC connection
  ncdf4::nc_close(main.nc)
  
  bps.names = atlantistools::load_bps(init = list.files(param.dir,'neus_init.nc',recursive = T, full.names = T),
                                      fgs = fgs.file)
  
  invert.n = atlantistools::load_nc(nc = main.nc.file,
                                    fgs = fgs.file,
                                    bps = bps.names,
                                    select_groups = invert.name,
                                    select_variable = 'N',
                                    prm_run = list.files(param.dir,'at_run.prm',full.names = T),
                                    bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm = bgm.file))
  )
  
  #Get total biomass from density
  invert.n.all = invert.n |> 
    dplyr::bind_rows(invert.age.n) |> 
    dplyr::rename(LongName = 'species') |> 
    dplyr::left_join(dplyr::select(fgs, Code, Name, LongName), by = "LongName") |> 
    dplyr::mutate(is.epi = Name %in% bps.names,
                  layer = ifelse(layer == 4, NA, layer)) |> 
    dplyr::left_join(box.area, by = "polygon") |> 
    dplyr::left_join(box.vol, by = c("polygon", "layer")) |> 
    dplyr::mutate(mgN = ifelse(is.epi, 
                               atoutput * area, 
                               atoutput * volume))
  
  vert.sn = atlantistools::load_nc(nc = main.nc.file,
                                   fgs = fgs.file,
                                   bps = bps.names,
                                   select_groups = vert.name,
                                   select_variable = 'StructN',
                                   prm_run = list.files(param.dir,'at_run.prm',full.names = T),
                                   bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm = bgm.file))
  ) |> 
    dplyr::rename(StructN = 'atoutput',
                  LongName = 'species')
  
  vert.rn = atlantistools::load_nc(nc = main.nc.file,
                                   fgs = fgs.file,
                                   bps = bps.names,
                                   select_groups = vert.name,
                                   select_variable = 'ResN',
                                   prm_run = list.files(param.dir,'at_run.prm',full.names = T),
                                   bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm = bgm.file))
  )|> 
    dplyr::rename(ResN = 'atoutput',
                  LongName = 'species')
  
  vert.num = atlantistools::load_nc(nc = main.nc.file,
                                    fgs = fgs.file,
                                    bps = bps.names,
                                    select_groups = vert.name,
                                    select_variable = 'Nums',
                                    prm_run = list.files(param.dir,'at_run.prm',full.names = T),
                                    bboxes = atlantistools::get_boundary(atlantistools::load_box(bgm = bgm.file))
  )|> 
    dplyr::rename(num = 'atoutput',
                  LongName = 'species')
  
  #Get size for inverts
  get_global_param = function(param){
    param.line = grep(param,bio.lines, value =T)
    if(length(param.line) == 0){
      return(NA)
    }else{
      param.split = strsplit(param.line, ' |\t')[[1]]
      param.split = param.split[param.split!='']
      param.val = as.numeric(param.split[2])
      return(param.val)
    }
  }
  bio.file = list.files(param.dir, 'at_biology.prm',full.names = T)
  bio.lines = readLines(bio.file)
  li.a.invert = get_global_param('li_a_invert')
  li.b.invert = get_global_param('li_b_invert')
  xrs = get_global_param('X_RS')
  xcn = get_global_param('X_CN')
  wetdry = get_global_param('wetdry')
  
  invert.sn = data.frame(Code = invert.code.all,
                         sn =sapply(invert.code.all,function(x){
                           sn.val = get_global_param(paste0('^',x,'_sn'))
                           return(sn.val)
                         })
  )
  
  #Get vertebrate li_a and li_b
  vert.lw = data.frame(Code = vert.code,
                       li.a =sapply(vert.code,function(x){
                         sn.val = get_global_param(paste0('^li_a_',x))
                         return(sn.val)
                       }),
                       li.b=sapply(vert.code,function(x){
                         sn.val = get_global_param(paste0('^li_b_',x))
                         return(sn.val)
                       })
  )
  
  #convert N to length
  len.num.invert = invert.n.all |> 
    dplyr::left_join(invert.sn, by = 'Code') |> 
    dplyr::mutate(wgt.ind.mgN = (1+ xrs)*sn,
                  wgt.ind.g = wgt.ind.mgN * wetdry * xcn * 1E-3,
                  num = mgN/wgt.ind.g,
                  len.cm = (wgt.ind.g/li.a.invert)^(1/li.b.invert)) |> 
    dplyr::select(Code, agecl, polygon, layer, time,len.cm, wgt.ind.g, num) |> 
    dplyr::mutate(layer = ifelse(is.na(layer), 4, layer),
                  time = floor(time/365))
  
  #Get length for verts
  len.num.vert = vert.sn |> 
    dplyr::left_join(vert.rn, by = c("LongName", "polygon", "layer", "time", "agecl")) |> 
    dplyr::left_join(vert.num, by = c("LongName", "polygon", "layer", "time", "agecl")) |> 
    dplyr::left_join(dplyr::select(fgs, Code, Name, LongName), by = "LongName") |> 
    dplyr::left_join(vert.lw, by = "Code") |> 
    dplyr::mutate(wgt.ind.g = (ResN + StructN) * wetdry * xcn * 1E-3,
                  len.cm = (wgt.ind.g/li.a)^(1/li.b)) |> 
    dplyr::select(Code, agecl, polygon, layer, time,len.cm, wgt.ind.g, num) |> 
    dplyr::filter(!is.na(num))
  
  #combine length data
  len.num.all = len.num.invert |> 
    dplyr::bind_rows(len.num.vert) |> 
    dplyr::left_join(dplyr::select(fgs, Code, Name, LongName), by = "Code") |> 
    dplyr::mutate(biomass = wgt.ind.g * num) |>
    tidyr::drop_na(biomass, wgt.ind.g) |>
    dplyr::filter(wgt.ind.g > 0, biomass > 0)
  
  
  # ============================================================================
  # --- START: NORMALIZED BIOMASS SIZE SPECTRUM (NBSS) CALCULATION ---
  # ============================================================================
  
  message("Aggregating and applying Log10 weight bins for Size Spectrum...")
  
  # 1. Spatial Aggregation Step
  if (aggregate_spatial) {
    processed_data <- len.num.all |>
      dplyr::group_by(time, Code, Name, LongName, agecl) |>
      dplyr::summarise(
        total_biomass = sum(biomass, na.rm = TRUE),
        # Calculate biomass-weighted mean body weight across all boxes
        mean_wgt_g = sum(wgt.ind.g * biomass, na.rm = TRUE) / (sum(biomass, na.rm = TRUE) + 1e-12),
        .groups = "drop"
      ) |>
      dplyr::mutate(polygon = "All")
  } else {
    processed_data <- len.num.all |>
      dplyr::rename(total_biomass = biomass, mean_wgt_g = wgt.ind.g)
  }
  
  # 2. Binning by Logarithmic Weight
  processed_data <- processed_data |>
    dplyr::mutate(
      log10_wgt = log10(mean_wgt_g),
      bin_lower_log10 = floor(log10_wgt / log10_bin_width) * log10_bin_width,
      bin_upper_log10 = bin_lower_log10 + log10_bin_width,
      bin_mid_log10 = bin_lower_log10 + (log10_bin_width / 2),
      
      # Determine linear width of the log bin in grams (Delta W)
      # This is critical to Normalize the Biomass later to avoid bin-width artifacts
      delta_wgt_g = (10^bin_upper_log10) - (10^bin_lower_log10) 
    )
  
  # Sum total biomass within each weight bin
  binned_spectrum <- processed_data |>
    dplyr::group_by(time, polygon, bin_mid_log10, delta_wgt_g) |>
    dplyr::summarise(
      sum_biomass = sum(total_biomass, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 3. Fit the Size Spectrum Slopes (Descending Limb Only)
  message("Fitting Normalized Biomass Size Spectrum regressions...")
  
  results_list <- list()
  unique_groups <- binned_spectrum |> 
    dplyr::select(time, polygon) |> 
    unique()
  
  for (i in seq_len(nrow(unique_groups))) {
    this_grp <- unique_groups[i, ]
    
    sub_data <- binned_spectrum |> 
      dplyr::inner_join(this_grp, by = c("time", "polygon")) |>
      dplyr::filter(sum_biomass > 0) |>
      dplyr::arrange(bin_mid_log10) # Ensure strictly ascending size order
    
    if (nrow(sub_data) < 3) next
    
    # CRITICAL ECO-METRIC STEP: Find the Peak of the Spectrum
    # Plankton/Larvae are under-represented or highly volatile. 
    # To avoid recruitment bias, we ONLY fit the regression starting at the 
    # peak biomass bin and moving to the right (the descending limb).
    peak_idx <- which.max(sub_data$sum_biomass)
    descending_limb <- sub_data[peak_idx:nrow(sub_data), ]
    
    # NBSS Transformation: log10(Biomass / Delta_W)
    descending_limb <- descending_limb |>
      dplyr::mutate(
        normalized_biomass = sum_biomass / delta_wgt_g,
        log10_norm_biomass = log10(normalized_biomass)
      ) |>
      dplyr::filter(is.finite(log10_norm_biomass))
    
    if (nrow(descending_limb) >= 3) {
      # Fit: log10(Normalized Biomass) = intercept + slope * log10(W)
      fit <- stats::lm(log10_norm_biomass ~ bin_mid_log10, data = descending_limb)
      fit_summary <- summary(fit)
      
      results_list[[length(results_list) + 1]] <- data.frame(
        time = this_grp$time,
        polygon = this_grp$polygon,
        size.spectrum.slope = stats::coef(fit)[2],
        r2 = fit_summary$r.squared,
        intercept = stats::coef(fit)[1],
        n_bins_fitted = nrow(descending_limb),
        spectrum_type = "NBSS_Weight"
      )
    }
  }
  
  final_slopes <- dplyr::bind_rows(results_list)
  rownames(final_slopes) <- NULL
  
  message("NBSS calculation complete!")
  return(final_slopes)
}