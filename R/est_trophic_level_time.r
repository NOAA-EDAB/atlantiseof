#' Estimate Trophic level over time from detailed diet
#'
#'
#' Need to process detailed diet data first
#' linux command line
#' # zip up file
#'   gzip -k neus_outputDetailedDietCheck.txt
#'   # then remove zeros and save ans another zip
#'   zcat neus_outputDetailedDietCheck.txt.gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > DetDiet.gz
#'   # strip headers from original file
#'   zcat neus_outputDetailedDietCheck.txt.gz | head -n1 | gzip > DetDietHead.gz
#'   # concatenate them
#'   cat DetDietHead.gz DetDiet.gz > DetDiet2.gz
#'
#'@param param.dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param fgs Character String. Name of groups.csv file
#'@param detDietfile Character String. Name of processed zipped detailed diet file
#'@param timeRange Vector
#'@param plottl Boolean. Plot the Trophic level to window. Default = F
#'
#'@return list
#'\item{trophiclevel}{dataframe. Species, year, trophic level}
#'\item{figuretl}{ggplot2 object. trophic level plots over time by species}
#'
#'@export




est_trophic_level_time <- function(param.dir = "C:/Users/andrew.beet/Documents/myWork/githubRepos/neus-atlantis/currentVersion",
                              atl.dir = here::here("other/dev_3a75e57d1"),
                              fgs = "neus_groups.csv",
                              detDietfile = "DetDiet2.gz",
                              timeRange = 1988:2017,
                              plottl=F) {

  fgs <- atlantisom::load_fgs(dir = param.dir,file_fgs= fgs)
  #Total consumption reported. flagdietcheck = 1
  dietmat <- atlantisom::load_detailed_diet_comp(dir = atl.dir,
                                                 file_diet = detDietfile,
                                                 fgs = fgs)
    # dplyr::filter(time.days < 19711) # 2017

  # TL over space, time, all age classes
  # average consumption per time step is calculated
  consump <- dietmat %>%
    dplyr::group_by(species,prey) %>%
    dplyr::summarise(totalconsumption = sum(atoutput),
                     timesteps = unique(time.days) %>% length(),
                     consumption = totalconsumption/timesteps,
                     .groups = "drop") %>%
    dplyr::select(species,prey,consumption)


  # TL over space, time > 1988 per year, all age classes
  # average consumption per year step is calculated
  dietmat2 <- dietmat %>%
    dplyr::mutate(year = 1963 + ceiling(-.1 + time.days/365))
  nsp <- length(unique(c(dietmat$species,dietmat$prey)))

  if(is.null(timeRange)) {
    atlRange <- range(dietmat2$year)
    yrs <- atlRange[1]:atlRange[2]
  } else {
    yrs <- timeRange
  }
  TLtime <- matrix(data = NA,nrow = nsp,ncol = length(yrs))
  ii <- 0
  for (iy in yrs) {
    ii <- ii + 1
    consump <- dietmat2 %>%
      dplyr::filter(year == iy) %>%
      dplyr::group_by(species,prey) %>%
      dplyr::summarise(totalconsumption = sum(atoutput),
                       timesteps = unique(time.days) %>% length(),
                       consumption = totalconsumption/timesteps,
                       .groups = "drop") %>%
      dplyr::select(species,prey,consumption)


    predtotalconsump <- consump %>%
      dplyr::group_by(species) %>%
      dplyr::summarise(totconsumption = sum(consumption))

    fji <- dplyr::left_join(consump,predtotalconsump,by = "species") %>%
      dplyr::mutate(fji = consumption/(totconsumption)) %>%
      dplyr::select(species,prey, fji)

    allspecies <- union(unique(fji$prey),unique(fji$species))
    preyspecies <- setdiff(unique(fji$prey),unique(fji$species))

    added <- data.frame(species = preyspecies,prey = "Diatom",fji = 0)

    fji <- rbind(fji,added)


    # 1. Pivot to wide format (Predators in Rows, Prey in Cols)
    tt <- tidyr::pivot_wider(fji, names_from = prey, values_from = fji, values_fill = 0)
    
    # 2. The Master List of species is the 'species' column (Rows)
    # (The previous 'added' logic ensured all basal prey are included here)
    master_species_list <- tt$species
    
    # 3. Identify which species are missing from the columns (Top Predators)
    missing_cols <- setdiff(master_species_list, colnames(tt))
    
    # 4. Add missing columns filled with 0
    if(length(missing_cols) > 0){
      tt[missing_cols] <- 0
    }
    
    # 5. Strict Ordering and Matrix Conversion
    # We select columns exactly matching the order of the species rows
    newtt <- tt %>%
      dplyr::select(dplyr::all_of(master_species_list)) %>%
      as.matrix()
    
    # 6. Assign rownames matches the columns
    rownames(newtt) <- master_species_list
    
    # 7. Invert the matrix
    # Now newtt is guaranteed to be N x N, so diag(N) - (N x N) works.
    oneminusf <- diag(nrow(newtt)) - newtt
    TLi <- solve(oneminusf, rep(1, nrow(newtt)))
    TLtime[,ii] <- TLi


  }

  TLtimes <- as.data.frame(TLtime)
  colnames(TLtimes) <- as.character(yrs)
  TLoverTime <- TLtimes %>%
    dplyr::mutate(species = names(TLi)) %>%
    dplyr::relocate(species) %>%
    tidyr::pivot_longer(.,cols=-species,names_to = "year",values_to = "TL") %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::left_join(fgs, by = c('species' = 'Name')) |> 
    dplyr::select(species,Code,year,TL)

  # plot TL change over time

  p1 <- ggplot2::ggplot(TLoverTime) +
    ggplot2::geom_line(ggplot2::aes(x = year, y = TL)) +
    ggplot2::facet_wrap(~species,scales = "fixed") +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, hjust=1)) +
    ggplot2::ylab("Trophic Level based on diet matrix") +
    ggplot2::xlab("")


  if(plottl) {
    print(p1)
  }

  return(list(trophiclevel = TLoverTime,figuretl = p1))


}
