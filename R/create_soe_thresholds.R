#' Replicate Link and Watson study using NEUS region
#'
#' Use regional Primary production to create new thresholds. This comes from Kim Hyde
#' @param epu A character string indicating the EPU to use. eg. MAB, GB, GOM
#'
#'
#'@export


create_soe_thresholds <- function(epu){

# phytoplankton data (https://github.com/khyde/READ-EDAB-SOE-PHYTOPLANKTON)
  # metric tons
  ppdata <- readr::read_csv(here::here("data-raw/data/MERGED_ANNUAL_SUM-NES_EPU_STATISTICAL_AREAS_NOEST-PPD-VGPM2_CHLOR_A-CCI-STATS-V2024.csv")) |>
    dplyr::select(YEAR,SUBAREA_NAME,ANNUAL_MTON,TOTAL_PIXEL_AREA_KM2) |>
    dplyr::filter(YEAR < 2023,SUBAREA_NAME == epu)

# square kilometers
  area <- unique(ppdata$TOTAL_PIXEL_AREA_KM2)

  # values that give rise to Ryther thresholds based on global production, TL, TE etc
  PP <- c(400,450,500)*10^9
  TE <- c(.10,.12,.14)
  alpha <- c(.10,.15,.15)
  TL <- c(3.6,3.4,3.2)

  thresholds <- c(0.27,1.14,2.7)


  catch = alpha*PP*(TE^(TL-1))/area
  d <- rbind(d,c(rPP,rTE,ralpha,rTL,catch[i]))
  d <- as.data.frame(d)
  names(d) <- c("PP","TE","Alpha","TL","C")

  d |>
    dplyr::filter(C<1.15 & C > 1.13)

  d |>
    dplyr::filter(C<.28 & C > .26)

  d |>
    dplyr::filter(C<2.75 & C > 2.65)

}

