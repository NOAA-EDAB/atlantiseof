#' Read in the Phyto_Forcing_xxx.nc files to get estimates of PP
#'
#' Get primary production data from nc forcing files
#'
#' @param bgm Path to bgm file
#' @param pathToForcing Character sting. Path to location of forcing files
#'
#' @return list of output
#' \item{annual}{annual primary production}
#' \item{daily}{daily primary production}
#' \item{dailyspecies}{daily primary production}
#' \item{dailybox}{daily primary production by box}
#' \item{totalArea}{area domain}
#'
#'@export


get_pp <- function(bgm,pathToForcing) {

  # read bgm file
  #(0,23-29 are boundary boxes) so omit them
  # top layer is 50 m deep. are in m2
  # pp only occurs in top layer

  neusbgm <- rbgm::bgmfile(bgm)
  volume <- neusbgm$boxes %>%
    dplyr::filter(.bx0 %in% c(1:22)) %>%
    dplyr::select(.bx0,area) %>%
    dplyr::mutate(m3 = area*50)

  totalArea <- sum(volume$area)/1e6 # m2 - > km2


  PPdata <- NULL
  PPdata2 <- NULL
  for (avariable in c("Diatom_N","DinoFlag_N","PicoPhytopl_N")) {
    PPvar <- NULL
    PPvar2 <- NULL
    for (iyr in 1964:2017) {

      file <- here::here(paste0(pathToForcing,"/Phyto_Forcing_",iyr,".nc"))

      # open the file and pull the data
      dat <- ncdf4::nc_open(file)
      var <- ncdf4::ncvar_get(dat, avariable)

      ndays <- dim(var)[3]
      # dino <- ncdf4::ncvar_get(dat, "DinoFlag_N")
      # pico <- ncdf4::ncvar_get(dat, "PicoPhytopl_N")

      # daily values per box/layer
      # dim = 5 x 30 x 366 (layer x box x time)
      # note: disregard boxes 1, 24-30 since they are boundary boxes. These are the slots in the nc file.
      # these boundary boxes are actually labelled (0,23-29 boundary boxes).
      # So box 0 is index 1 in nc file

      # aggregate the data to get estimate of mean PP for the year
      # 1st dimension = layer.
      # bottommost layer = 1. Thus is a box has 5 layers, layer 5 is surface,
      # if box has 2 layers, layer 2 is surface
      # mg N m^{-3}
      # total mg N m-3 per year over footprint in top layer
      # surface layer 50 m deep

      boxns <- 2:23
      dailyboxN <- apply(var[,c(boxns),],c(2,3),sum,na.rm=T)

      # PP over NEUS (aggregate over boxes)
      # weight by box volume (tons N) day-1 (mg -> tons)
      dailyN <- colSums(dailyboxN * volume$m3) / 1e9
      df <- data.frame(year = iyr,
                       day = 1:length(dailyN),
                       value = dailyN,
                       var = avariable) %>%
        dplyr::mutate(t = lubridate::date_decimal(year+day/length(dailyN))) %>%
        tibble::as_tibble()

      PPvar <- rbind(PPvar,df)

      # PP by box by day tons N day -1
      # daily N mg-1 day-1 * volume then convert to tons

      dailytotboxN <- as.data.frame(dailyboxN * volume$m3/1e9)
      rownames(dailytotboxN) <- boxns-1 # box
      colnames(dailytotboxN) <- 1:ndays # day
      dailytotboxN <- tibble::rownames_to_column(dailytotboxN)
      names(dailytotboxN)[1] <- "box"
      df2 <- tidyr::pivot_longer(dailytotboxN,cols=-box,names_to = "day",values_to = "N") %>%
        dplyr::mutate(year = iyr,
                      variable = avariable,
                      day = as.integer(day),
                      box = as.integer(box)) %>%
        dplyr::relocate(year,day,box,variable,N)



      # df2 <- data.frame(year = iyr,
      #                  day = 1:length(dailyN),
      #                  value = dailyN,
      #                  var = avariable) %>%
      #   dplyr::mutate(t = lubridate::date_decimal(year+day/length(dailyN))) %>%
      #   tibble::as_tibble()

      PPvar <- rbind(PPvar,df)
      PPvar2 <- rbind(PPvar2,df2)
    }
    #PPvar$t <- 1:19724
    PPdata <- rbind(PPdata,PPvar)
    PPdata2 <- rbind(PPdata2,PPvar2)
  }

  dailypp <- PPdata %>%
    dplyr::group_by(year,day,t) %>%
    dplyr::summarise(pp = sum(value),.groups="drop")

  annualpp <- dailypp %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(pp = mean(pp),.groups="drop") %>%
    dplyr::mutate(t = lubridate::date_decimal(year+.5))

  pp <- list()
  pp$annual <- annualpp
  pp$daily <- dailypp
  pp$dailyspecies <-PPdata
  pp$dailybox <- PPdata2
  pp$totalArea <- totalArea

  return(pp)
}

