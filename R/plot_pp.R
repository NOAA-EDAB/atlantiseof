#' Plot primary production data from Atlantis forcing files
#'
#' @param ppdata list. output from \code{ppdata}
#' @param filterbyepu Character string. Name of epu to plot. (Options = "MAB","GB","GOM","SS") Default = NULL
#' @param filterbypp Character string. Name of primary producer to filter by. Default = "Diatom_N"
#'
#' @return Figures sent to Plots window
#'
#' @export

plot_pp <- function(ppdata,filterbyepu=NULL,filterbypp="Diatom_N"){


  p <- ggplot2::ggplot(data = ppdata$annual %>% dplyr::filter(year >=1998)) +
    ggplot2::geom_line(ggplot2::aes(x=year,y=value)) +
    ggplot2::ggtitle("Annual Daily Average") +
    ggplot2::ylab("N (tons)")
  plot(p)

  # process the get_pp data and sum over boxes to epu
  # note box values of N are total N (tons) per box volume NOT mg m-3
  epudata <- ppdata$dailyspeciesbox %>%
    dplyr::left_join(.,NEFSCspatial::Neus_atlantis %>% sf::st_as_sf(),by = c("box"="BOX_ID")) %>%
    dplyr::group_by(year,day,epu,variable) %>%
    dplyr::summarise(N = sum(value),.groups="drop") %>%
    dplyr::mutate(t = lubridate::date_decimal(year+day/366))


  # facet plot
  p <- ggplot2::ggplot(data = epudata %>% dplyr::filter(year >=1998)) +
    ggplot2::geom_line(ggplot2::aes(x=t,y=N,color=epu)) +
    ggplot2::facet_wrap(ggplot2::vars(variable)) +
    ggplot2::ggtitle("Daily Nitrogen Availability") +
    ggplot2::ylab("N (tons)")
  plot(p)


  if(!is.null(filterbyepu)) {
    p <- ggplot2::ggplot(data = epudata %>% dplyr::filter(year >=1998,epu == filterbyepu)) +
      ggplot2::geom_line(ggplot2::aes(x=t,y=N)) +
      ggplot2::facet_wrap(ggplot2::vars(variable)) +
      ggplot2::ggtitle(paste0("Daily Nitrogen for ",filterbyepu)) +
      ggplot2::ylab("N (tons)")
    plot(p)
  }

  epudata2 <- ppdata$dailyspeciesbox %>%
    dplyr::left_join(.,NEFSCspatial::Neus_atlantis %>% sf::st_as_sf(),by = c("box"="BOX_ID")) %>%
    dplyr::group_by(year,epu,variable) %>%
    dplyr::summarise(N = sum(value),.groups="drop") %>%
    dplyr::mutate(C = 5.7*N)
  #%>%
  #  dplyr::filter(epu == "MAB")

  p <- ggplot2::ggplot(data = epudata2 %>%
                         dplyr::filter(year >=1998,variable %in% filterbypp)) +
    ggplot2::geom_line(ggplot2::aes(x=year,y=C/1e6)) +
    ggplot2::facet_wrap(ggplot2::vars(epu),scales="free_y") +
    ggplot2::ggtitle(paste0("Annual Carbon for ",filterbypp)) +
    ggplot2::ylab("C (million tons)")
  print(p)



}
