#' Plot primary production data from Atlantis forcing files
#'
#' @param ppdata list. output from \code{ppdata}
#' @param chooseepu Character string. Name of epu to plot. Default = "MAB"
#'
#' @return Figures sent to Plots window
#'
#' @export

plot_pp <- function(ppdata,filterbyepu="MAB"){


  p <- ggplot2::ggplot(data = ppdata$annual %>% dplyr::filter(year >=1998)) +
    ggplot2::geom_line(ggplot2::aes(x=year,y=pp)) +
    ggplot2::ggtitle("Annual Daily Average") +
    ggplot2::ylab("N (tons)")
  plot(p)

  # process the get_pp data and sum over boxes to epu
  # note box values of N are total N (tons) per box volume NOT mg m-3
  epudata <- ppdata$dailybox %>%
    dplyr::left_join(.,NEFSCspatial::Neus_atlantis %>% sf::st_as_sf(),by = c("box"="BOX_ID")) %>%
    dplyr::group_by(year,day,epu,variable) %>%
    dplyr::summarise(N = sum(N),.groups="drop") %>%
    dplyr::mutate(t = lubridate::date_decimal(year+day/366))


  # facet plot
  p <- ggplot2::ggplot(data = epudata %>% dplyr::filter(year >=1998)) +
    ggplot2::geom_line(ggplot2::aes(x=t,y=N,color=epu)) +
    ggplot2::facet_wrap(ggplot2::vars(variable)) +
    ggplot2::ggtitle("Total Nitrogen") +
    ggplot2::ylab("N (tons)")
  plot(p)
  # MAB
  p <- ggplot2::ggplot(data = epudata %>% dplyr::filter(year >=1998,epu == filterbyepu)) +
    ggplot2::geom_line(ggplot2::aes(x=t,y=N)) +
    ggplot2::facet_wrap(ggplot2::vars(variable)) +
    ggplot2::ggtitle(paste0("Total Nitrogen for ",filterbyepu)) +
    ggplot2::ylab("N (tons)")

  plot(p)
}
