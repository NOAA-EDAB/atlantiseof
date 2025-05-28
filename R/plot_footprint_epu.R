#' plots NEUS Atlantis box structure overlaying EPU's
#'
#'@param crs Integer scalar. coordinate reference system EPSG number
#'
#'@return An sf object.
#'
#'A figure is also plotted to the Plots window
#'
#'@export

plot_footprint_epu <- function(crs=4269L){

  options(warn = -1)
  sf::sf_use_s2(FALSE)
  neusEPU <- NEFSCspatial::Neus_atlantis
  #message("Please wait ... plotting map")
  p1 <- ggplot2::ggplot(data  = neusEPU) +
    ggplot2::geom_sf(mapping=ggplot2::aes(fill=epu), alpha = .3, color = "grey" ) +
    # add centroid text for stat areas
    #ggplot2::geom_text(ggplot2::aes(x=X,y=Y,label=Id),size=2, color="black") +
    # plot neus box
    ggplot2::geom_sf(data = neusEPU,
                     ggplot2::aes(fill = BOX_ID),
                     fill = "grey",
                     alpha = 0,
                     color="black") +
    # add centroid text for neusbox
    #ggplot2::geom_text(data = neusBox,ggplot2::aes(x=X,y=Y,label=BOX_ID),size=2,color="black") +
    # resize output window
    ggplot2::coord_sf(xlim = c(-78, -63), ylim = c(34,46.5), expand = FALSE) +
    ggplot2::ggtitle("NEUS EPUs mapped to Atlantis Boxes") +
    ggplot2::ylab("") +
    ggplot2::xlab("")
  print(p1)

  return(neusEPU)

}
