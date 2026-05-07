#' Plot Ecological Indicators
#'
#' Plots distance metrics relative to the reference run. Generates both individual 
#' and faceted time-series plots for ecological indicators as a function of the 
#' catch scalar, highlighting the EOF threshold ranges.
#'
#' @param figure.dir Character string. Path to the directory where output figures will be saved.
#' @param eco.ind.file Character string. Path to the saved run indicators `.rds` file.
#' @param setup.file Character string or data frame. The setup data used to join with the ecological indicators.
#' @param thresholds.file Character string. Path to the saved thresholds `.rds` file.
#'
#' @return Generates and saves `.png` plot files to the specified `figure.dir`. Returns invisibly.
#' 
#' @export
plot_eco_ind <- function(figure.dir, eco.ind.file, setup.file, thresholds.file) {
  
  # Read in run indicators 
  eco.ind = base::readRDS(eco.ind.file) |> 
    dplyr::left_join(setup.file)
  
  # Get variables names
  var.names = base::unique(eco.ind$Variable)
  
  run.thresholds.df = base::readRDS(thresholds.file)
  ref.threshold.min = base::min(run.thresholds.df$threshold, na.rm = TRUE)
  ref.threshold.max = base::max(run.thresholds.df$threshold, na.rm = TRUE)
  
  # identify the catch scalar closest to the mean link threshold
  scale.min.thresh = run.distance.df$catch.scalar[base::which.min(base::abs(run.distance.df$catch.threshold - ref.threshold.min))]
  scale.max.thresh = run.distance.df$catch.scalar[base::which.min(base::abs(run.distance.df$catch.threshold - ref.threshold.max))]
  
  
  for(i in 1:base::length(var.names)){
    
    this.ind = eco.ind |>
      dplyr::filter(Variable == var.names[i]) |> 
      dplyr::arrange(run)
    
    # plot each indicator as a function of catch.scalar
    p = ggplot2::ggplot(this.ind, ggplot2::aes(x = catch.scalar, y = state.value, color = run)) +
      # polygon of threshold area
      ggplot2::annotate('polygon', x = base::c(scale.min.thresh, scale.max.thresh, scale.max.thresh, scale.min.thresh),
                        y = base::c(-Inf, -Inf, Inf, Inf),
                        fill = 'lightblue', alpha = 0.5) +
      # text to right of polygon
      ggplot2::annotate('text', x = scale.max.thresh + 0.01, y = base::max(this.ind$state.value), hjust = 0, label = 'EOF Threshold Range', color = 'black') +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::ylab(var.names[i]) +
      ggplot2::xlab('Catch Scalar') +
      ggplot2::ggtitle(base::paste0('Eco Indicator: ', var.names[i])) +
      ggplot2::theme_bw()
    
    # ggplot2::ggsave saves the last plot drawn, or the plot explicitly passed to it
    ggplot2::ggsave(filename = base::paste0(figure.dir, '/eco_ind_', var.names[i], '.png'), plot = p, width = 12, height = 8, dpi = 300)
  }
  
  # Faceted plot for all indicators
  p_all <- ggplot2::ggplot() +
    # added shaded region from min to max threshold
    ggplot2::annotate('polygon', x = base::c(scale.min.thresh, scale.max.thresh, scale.max.thresh, scale.min.thresh),
                      y = base::c(-Inf, -Inf, Inf, Inf),
                      fill = 'lightblue', alpha = 0.5) +
    ggplot2::geom_line(data = eco.ind, ggplot2::aes(x = catch.scalar, y = state.value, color = run)) +
    ggplot2::geom_point(data = eco.ind, ggplot2::aes(x = catch.scalar, y = state.value, color = run)) +
    ggplot2::facet_wrap(~Variable, scale = 'free_y') +
    ggplot2::ylab('Value') +
    ggplot2::xlab('Catch Scalar') +
    ggplot2::ggtitle('Eco Indicators') +
    ggplot2::theme_bw()
  
  ggplot2::ggsave(filename = base::paste0(figure.dir, '/eco_indicators_all.png'), plot = p_all, width = 12, height = 10, dpi = 300)
  
}