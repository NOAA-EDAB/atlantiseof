# Plots distance metrics relative to reference run
#'
#'@param figure.dir Character String. Path to figure directory
#'@param eco.ind.file Character String. Path to run indicators file
#'@param setup.file Character String. Path to setup file
#'@param thresholds.file Character String. Path to thresholds file
#'
#'
#'@return dataframe of eco indicators by year
#'
#'@export
#'
#'

# eco.ind.file  = paste0(data.dir,'catch_thresholds_eof_3_run_eco_ind.rds')

plot_distance_metrics <- function(figure.dir,eco.ind.file,setup.file,thresholds.file) {

  #Read in run indicators 
  eco.ind = readRDS(eco.ind.file) |> 
    dplyr::left_join(setup.file)

  #Get variables names
  var.names = unique(eco.ind$Variable)
  
  run.thresholds.df = readRDS(thresholds.file)
  ref.threshold.min = min(run.thresholds.df$threshold,na.rm=T)
  ref.threshold.max = max(run.thresholds.df$threshold,na.rm=T)
  
  #identify the catch scalar closest to the mean link threshold
  scale.min.thresh =run.distance.df$catch.scalar[which.min(abs(run.distance.df$catch.threshold-ref.threshold.min))]
  scale.max.thresh =run.distance.df$catch.scalar[which.min(abs(run.distance.df$catch.threshold-ref.threshold.max))]
  
  
  for(i in 1:length(var.names)){
    
    this.ind = eco.ind |>
      dplyr::filter(Variable == var.names[i]) |> 
      dplyr::arrange(run)
    
    #plot each indicator as a function of catch.scalar
    p = ggplot2::ggplot(this.ind, ggplot2::aes(x = catch.scalar, y = state.value, color = run)) +
      #polygon of threshold area
      ggplot2::annotate('polygon',x = c(scale.min.thresh,scale.max.thresh,scale.max.thresh,scale.min.thresh),
                        y = c(-Inf,-Inf,Inf,Inf),
                        fill = 'lightblue', alpha = 0.5)+
      #text to right of polygon
      ggplot2::annotate('text',x = scale.max.thresh+ 0.01, y = max(this.ind$state.value),hjust =0, label = 'EOF Threshold Range', color = 'black') +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::ylab(var.names[i]) +
      ggplot2::xlab('Catch Scalar') +
      ggplot2::ggtitle(paste0('Eco Indicator: ', var.names[i])) +
      ggplot2::theme_bw()
    
    p
    ggplot2::ggsave(filename = paste0(figure.dir, '/eco_ind_', var.names[i], '.png'), plot = p, width = 12, height = 8, dpi = 300)
  }

  ggplot2::annotate('polygon',x = c(scale.min.thresh,scale.max.thresh,scale.max.thresh,scale.min.thresh),
                    y = c(-Inf,-Inf,Inf,Inf),fill = 'lightblue', alpha = 0.5)
  ggplot2::annotate('text',x = scale.max.thresh+ 0.01, y = max(this.ind$state.value),hjust =0, label = 'EOF Threshold Range', color = 'black') 
    
  ggplot2::ggplot() +
    #added shaded region from min to max threshold
    ggplot2::annotate('polygon', x = c(scale.min.thresh, scale.max.thresh, scale.max.thresh, scale.min.thresh),
                      y = c(-Inf, -Inf, Inf, Inf),
                      fill = 'lightblue', alpha = 0.5) +
    ggplot2::geom_line(data =eco.ind, ggplot2::aes(x = catch.scalar, y = state.value, color = run)) +
    ggplot2::geom_point(data =eco.ind, ggplot2::aes(x = catch.scalar, y = state.value, color = run)) +
    #add text right of shaded region 
    # ggplot2::annotate('text', x = scale.max.thresh + 0.01, y = Inf, hjust = 0, vjust = 2.5,
    #                   label = 'EOF Threshold Range', color = 'black') +
    ggplot2::facet_wrap(~Variable, scale = 'free_y')+
    ggplot2::ylab('Value') +
    ggplot2::xlab('Catch Scalar') +
    ggplot2::ggtitle(paste0('Eco Indicators')) +
    ggplot2::theme_bw()
  
  ggplot2::ggsave(filename = paste0(figure.dir, '/eco_indicators_all.png'), width = 12, height =10, dpi = 300)
    
}
