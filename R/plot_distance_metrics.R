# Plots distance metrics relative to reference run
#'
#'@param figure.dir Character String. Path to figure directory
#'@param distance.file Character String. Path to distance file
#'@param thresholds.file Character String. Path to thresholds file
#'@param ref.state.file Character String. Path to reference file
#'
#'
#'@return dataframe of eco indicators by year
#'
#'@export
#'

plot_distance_metrics <- function(figure.dir,distance.file,thresholds.file,ref.state.file) {
  
  if(!dir.exists(figure.dir)) {
    dir.create(figure.dir, recursive = TRUE)
  }
  
  #Read in reference state
  ref.state = readRDS(ref.state.file)
  catch.tot.ref = ref.state$mean.value[which(ref.state$Variable == 'catch.tot')]
  
  #Read in distance ouref.state.file#Read in distance output data
  run.distance.df = readRDS(distance.file) %>%
    dplyr::mutate(catch.tot.rel = catch.tot/catch.tot.ref)
  run.thresholds.df = readRDS(thresholds.file)
  
  ref.threshold.min = min(run.thresholds.df$threshold,na.rm=T)
  ref.threshold.max = max(run.thresholds.df$threshold,na.rm=T)
  
  #identify the catch scalar closest to the mean link threshold
  scale.min.thresh =run.distance.df$catch.scalar[which.min(abs(run.distance.df$catch.threshold-ref.threshold.min))]
  scale.max.thresh =run.distance.df$catch.scalar[which.min(abs(run.distance.df$catch.threshold-ref.threshold.max))]
  
  #plot1: Distance as a function of scalars of ref.threshold.mean
  ggplot2::ggplot(run.distance.df, ggplot2::aes(x = rel.threshold, y = distance.ref, color = run))+
    ggplot2::geom_line(ggplot2::aes(color = run))+
    ggplot2::scale_color_viridis_c()+
    ggplot2::geom_point()+
    ggplot2::ylab('Distance from Reference Model State')+
    ggplot2::xlab('Catch Relative to EOF Threshold')+
    ggplot2::ggtitle('Base Run')+
    ggplot2::geom_vline(xintercept = 1,lty =2)+
    ggplot2::theme_bw()
  ggsave(filename = paste0(figure.dir,'/state_distance_rel_threshold.png'),width = 12, height = 8, dpi = 300)
  
  #plot2: Distance as a function of catch forcing scalar
  ggplot2::ggplot(run.distance.df, ggplot2::aes(x = catch.scalar, y = distance.ref))+
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    ggplot2::annotate('polygon',x = c(scale.min.thresh,scale.max.thresh,scale.max.thresh,scale.min.thresh),
                     y = c(0,0,Inf,Inf),
                     fill = 'lightblue', alpha = 0.5)+
    ggplot2::annotate('text',x = scale.max.thresh+ 0.01, y = max(run.distance.df$distance.ref),hjust =0, label = 'EOF Threshold Range', color = 'black')+
    ggplot2::ylab('Distance from Reference Model State')+
    ggplot2::xlab('Catch Relative to Reference Model')+
    ggplot2::geom_vline(xintercept = c(1),lty =c(1))+
    ggplot2::theme_bw()
  ggplot2::ggsave(filename = paste0(figure.dir,'/state_distance_rel_catch_scalar.png'),width = 12, height = 8, dpi = 300)
  
  #plot3: Distance as a function of catch forcing scalar and total catch (realized)
  
  png(filename = paste0(figure.dir,'/state_distance_rel_catch_scalar_3d.png'),
      width = 10, height = 10, res = 300,units = 'in', pointsize = 12, bg = 'white')
  plot3D::scatter3D(x = run.distance.df$catch.tot.rel, y = run.distance.df$catch.scalar, z = run.distance.df$distance.ref,
            type = "b", # "l" for line, "p" for points, "b" for both
            colvar = run.distance.df$run, # Color by run,
            col = NULL,
            xlab = "realized catch",
            ylab = 'catch scalar',
            zlab = "distance",
            main = "",
            phi = 25,    # Elevation angle
            theta = 50,  # Azimuth angle
            ticktype = "detailed", # Show detailed ticks on axes
            pch = 19, # Point character (if using points)
            cex = 0.8, # Point size (if using points)
            clab = c('Run')
  )
  dev.off()
  
  #Plot 4: Total catch relative to reference model as a function of catch forcing scalar
  ggplot(run.distance.df, ggplot2::aes(x = catch.scalar, y = catch.tot.rel, color = run))+
    ggplot2::geom_line(ggplot2::aes(color = run))+
    ggplot2::scale_color_viridis_c()+
    ggplot2::geom_point()+
    ggplot2::ylab('Total Catch Relative to Reference Model')+
    ggplot2::xlab('Catch Forcing Scalar')+
    ggplot2::theme_bw()
  ggplot2::ggsave(filename = paste0(figure.dir,'/catch_scalar_realized.png'),width = 12, height = 8, dpi = 300)
    
  #plot5: Distance as a function of scalars of base run total catch (realized)
  ggplot2::ggplot(run.distance.df, ggplot2::aes(x = rel.ref.catch, y = distance.ref, color = run))+
    ggplot2::geom_line(ggplot2::aes(color = run))+
    ggplot2::scale_color_viridis_c()+
    ggplot2::geom_point()+
    ggplot2::ylab('Distance from Reference Model State')+
    ggplot2::xlab('Catch Relative to Desired Catch')+
    ggplot2::geom_vline(xintercept = 1)+
    ggplot2::theme_bw()
  ggplot2::ggsave(filename = paste0(figure.dir,'/state_distance_rel_ref_catch.png'),width = 12, height = 8, dpi = 300)
  
  #plot6: First difference of distance as a function of catch scalar
  ggplot2::ggplot(run.distance.df, ggplot2::aes(x = catch.scalar, y = incremental.distance.ref, color = run))+
    ggplot2::geom_line(ggplot2::aes(color = run))+
    ggplot2::scale_color_viridis_c()+
    ggplot2::geom_point()+
    ggplot2::ylab('Incremental Distance from Reference Model State')+
    ggplot2::xlab('Catch Forcing Scalar')+
    ggplot2::geom_vline(xintercept = c(1,scale.close.thresh ),lty =c(1,2))+
    ggplot2::theme_bw()
  ggplot2::ggsave(filename = paste0(figure.dir,'/state_distance_incremental.png'),width = 12, height = 8, dpi = 300)
  
}