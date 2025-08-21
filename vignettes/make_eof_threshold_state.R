#Script to calculate the distance from the desired model state based on the EOF runs
library(dplyr)
library(ggplot2)
library(patchwork)

# figure.dir = here::here('figures')

#Get baseline ecosystem states
# base.state = readRDS(here::here('data-raw','base_eco_state.rds')) |> 
#   dplyr::mutate(desired.min.scaled = ifelse(!is.finite(desired.min.scaled),-Inf,desired.min.scaled),
#                 desired.max.scaled = ifelse(!is.finite(desired.max.scaled),Inf,desired.max.scaled))
pristine.state = readRDS(here::here('data-raw','pristine_eco_state.rds')) |> 
  dplyr::mutate(desired.min.scaled = ifelse(!is.finite(desired.min.scaled),0,desired.min.scaled),
                desired.max.scaled = ifelse(!is.finite(desired.max.scaled),0,desired.max.scaled))

# desired.lower.base = base.state$desired.min.scaled
# desired.upper.base = base.state$desired.max.scaled

desired.lower.pristine= pristine.state$desired.min.scaled
desired.upper.pristine = pristine.state$desired.max.scaled

ind.names = base.state$Variable
#Set run  set directory
# data.dir = 'D:/catch_thresholds_eof_2/'
# out.dir = 'D:/catch_thresholds_eof_2/output/'
# run.names = list.files(data.dir,pattern = 'catch_thresholds_eof_2*',full.names = F, recursive = FALSE, include.dirs = T)
# run.id = gsub('catch_thresholds_eof_2_', '', run.names) |> as.numeric()
# 
# setup.file = read.csv(paste0(out.dir,'catch_thresholds_eof_2_setup.csv'))



# #Get link thresholds
ref.threshold = atlantiseof::est_link_threshold(param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/',
                                   atl.dir = 'C:/Users/joseph.caracappa/Documents/Data/master_06162025/',
                                   dietSource = 'detdiet',
                                   TL = NA,
                                   TE = c(0.1,0.15),
                                   alpha = c(0.15,0.2),
                                   year = c(2000,2010))
ref.threshold.range = range(ref.threshold$threshold)
ref.threshold.mean = mean(ref.threshold$t)

# out.df = data.frame(run.name = run.names, rel.base.catch = NA, rel.threshold = NA,  distance.base = NA, distance.pristine = NA)

i=1

# run.ind.base.ls = list()
run.ind.pristine.ls = list()
# run.closest.ls = list()
# run.distance.df = data.frame(run.name = run.names, run = run.id, 
#                              catch.tot = NA,
#                               rel.base.catch = NA, rel.threshold = NA, 
#                               distance.base = NA, distance.pristine = NA,
#                               stringsAsFactors = FALSE) |> 
#   dplyr::left_join(setup.file)
# for(i in 1:length(run.names)){
#   
#   #strip run id from run name
#   
#   this.run.ind =  readRDS(paste0(data.dir,run.names[i],'/eco_indicators_mean.rds')) |> 
#     dplyr::mutate(run.name = run.names[i],
#                   run = run.id[i]) |> 
#     dplyr::left_join(setup.file, by = 'run') |> 
#     dplyr::select(run.name,run, everything())
#   # run.ind.ls[[i]] = this.run.ind
#   
#   #calcuate distance from base
#   
#   this.run.base = this.run.ind %>%
#     tidyr::gather('Variable','state.value', -run,-run.name,-catch.force, -catch.threshold, -catch.scalar)%>%
#     dplyr::left_join(base.state)%>%
#     dplyr::mutate(state.value.scaled = state.value / mean.value,
#                   state.value.scaled = ifelse(!is.finite(state.value.scaled),0,state.value.scaled))
#   run.ind.base.ls[[i]] = this.run.base
#   
#   dist.base = atlantiseof::make_state_distance_rect(desired.upper = desired.upper.base,
#                                                                  desired.lower = desired.lower.base,
#                                                                  observed.state =  this.run.base$state.value.scaled)
#   
#   
#   
#   #calculate distance from pristine
#   this.run.pristine = this.run.ind %>%
#     tidyr::gather('Variable','state.value', -run,-run.name,-catch.force, -catch.threshold, -catch.scalar)%>%
#     dplyr::left_join(pristine.state)%>%
#     dplyr::mutate(state.value.scaled = state.value / mean.value,
#                   state.value.scaled = ifelse(!is.finite(state.value.scaled),0,state.value.scaled))
#   run.ind.pristine.ls[[i]] = this.run.pristine
#   
#   dist.pristine = atlantiseof::make_state_distance_rect(desired.upper = desired.upper.pristine,
#                                                                  desired.lower = desired.lower.pristine,
#                                                                  observed.state =  this.run.base$state.value.scaled)
#   
#   #calc run statistics
#   run.distance.df$catch.tot[i] = this.run.ind$catch.tot[1]
#   run.distance.df$rel.threshold[i] = this.run.ind$catch.tot[1]/ref.threshold.mean
#   run.distance.df$distance.base[i] = dist.base$distance
#   run.distance.df$distance.pristine[i] = dist.pristine$distance
#   run.distance.df$rel.base.catch[i] = this.run.ind$catch.tot[1]/base.state$mean.value[which(base.state$Variable == 'catch.tot')]
#   
#   #Save closest point
#   run.closest.ls[[i]] = data.frame(run.name = run.names[i], run = run.id[i],Variable = ind.names, closest.base = dist.base$closest_point, closest.pristine = dist.pristine$closest_point)
#   
# }

#Calculate incremental distance.base for each catch.scalar
# run.distance.df = run.distance.df %>%
#   dplyr::mutate(catch.scalar.rel = catch.scalar / max(catch.scalar, na.rm = TRUE)) %>%
#   dplyr::arrange(catch.scalar.rel) %>%
#   dplyr::mutate(incremental.distance.base = distance.base - dplyr::lag(distance.base, default = first(distance.base)),
#                 incremental.distance.pristine = distance.pristine - dplyr::lag(distance.pristine, default = first(distance.pristine)))


# saveRDS(run.distance.df, file = paste0(out.dir,'catch_thresholds_eof_2_distance.rds'))

# run.ind.base = dplyr::bind_rows(run.ind.base.ls) 
# saveRDS(run.ind.base, file = paste0(out.dir,'run_ind_base.rds'))
run.ind.pristine = dplyr::bind_rows(run.ind.pristine.ls)
saveRDS(run.ind.pristine, file = paste0(out.dir,'run_ind_pristine.rds'))
# run.closest = dplyr::bind_rows(run.closest.ls)
# saveRDS(run.closest, file = paste0(out.dir,'run_closest.rds'))

scale.close.thresh =run.distance.df$catch.scalar[which.min(abs(run.distance.df$catch.threshold-ref.threshold.mean))]
#plot rel to base run
# b1 = ggplot(run.distance.df, aes(x = rel.threshold, y = distance.base))+
#   geom_line()+
#   geom_point()+
#   ylab('Distance from Baseline Model State')+
#   xlab('Catch Relative to EOF Threshold')+
#   ggtitle('Base Run')+
#   geom_vline(xintercept = 1)

# b2 =ggplot(run.distance.df, aes(x = catch.scalar, y = distance.base))+
#   geom_line()+
#   geom_point()+
#   ylab('Distance from Baseline Model State')+
#   xlab('Catch Relative to Base Model')+
#   ggtitle('Base Run')+
#   geom_vline(xintercept = 1)

# b3= ggplot(run.distance.df, aes(x = rel.base.catch, y = distance.base))+
#   geom_line()+
#   geom_point()+
#   ylab('Distance from Baseline Model State')+
#   xlab('Catch Relative to Desired Catch')+
#   ggtitle('Base Run')+
#   geom_vline(xintercept = 1)

#Plot incremental distance.base
# b4 = ggplot(run.distance.df, aes(x = catch.scalar, y = incremental.distance.base))+
#   geom_line()+
#   geom_point()+
#   ylab('Incremental Distance from Baseline Model State')+
#   xlab('Catch Relative to Base Model')+
#   ggtitle('Base Run')+
#   geom_vline(xintercept = c(1,scale.close.thresh ),lty =c(1,2))

#plot rel to pristine run
p1 = ggplot(run.distance.df, aes(x = rel.threshold, y = distance.pristine))+
  geom_line()+
  geom_point()+
  ylab('Distance from Pristine Model State')+
  xlab('Catch Relative to EOF Threshold')+
  ggtitle('Pristine Run')+
  geom_vline(xintercept = 1)


p2 = ggplot(run.distance.df, aes(x = catch.scalar, y = distance.pristine))+
  geom_line()+
  geom_point()+
  ylab('Distance from Pristine Model State')+
  xlab('Catch Relative to Base Model')+
  ggtitle('Pristine Run')+
  geom_vline(xintercept = 1)

p3=ggplot(run.distance.df, aes(x = rel.base.catch, y = distance.pristine))+
  geom_line()+
  geom_point()+
  ylab('Distance from Pristine Model State')+
  xlab('Catch Relative to Desired Catch')+
  ggtitle('Pristine Run')+
  geom_vline(xintercept = 1)

#Plot incremental distance.base
p4 = ggplot(run.distance.df, aes(x = catch.scalar, y = incremental.distance.pristine))+
  geom_line()+
  geom_point()+
  ylab('Incremental Distance from Pristine Model State')+
  xlab('Catch Relative to Base Model')+
  ggtitle('Base Run')+
  geom_vline(xintercept = c(1,scale.close.thresh ),lty =c(1,2))

#Save plot combinations
b1 / p1
ggsave(filename = paste0(figure.dir,'/threshold_scalar_distance_base_pristine.png'),width = 8, height = 10, dpi = 300)

b2 / p2
ggsave(filename = paste0(figure.dir,'/catch_scalar_distance_base_pristine.png'),width = 8, height = 10, dpi = 300)

b3 / p3
ggsave(filename = paste0(figure.dir,'/cath_target_base_pristine.png'),width = 8, height = 10, dpi = 300)

b4/p4
ggsave(filename = paste0(figure.dir,'/incremental_distance_base_pristine.png'),width = 8, height = 10, dpi = 300)


