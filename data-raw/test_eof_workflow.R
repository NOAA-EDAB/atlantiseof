
param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
atl.dir = 'C:/Users/joseph.caracappa/Documents/Data/base_run_eof/'
ref.run.dir = atl.dir
group.index = here::here('data-raw','neus_species_index.csv')
fgs.file  = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_groups.csv'
dietSource = 'detdiet'
timeRange = 30:52
out.name = 'ref_'
desired.p = c(0.025,0.975)
start.year = 1964

figure.dir = here::here('figures')
ref.state.file = here::here('data-raw','ref_eco_state.rds')
data.dir = 'D:/data/catch_thresholds_eof_3/'
out.dir = 'D:/data/catch_thresholds_eof_3/output/'
run.prefix = 'catch_thresholds_eof_3'
setup.file = read.csv('D:/data/catch_thresholds_eof_3/catch_thresholds_eof_3_setup.csv')
dietSource = 'detdiet'
survdat.data = readRDS(here::here('data-raw','survey_lenagewgt.rds'))

ref.thresh =atlantiseof::est_link_threshold(param.dir = param.dir,
                   atl.dir = atl.dir,
                   dietSource = 'detdiet',
                   TL = NA,
                   TE = c(0.1,0.15),
                   alpha = c(0.15,0.2),
                   year = c(2000,2010))
range(ref.thresh$threshold, na.rm = TRUE)

atlantiseof::make_reference_state(param.dir,atl.dir,group.index,fgs.file,dietSource,20:60,out.name,desired.p)
atlantiseof::make_desired_state_distance(param.dir,atl.dir,dietSource,ref.state.file,data.dir,out.dir,run.prefix,setup.file)
atlantiseof::plot_distance_metrics(figure.dir = 'D:/data/catch_thresholds_eof_3/output',
                                   distance.file = 'D:/data/catch_thresholds_eof_3/output/catch_thresholds_eof_3_distance.rds',
                                   thresholds.file = 'D:/data/catch_thresholds_eof_3/output/catch_thresholds_eof_3_eof_threshold.rds',
                                   ref.state.file = here::here('data-raw','ref_eco_state.rds'))
data.dir = 'D:/data/catch_thresholds_eof_3/output/'
atlantiseof::plot_ind_pca(data.dir = 'D:/data/catch_thresholds_eof_3/output/',
                          ref.ind.file = paste0(data.dir,'catch_thresholds_eof_3_run_eco_ind.rds'),
                          ref.state.file = here::here('data-raw','ref_eco_state.rds'),
                          figure.dir =  here::here('figures','eof_thresholds_3',''))

readRDS(file.path(out.dir, 'catch_thresholds_eof_3_eof_threshold.rds'))
pca =readRDS(file.path(out.dir, 'pca_result.rds'))

foodweb = atlantiseof::calc_foodweb(atl.dir = atl.dir,
                                    param.dir = param.dir,
                                    dietSource = dietSource,
                                    fgs.file = fgs.file,
                                    timeRange = timeRange,
                                    bySpecies = T,
                                    show.plot = F) 
foodweb.mean = foodweb |>
  dplyr::filter(time >= (20*365)) |> 
  dplyr::group_by(species) |> 
  dplyr::summarise(relative_total_impact = mean(relative_total_impact,na.rm=T),
                   keystone_idx1 = mean(keystone_idx1,na.rm=T),
                   keystone_idx2 = mean(keystone_idx2,na.rm=T),
                   keystone_idx3 = mean(keystone_idx3,na.rm=T)) |> 
  dplyr::mutate(relative_total_impact_scaled = relative_total_impact/max(relative_total_impact))
write.csv(foodweb.mean,here::here('data-raw','ref_foodweb_keystone_summary.csv'),row.names=F)
  
