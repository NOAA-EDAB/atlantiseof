
param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
atl.dir = 'C:/Users/joseph.caracappa/Documents/Data/base_run_eof/'
group.index = here::here('data-raw','neus_species_index.csv')
fgs.file  = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_groups.csv'
dietSource = 'detdiet'
timeRange = 30:52
out.name = 'ref_'
desired.p = c(0.025,0.975)

figure.dir = here::here('figures')
ref.state.file = here::here('data-raw','ref_eco_state.rds')
data.dir = 'D:/catch_thresholds_eof_3/'
out.dir = 'D:/catch_thresholds_eof_3/output/'
run.prefix = 'catch_thresholds_eof_3'
setup.file = read.csv('D:/catch_thresholds_eof_3/output/catch_thresholds_eof_3_setup.csv')
dietSource = 'detdiet'

ref.thresh =est_link_threshold(param.dir = param.dir,
                   atl.dir = atl.dir,
                   dietSource = 'detdiet',
                   TL = NA,
                   TE = c(0.1,0.15),
                   alpha = c(0.15,0.2),
                   year = c(2000,2010))
range(ref.thresh$threshold, na.rm = TRUE)

atlantiseof::make_reference_state(param.dir,atl.dir,group.index,fgs.file,dietSource,20:60,out.name,desired.p)
atlantiseof::make_desired_state_distance(param.dir,atl.dir,dietSource,ref.state.file,data.dir,out.dir,run.prefix,setup.file)
atlantiseof::plot_distance_metrics(figure.dir = here::here('figures','eof_thresholds_3',''),
                                   distance.file = 'D:/catch_thresholds_eof_3/output/catch_thresholds_eof_3_distance.rds',
                                   thresholds.file = 'D:/catch_thresholds_eof_3/output/catch_thresholds_eof_3_eof_threshold.rds',
                                   ref.state.file = here::here('data-raw','ref_eco_state.rds'))
atlantiseof::plot_ind_pca(data.dir = 'D:/catch_thresholds_eof_3/output/',
                          ref.ind.file = paste0(data.dir,'output/catch_thresholds_eof_3_run_eco_ind.rds'),
                          ref.state.file = here::here('data-raw','ref_eco_state.rds'),
                          figure.dir =  here::here('figures','eof_thresholds_3',''))

readRDS(file.path(out.dir, 'catch_thresholds_eof_3_eof_threshold.rds'))
pca =readRDS(file.path(out.dir, 'pca_result.rds'))
