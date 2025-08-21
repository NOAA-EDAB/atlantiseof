#Creat eco indicators for reference run and pristine run, generate desired state, and estimate distance from state
param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/'
test.run.dir = 'C:/Users/joseph.caracappa/Documents/Data/base_run_eof/'
pristine.run.dir = 'C:/Users/joseph.caracappa/Documents/Data/master_nofishing_06162025/'
group.index = here::here('data-raw','neus_species_index.csv')
fgs.file  = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_groups.csv'
dietSource = 'detdiet'
timeRange = 1:5
library(dplyr)

#Get status for pristine run
eco.pristine = atlantiseof::make_eco_indicators(
  param.dir = param.dir,
  atl.dir = pristine.run.dir,
  group.index = group.index,
  fgs.file = fgs.file,
  dietSource = dietSource,
  timeRange = timeRange
)

eco.pristine.year = atlantiseof::make_eco_indicators_time(
  param.dir = param.dir,
  atl.dir = pristine.run.dir,
  group.index = group.index,
  fgs.file = fgs.file,
  dietSource = dietSource,
  timeRange = timeRange
)

eco.pristine.obs = atlantiseof::make_run_state(param.dir = param.dir,
                                            atl.dir = pristine.run.dir,
                                            group.index = group.index,
                                            fgs.file  =fgs.file,
                                            dietSource = dietSource,
                                            timeRange = timeRange)
saveRDS(eco.pristine.obs,here::here('data-raw','pristine_eco_obs.rds'))

#Get status for test run
# eco.test = atlantiseof::make_eco_indicators(
#   param.dir = param.dir,
#   atl.dir =  'C:/Users/joseph.caracappa/Documents/Data/base_run_eof/',
#   group.index = group.index,
#   fgs.file = fgs.file,
#   dietSource = dietSource,
#   timeRange = timeRange
# )

#Set Desired state from pristine run
# eco.test.year = atlantiseof::make_eco_indicators_time(
#   param.dir = param.dir,
#   atl.dir = test.run.dir,
#   group.index = group.index,
#   fgs.file = fgs.file,
#   dietSource = dietSource,
#   timeRange = timeRange
# )

# eco.test.obs = atlantiseof::make_run_state(param.dir = param.dir,
#                                             atl.dir = test.run.dir,
#                                             group.index = group.index,
#                                             fgs.file  =fgs.file,
#                                             dietSource = dietSource,
#                                             timeRange = timeRange)
# saveRDS(eco.test.obs, here::here('data-raw','base_eco_obs.rds'))

# summary.base.state = eco.test.year %>%
#   # dplyr::mutate(dplyr::across(bio.tot:mean.tl.bio, ~ scale(.) %>% as.vector))%>%
#   tidyr::gather(Variable,Value,-year)%>%
#   dplyr::group_by(Variable)%>%
#   dplyr::summarise(mean.value = mean(Value[is.finite(Value)], na.rm = TRUE),
#                    median.value = median(Value[is.finite(Value)], na.rm = TRUE),
#                    sd.value = sd(Value[is.finite(Value)]),
#                    abs.min.value = min(Value[is.finite(Value)], na.rm = TRUE),
#                    abs.max.value = max(Value[is.finite(Value)], na.rm = TRUE),
#                    desired.min = quantile(Value[is.finite(Value)], 0.25, na.rm = TRUE),
#                    desired.max = quantile(Value[is.finite(Value)], 0.75, na.rm = TRUE))%>%
#   dplyr::mutate(desired.min.scaled = (desired.min-abs.min.value)/(abs.max.value - abs.min.value),
#                 desired.max.scaled = (desired.max-abs.min.value)/(abs.max.value - abs.min.value),
#                 desired.center = (desired.min.scaled + desired.max.scaled)/2,
#                 desired.range = desired.max.scaled - desired.min.scaled
#                 )

summary.pristine.state = eco.pristine.year %>%
  # dplyr::mutate(dplyr::across(bio.tot:mean.tl.bio, ~ scale(.) %>% as.vector))%>%
  tidyr::gather(Variable,Value,-year)%>%
  dplyr::mutate(Value = ifelse(!is.finite(Value),NA,Value))%>%
  dplyr::group_by(Variable)%>%
  dplyr::summarise(mean.value = mean(Value[is.finite(Value)], na.rm = TRUE),
                   median.value = median(Value[is.finite(Value)], na.rm = TRUE),
                   sd.value = sd(Value[is.finite(Value)]),
                   abs.min.value = min(Value[is.finite(Value)], na.rm = TRUE),
                   abs.max.value = max(Value[is.finite(Value)], na.rm = TRUE),
                   desired.min = quantile(Value[is.finite(Value)], 0.25, na.rm = TRUE),
                   desired.max = quantile(Value[is.finite(Value)], 0.75, na.rm = TRUE))%>%
  dplyr::mutate(desired.min.scaled = (desired.min-abs.min.value)/(abs.max.value - abs.min.value),
                desired.max.scaled = (desired.max-abs.min.value)/(abs.max.value - abs.min.value),
                desired.center = (desired.min.scaled + desired.max.scaled)/2,
                desired.range = desired.max.scaled - desired.min.scaled
  )

  
#replace nonfinite with NA
# summary.base.state[] <- lapply(summary.base.state, function(col) {
#   if (is.numeric(col)) col[!is.finite(col)] <- NA
#   col
# })
summary.pristine.state[] <- lapply(summary.pristine.state, function(col) {
  if (is.numeric(col)) col[!is.finite(col)] <- NA
  col
})

#Write state output
# saveRDS(summary.base.state, here::here('data-raw','base_eco_state.rds'))
saveRDS(summary.pristine.state, here::here('data-raw','pristine_eco_state.rds'))

#Ideal state defined by run output
ideal.state = summary.base.state$desired.range
ideal.center = (summary.base.state$desired.min.scaled + summary.base.state$desired.max.scaled) / 2
#Ideal state defined by other means



#Calculate state from pristine and test run
eco.pristine.state =  eco.pristine %>%
  tidyr::gather('Variable','state.value')%>%
  dplyr::left_join(summary.pristine.state)%>%
  dplyr::mutate(state.value.scaled = state.value / mean.value,
                state.value.scaled = ifelse(!is.finite(state.value.scaled),0,state.value.scaled))

# eco.test.state = eco.test %>%
#   tidyr::gather('Variable','state.value')%>%
#   dplyr::left_join(summary.base.state)%>%
#   dplyr::mutate(state.value.scaled = state.value / mean.value,
# state.value.scaled = ifelse(!is.finite(state.value.scaled),0,state.value.scaled))
# 
# ideal.center = summary.base.state$desired.center
# ideal.state = summary.base.state$desired.range

#Calculate distance from pristine state
eco.test.dist.ell = atlantiseof::make_state_distance_ellipse(ideal.state = ideal.state,
                                 ideal.center = ideal.center,
                                 observed.state = eco.test.state$state.value.scaled)

# eco.test.dist.ell.rect = atlantiseof::make_state_distance_rect(desired.upper = summary.state$desired.max.scaled,
#                                                                desired.lower = summary.state$desired.min.scaled,
#                                                                observed.state =  )


