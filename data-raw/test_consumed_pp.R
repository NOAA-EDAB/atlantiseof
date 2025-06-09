grazer.con =atlantiseof::get_ppc(param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/',
        atl.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/Atlantis_Runs/Dev_11032022/',
        fgs = paste0(param.dir,'neus_groups.csv'),
        dietSource = 'prm',
        timeRange = 30:52)

pp =get_pp(pathToForcing = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/tsfiles/Annual_Files/',
       bgm = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/neus_tmerc_RM2.bgm')

phyto.bio = pp$dailyspecies %>%
  dplyr::mutate(value.C = value * 5.7) %>%
  dplyr::group_by(year,variable)%>%
  dplyr::summarise(value.C = sum(value.C))%>%
  tidyr::separate(variable,c('prey','d'),sep = '_')

phyto.con = grazer.con %>%
  dplyr::group_by(year,prey)%>%
  dplyr::summarise(consumed = sum(consumption.mTCyr))%>%
  dplyr::mutate(year = year + 1964)
  
prop.con = phyto.con %>%
  dplyr::left_join(phyto.bio)%>%
  dplyr::mutate(consumed.prop = consumed/value.C)


library(ggplot2)

ggplot(prop.con, aes(x= year, y = consumed.prop))+
  geom_line()+
  facet_wrap(~prey,scale = 'free_y')

write.csv(prop.con,here::here('data-raw','consumed_pp_pct.csv'),row.names =F)
