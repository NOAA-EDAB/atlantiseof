# Script to generate a range of ecosystem overfishing thresholds based on parameter ranges and primary production forcing
library(ggplot2)

#Define potential alpha ranges
alpha.v = seq(0.15,0.2,0.01)
#Define potential TE ranges
TE.v = seq(0.1,0.16,0.01)
#Define trophic level ranges
# TL.v = seq(2,3.5,0.1)

eof.range = atlantiseof::est_link_threshold(atl.dir = 'C:/Users/joseph.caracappa/Documents/Data/master_06162025/',
                                param.dir = 'C:/Users/joseph.caracappa/Documents/GitHub/neus-atlantis/currentVersion/',
                                dietSource = 'detdiet',
                                TL = NA,
                                TE = TE.v,
                                alpha = alpha.v,
                                year = 2018
                                )
thresh.mat = matrix(eof.range$threshold, nrow = length(alpha.v), ncol = length(TE.v), byrow = T)
# TL.mat = matrix(eof.range$TL, nrow = length(alpha.v), ncol = length(TE.v), byrow = T)

# thresh.mat = eof.range %>%
#   tidyr::pivot_wider(names_from = TE, values_from = threshold) %>%
#   dplyr::select(-alpha, -TL, -year) %>%
#   as.matrix()

plotly::plot_ly(eof.range,
  x = ~alpha.v, y = ~TE.v, z = ~thresh.mat,
  surfacecolor = ~thresh.mat,  # Use fourth variable for color
  type = "surface"
)

ggplot(eof.range,aes(x = alpha, y = threshold, color = factor(TE)))+
  geom_line()

hist(thresh.mat)  

min(thresh.mat)
max(thresh.mat)
             