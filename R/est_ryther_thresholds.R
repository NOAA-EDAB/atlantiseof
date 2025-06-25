#' Replicate Link and Watson study using NEUS region
#'
#' Ryther index only
#'
#'
#'
#'


# use realized catch from dev run
est_link_thresholds <- function() {

## Link method with regional PP estimate
linkthresholds <- c(0.3,1.1,3,5)

temp <- base::tempfile()
download.file(url="https://raw.githubusercontent.com/NOAA-EDAB/neus-atlantis/master/currentVersion/neus_tmerc_RM2.bgm",destfile=temp, quiet=TRUE)
ppdata <- get_pp(bgm = temp,pathToForcing = here::here("data-raw/data"))

# estimate average PP tons per year with bounds
mab <- ppdata$dailyspeciesbox %>%
  dplyr::left_join(.,NEFSCspatial::Neus_atlantis %>% sf::st_as_sf(),by = c("box"="BOX_ID")) %>%
  dplyr::group_by(year,epu,variable) %>%
  dplyr::summarise(N = sum(value),.groups="drop") %>%
  dplyr::mutate(C = 5.7*N) %>%
  dplyr::filter(epu == "MAB",
                variable == "Diatom_N",
                year >=1998)

area <- ppdata$Areas %>%
  dplyr::filter(epu == "MAB") %>%
  dplyr::pull(area)


neus <- ppdata$dailyspeciesbox %>%
  dplyr::left_join(.,NEFSCspatial::Neus_atlantis %>% sf::st_as_sf(),by = c("box"="BOX_ID")) %>%
  dplyr::group_by(year,variable) %>%
  dplyr::summarise(N = sum(value),.groups="drop") %>%
  dplyr::mutate(C = 5.7*N) %>%
  dplyr::filter(variable == "Diatom_N",
                year >=1998)

neusarea <- ppdata$Areas %>%
  dplyr::pull(area) %>%
  sum()

# MAB PP
PP <- c(min(mab$C),mean(mab$C),max(mab$C)) * 10 #(carbon to wet weight)

# Threshold = .276 (PP = 3.1, alpha = 0.1, TE = .1, TL = 3.6)
# Threshold = 1.14 (PP = 3.6, alpha = 0.15, TE = .12, TL = 3.4)
# Threshold = 2.73 (PP = 4, alpha = 0.15, TE = .14, TL = 3.2)
# Threshold = 3.64 (PP = 4, alpha = 0.20, TE = .14, TL = 3.2)

# calculate threshold like Jason
  catchLink1 = .1*PP[1]*(.1^(3.6-1))/area
  catchLink2 = .15*PP[2]*(.12^(3.4-1))/area
  catchLink3 = .15*PP[3]*(.14^(3.2-1))/area
  catchLink4 = .20*PP[3]*(.14^(3.2-1))/area
  print(catchLink1)
  print(catchLink2)
  print(catchLink3)
  print(catchLink4)















PP <- c(min(neus$C),mean(neus$C),max(neus$C)) *10
PP <- c(400,450,500)*10^9
TE <- c(.10,.12,.14)
alpha <- c(.10, .15,.20)
TL <- c(3.2,3.4,3.6)
area <- 363*10^6

len <- length(PP)*length(TE)*length(alpha)*length(TL)

i <- 0
catch <- vector(mode="numeric",length=len)
d <- NULL
for (rPP in PP) {
  for (rTE in TE) {
    for (ralpha in alpha) {
      for (rTL in TL) {
        i <- i + 1
        catch[i] = ralpha*rPP*(rTE^(rTL-1))/area
        d <- rbind(d,c(rPP,rTE,ralpha,rTL,catch[i]))
      }
    }

  }
}
catch
range(catch)


PP <- c(450)*10^9
TE <- c(.12)
alpha <- c(.15)
TL <- c(3.4)
area <- 363*10^6


rPP <- 450 * 10^9#Gt wet weight
catch <- .42*10^9
area <- 363*10^6

for (rPP in PP) {
  catchLink = .15*rPP*(.12^(3.4-1))/area
  print(catchLink)
}
catch/area
## Link method with regional TL bounds (if different)



## Link method with species level TL and TE


}

