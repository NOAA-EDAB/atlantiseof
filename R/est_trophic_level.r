#' testing load_detailed_diet from atlantisom
#'
#' Need to process detailed diet data frirst
#' linux command line
#' # zip up file
#'gzip -k neus_outputDetailedDietCheck.txt
#'then remove zeros and save ans another zip
#'zcat neus_outputDetailedDietCheck.txt.gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > DetDiet.gz
#'strip headers from original file
#'zcat neus_outputDetailedDietCheck.txt.gz | head -n1 | gzip > DetDietHead.gz
#'concatenate them
#'cat DetDietHead.gz DetDiet.gz > DetDiet2.gz
#'
#'@param param,dir Character String. Path to Parameter directory
#'@param atl.dir Character String. Path to output directory
#'@param fgs Character String. Name of groups.csv file
#'@param detDietfile Character String. Name of processed zipped detailed diet file
#'@param plottl Boolean. Plot the Trophic level to window. Default = F
#'
#'@return list
#'\item{trophiclevel}{dataframe. Species, year, trophic level}
#'\item{figuretl}{ggplot2 object. trophic level bar plot by species}
#'
#'@export


est_trophic_level <- function(param.dir = "C:/Users/andrew.beet/Documents/myWork/githubRepos/neus-atlantis/currentVersion",
                              atl.dir = here::here("other/dev_3a75e57d1"),
                              fgs = "neus_groups.csv",
                              detDietfile = "DetDiet2.gz",
                              plottl=F) {

  fgs <- atlantisom::load_fgs(dir = param.dir,file_fgs= fgs)
  #Total consumption reported. flagdietcheck = 1
  dietmat <- atlantisom::load_detailed_diet_comp(dir = atl.dir,
                                                 file_diet = detDietfile,
                                                 fgs = fgs) %>%
    dplyr::filter(time.days < 19711) # 2017

  # TL over space, time, all age classes
  # average consumption per time step is calculated
  consump <- dietmat %>%
    dplyr::group_by(species,prey) %>%
    dplyr::summarise(totalconsumption = sum(atoutput),
                     timesteps = unique(time.days) %>% length(),
                     consumption = totalconsumption/timesteps,
                     .groups = "drop") %>%
    dplyr::select(species,prey,consumption)


  predtotalconsump <- consump %>%
    dplyr::group_by(species) %>%
    dplyr::summarise(totconsumption = sum(consumption))

  fji <- dplyr::left_join(consump,predtotalconsump,by = "species") %>%
    dplyr::mutate(fji = consumption/(totconsumption)) %>%
    dplyr::select(species,prey, fji)

  allspecies <- union(unique(fji$prey),unique(fji$species))
  preyspecies <- setdiff(unique(fji$prey),unique(fji$species))

  added <- data.frame(species = preyspecies,prey = "Diatom",fji = 0)

  fji <- rbind(fji,added)


  # create matrix. predator in rows, prey in rows. fij in cell
  tt <- tidyr::pivot_wider(fji,names_from = prey,values_from = fji)
  # reorder to form a square matrix
  newtt <- tt %>% dplyr::relocate(c("species",dplyr::pull(tt,species))) %>%
    tibble::column_to_rownames(var="species")

  # replace NAs with zero
  newtt[is.na(newtt)] <- 0
  newtt <- as.matrix(newtt)

  #invert the matrix
  oneminusf <- diag(nrow(newtt)) - newtt
  TL <- solve(oneminusf,rep(1,nrow(tt)))

  p1 <- TL %>%
    as.data.frame() %>%
    tibble::rownames_to_column("species") %>%
    dplyr::rename(TrophicLevel = ".") %>%
    dplyr::arrange(TrophicLevel) %>%
    ggplot2::ggplot(.,ggplot2::aes(x=reorder(species,TrophicLevel),y=TrophicLevel)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, hjust=1)) +
    ggplot2::ylab("Trophic Level based on diet matrix") +
    ggplot2::xlab("") +
    #ggplot2::ggtitle("Mean consumption per timestep") +
    ggplot2::coord_flip()

  if(plottl) {
    print(p1)
  }

  return(list(trophiclevel = TL,figuretl = p1))


}
