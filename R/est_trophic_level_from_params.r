#' Estimate Trophic level from diet matrix
#'
#' uses PPrey values to determine Trophic level
#'
#' Need to process detailed diet data frirst
#' linux command line
#' # zip up file
#'   gzip -k neus_outputDetailedDietCheck.txt
#'   # then remove zeros and save ans another zip
#'   zcat neus_outputDetailedDietCheck.txt.gz | awk 'NR > 1{s=0; for (i=6;i<=NF;i++) s+=$i; if (s!=0)print}' | gzip > DetDiet.gz
#'   # strip headers from original file
#'   zcat neus_outputDetailedDietCheck.txt.gz | head -n1 | gzip > DetDietHead.gz
#'   # concatenate them
#'   cat DetDietHead.gz DetDiet.gz > DetDiet2.gz
#'
#'@param prm_biol Character String. Path to biology prm file (Atlantis)
#'@param fgs Character String. Name of groups.csv file
#'@param plottl Boolean. Plot the Trophic level to window. Default = F
#'
#'@return list
#'\item{trophiclevel}{dataframe. Species, year, trophic level}
#'\item{figuretl}{ggplot2 object. trophic level bar plot by species}
#'
#'@export


est_trophic_level_from_params <- function(prm_biol = "C:/Users/andrew.beet/Documents/myWork/githubRepos/neus-atlantis/currentVersion/at_biology.prm",
                                          fgs = "C:/Users/andrew.beet/Documents/myWork/githubRepos/neus-atlantis/currentVersion/neus_groups.csv",
                                          plottl=F) {

  speciesnames <- atlantistools::load_fgs(fgs) %>%
    dplyr::select(Code,Name)

  pprey <- atlantistools::load_dietmatrix(prm_biol = prm_biol,
                                          fgs = fgs) %>%
    dplyr::mutate(prey = dplyr::case_when(grepl("sed",prey) ~ substr(prey,1,2),
                                          TRUE ~ prey)) %>%
    dplyr::left_join(.,speciesnames,by = c("pred"="Code")) %>%
    dplyr::rename(predName = "Name") %>%
    dplyr::left_join(.,speciesnames,by = c("prey"="Code")) %>%
    dplyr::rename(preyName = "Name") %>%
    dplyr::select(-pred,-prey) %>%
    dplyr::rename(pred = "predName",prey = "preyName") %>%

    tibble::as_tibble()

  # sums up pprey values over pred,prey stanzas
  consump <- pprey %>%
    dplyr::group_by(pred,prey) %>%
    dplyr::summarise(consumption = sum(avail),
                     .groups = "drop")

  # total pprey values for predator
  predtotalconsump <- consump %>%
    dplyr::group_by(pred) %>%
    dplyr::summarise(totconsumption = sum(consumption),
                     .groups = "drop")

  # normalize the pprey vector by predator sum (over stanzas)
  fji <- dplyr::left_join(consump,predtotalconsump,by = "pred") %>%
    dplyr::mutate(fji = consumption/(totconsumption)) %>%
    dplyr::select(pred,prey, fji)



  allspecies <- union(unique(fji$prey),unique(fji$pred))
  preyspecies <- setdiff(unique(fji$prey),unique(fji$pred))

  added <- data.frame(pred = preyspecies,prey = "Diatom",fji = 0)

  fji <- rbind(fji,added)


  # create matrix. predator in rows, prey in rows. fij in cell
  tt <- tidyr::pivot_wider(fji,names_from = prey,values_from = fji)
  # reorder to form a square matrix
  newtt <- tt %>% dplyr::relocate(c("pred",dplyr::pull(tt,pred))) %>%
    tibble::column_to_rownames(var="pred")

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
