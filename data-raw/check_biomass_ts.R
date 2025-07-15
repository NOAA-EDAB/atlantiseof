#plot biomass overlayed for each run

data.dir = 'D:/catch_thresholds_eof_3/'
run.dirs = list.files(pattern='catch',data.dir, full.names = TRUE)

data.ls = list()
for(i in 1:length(run.dirs)){
  
  run.dir = run.dirs[i]
  run.name = basename(run.dir)
  #get number suffix
  run.number = sub(".*_(\\d+)$", "\\1", run.name)
  
  # Load the biomass data
  data.file = file.path(run.dir, 'biomass.rds')
  if (file.exists(data.file)) {
    data.ls[[i]] = readRDS(data.file) |> 
      mutate(run = as.numeric(run.number) )

  } else {
    message(paste("No biomass data found for", run.name))
  }
}

data = bind_rows(data.ls)

species.names = unique(data$species)

#make a pdf with the biomass plots for each species per page
pdf(file = file.path(data.dir, 'biomass_plots.pdf'), width = 8, height = 6)
for (j  in species.names) {
  # Filter data for the current species
  species_data = data %>% filter(species == j)
  
  # Create the plot
  p = ggplot(species_data, aes(x = time, y = atoutput, color = as.factor(run))) +
    geom_line() +
    labs(title = paste("Biomass for", j), x = "Year", y = "Biomass") +
    theme_minimal() +
    theme(legend.title = element_blank())+
    guides(color = 'none')
  
  # Print the plot to the PDF
  print(p)
}
dev.off()

