# pruned_data_file =paste0(results_dir,'atlantis_EOF_scenario_data_pruned.rds')
# out_dir = results_dir

plot_scenario_metrics = function(pruned_data_file, out_dir){
  
  data = readRDS(pruned_data_file)
  
  data.realized.catch = data |> 
    dplyr::filter(Variable == 'catch.tot') |> 
    dplyr::rename(catch_tot = 'Value') |> 
    # dplyr::select(year, run.id, catch_tot, scenario_name, scenario_original_name, dominant_group, dominance_factor) |> 
    dplyr::mutate(realized_catch = catch_tot/eof_threshold_mT) |> 
    dplyr::mutate(realized_catch = ifelse(is.finite(realized_catch),realized_catch, 0))
  
  # data.realized.catch = data |> 
  #   dplyr::left_join(data.catch.tot) |> 
  #   dplyr::mutate(realized_catch = catch_tot/eof_threshold_mT) |> 
  #   dplyr::mutate(realized_catch = ifelse(is.finite(realized_catch),realized_catch, 0))
  # 
  ggplot2::ggplot(data.realized.catch, ggplot2::aes(x= catch.scalar, y = realized_catch, color = scenario_name))+
    ggplot2::geom_point()
}