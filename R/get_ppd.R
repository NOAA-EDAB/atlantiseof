#' Get Primary Production Data
#'
#' This function reads satellite primary production data files, aggregates them
#' daily, and fills in missing days within each year using a climatological approach.
#' Missing values are interpolated by substituting the mean value for that specific 
#' day of the year (DOY) calculated across the entire available time series.
#'
#' @param ppd.files Character vector. A list of file paths to the CSV files containing primary production data.
#' @param window_size Integer. The size of the centered rolling window (in days) used to 
#'   calculate the recent anomaly mean. Defaults to 15.
#'
#' @return A list containing two data frames:
#'   \item{daily}{Daily primary production data, including climatological interpolated values for missing days.}
#'   \item{yearly}{Yearly aggregated primary production data, with sums of both raw and interpolated daily values.}
#'
#' @importFrom dplyr filter group_by summarise rename mutate ungroup bind_rows left_join select arrange
#' @importFrom tidyr complete
#' @importFrom zoo rollapply
#' @importFrom utils read.csv
#'
#' @export
get_ppd <- function(ppd.files, window_size = 15) {
  
  # List to store processed daily data
  daily_list <- list()
  
  for (i in seq_along(ppd.files)) {
    file <- ppd.files[i]
    
    # 1. Read and filter data
    dat <- utils::read.csv(file) |> 
      dplyr::filter(PROD == 'PPD')
    
    # 2. Summarize to daily and format date
    dat.d <- dat |> 
      dplyr::group_by(start.year, start, UNITS) |> 
      dplyr::summarise(PPD = sum(GMEAN, na.rm = TRUE), .groups = "drop") |> 
      dplyr::rename(
        year = start.year,
        date = start
      ) |> 
      # Convert date string to actual Date object for sequencing
      dplyr::mutate(date = as.Date(date))
    
    daily_list[[i]] <- dat.d
  }
  
  # 3. Combine all daily data
  ppd.data.d <- dplyr::bind_rows(daily_list)
  
  # 4. Add Day of Year (yday) to calculate climatology
  ppd.data.d <- ppd.data.d |> 
    dplyr::mutate(yday = as.numeric(format(date, "%j")))
  
  # 5. Calculate Climatology (Mean PPD per Day of Year)
  climatology <- ppd.data.d |> 
    dplyr::group_by(UNITS, yday) |> 
    dplyr::summarise(PPD_clim_mean = mean(PPD, na.rm = TRUE), .groups = "drop")
  
  # 6. Complete missing days for each year
  # Grouping by year and UNITS ensures we create a full 365/366 day year for each unit
  ppd.data.d <- ppd.data.d |> 
    dplyr::group_by(year, UNITS) |> 
    tidyr::complete(
      date = seq.Date(
        from = as.Date(paste0(format(min(date, na.rm = TRUE), "%Y"), "-01-01")), 
        to = as.Date(paste0(format(min(date, na.rm = TRUE), "%Y"), "-12-31")), 
        by = "day"
      )
    ) |> 
    dplyr::ungroup() |> 
    # Recalculate yday for the newly injected missing dates
    dplyr::mutate(yday = as.numeric(format(date, "%j")))
  
  # 7. Interpolate missing values using Anomaly-Adjusted Climatology
  ppd.data.d <- ppd.data.d |> 
    dplyr::left_join(climatology, by = c("UNITS", "yday")) |> 
    dplyr::arrange(UNITS, date) |> 
    dplyr::group_by(UNITS) |> 
    dplyr::mutate(
      # Calculate raw anomaly (how far off is the actual from the historic day average)
      PPD_anom = PPD - PPD_clim_mean,
      
      # Calculate the recent mean of the anomaly using a rolling window
      recent_anom_mean = zoo::rollapply(
        data = PPD_anom, 
        width = window_size, 
        FUN = function(x) mean(x, na.rm = TRUE), 
        partial = TRUE, 
        align = "center",
        fill = NA
      ),
      
      # If the gap is wider than the window, default anomaly to 0 (revert strictly to climatology)
      recent_anom_mean = ifelse(is.na(recent_anom_mean), 0, recent_anom_mean),
      
      # Interpolate: Base Climatology + Recent Anomaly
      ppd.interp = ifelse(is.na(PPD), PPD_clim_mean + recent_anom_mean, PPD),
      
      # Ensure no negative PPD values result from subtracting a negative anomaly
      ppd.interp = pmax(ppd.interp, 0)
    ) |> 
    dplyr::ungroup() |> 
    dplyr::select(-PPD_clim_mean, -PPD_anom, -recent_anom_mean)
  
  # 8. Calculate yearly summaries
  ppd.data.y <- ppd.data.d |> 
    dplyr::group_by(year, UNITS) |> 
    dplyr::summarise(
      PPD = sum(PPD, na.rm = TRUE),
      PPD_interp = sum(ppd.interp, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 9. Return both dataframes as a list
  return(list(
    daily = ppd.data.d,
    yearly = ppd.data.y
  ))
}

# plot.dat = ppd.data.y |> 
#   tidyr::gather(Var, Value, -year, -UNITS)
# ggplot(plot.dat, aes(x = year, y = Value,color = Var))+
#   geom_line()
