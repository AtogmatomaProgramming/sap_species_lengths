

#Functions

take_range_measures <- function(measure_ranges, fishing_environment, result_directory){
  
  #Step 1: Add the fishing environment to our measure_ranges
  measure_ranges <- merge(measure_ranges, fishing_environment[, c("CALADERO", "COD_ORIGEN")], by = "COD_ORIGEN", all.x = TRUE)
  
  #Step 2: take the vector with the value of the fishing grounds
  
  fishing_grounds <- unique(measure_ranges$CALADERO)
  
  #Step 3: remove Na values from fishing_grounds
  
  fishing_grounds <- fishing_grounds[!is.na(fishing_grounds)]
  
  #Step 4: filter the measure_range according to the fishing_ground
  #and take the columns "ESP_CAT" and "TALLA"

  lista_outliers <- lapply(fishing_grounds, function(i){
    filter_measure_dataframe(measure_ranges, i)
  })
  
  #Step 5: export the dataframes from the fishing grounds and save them as csv file
  
  df_outliers <- do.call(rbind.data.frame, lista_outliers)
  
  df_outliers[, -c(1)]
  
  sapmuebase::exportCsvSAPMUEBASE(df_outliers, "outliers_especies_2019_2023.csv", path = paste0(getwd(),"/",result_directory))
  
  df_outliers
  
}


filter_measure_dataframe <- function(measure_ranges, fishing_ground){
  
  #Step 1.1: transform unitary vector to a string
  fishing_ground <- as.character(fishing_ground)
  
  measure_ranges <- measure_ranges[measure_ranges$CALADERO==fishing_ground, c("ESP_CAT", "TALLA")]
  
  #Step 2: Make the boxplot from the measures
  m_graphic <- boxplot(TALLA ~ ESP_CAT, data = measure_ranges)
  
  #Step 3: Take the dataframe with the stats
  m_stats <- as.data.frame(t(m_graphic$stats))
  
  #Step 4: modify the m_stats dataframe 
  m_stats$Species <- m_graphic$names
  
  columns_names <- c("Down_Whisker", "First_Quartile", "Median", "Third_Quartile", "Upper_Whisker", "Species")
  
  colnames(m_stats) <- columns_names
  
  m_stats <- m_stats[, c("Species", "Down_Whisker", "Upper_Whisker")]
  
  m_stats <- m_stats[!is.na(m_stats$Down_Whisker), ]
  
  m_stats$CALADERO <- fishing_ground
  
  return(m_stats)
  
}


