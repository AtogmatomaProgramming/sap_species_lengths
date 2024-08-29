#### LIST OF FUNCTIONS ####

#' @check_directories
#' @description Function to create or check if we 
#' have the necessary directories for work with
#' the script
#' @param data_source path of the directory where
#' we have the work data
#' @param result_directory path of the directory
#' where we store the results (boxplots, dataframes)
#' of the process
#' @return create the directories or print a message 
#' of "Directory has already exist"

check_work_directories <- function(data_source, result_directory){
  
  ifelse(dir.exists(data_source), 
         print("Directory has already exist"),
         dir.create(data_source))
  
  ifelse(dir.exists(result_directory), 
         print("Directory has already exist"),
         dir.create(result_directory))
  
}


#' @name draw_interactive_boxplot
#' @description Function to draw interactive box plots for
#' the distribution of the lengths for the diverse
#' fishing grounds
#' @param measure_ranges: dataframe with the different measures
#' for the species
#' @param ver: string value if you want to name the plots in another way
#' if you are drawing more than one
#' @param result_directory: a string value with the name of the
#' directory where the interactive graphics will be stored
#' @return the interactive boxplot for species according to
#' fishing ground

draw_interactive_boxplot <- function(measure_ranges, ver, result_directory, fishing_ground) {
  

  plot <- ggplot2::ggplot(data = measure_ranges, ggplot2::aes(x = ESP_CAT, y = TALLA)) +
    ggplot2::stat_boxplot(geom = "errorbar", width = 0.5) +
    ggplot2::geom_boxplot(fill = "#4271AE", outlier.colour = "red", alpha = 0.9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::ggtitle(paste0("Distribución tallas caladero ", fishing_ground))

  p <- plotly::ggplotly(plot)

  htmlwidgets::saveWidget(
    p,
    paste0(
      result_directory,
      "/grafico_interactivo_tallas_",
      ver,
      "_",
      fishing_ground,
      ".html"
    )
  )
}

#' @name filter_measure_dataframe
#' @description function to get the stat values for
#' the lengths of the species according to the fishing grounds.
#' @param measure_ranges: dataframe with the different measures
#' for the species.
#' @param fishing_ground: a unitary string vector with the acronym
#' of the fishing ground
#' @param result_directory: a string value with the name of the
#' directory where the interactive graphics will be stored
#' @return a dataset with the stat values for
#' the lengths of the species according to the fishing grounds.

filter_measure_dataframe <- function(measure_ranges, fishing_ground) {
  
  measure_ranges <- manipulate_measure_range(measure_ranges, fishing_ground)

  m_graphic <- boxplot(TALLA ~ ESP_CAT, data = measure_ranges)

  m_stats <- as.data.frame(t(m_graphic$stats))

  m_stats$Species <- m_graphic$names

  columns_names <- c("Down_Whisker", "First_Quartile", "Median", "Third_Quartile", "Upper_Whisker", "Species")

  colnames(m_stats) <- columns_names

  m_stats <- m_stats[, c("Species", "Down_Whisker", "Upper_Whisker")]

  m_stats <- m_stats[!is.na(m_stats$Down_Whisker), ]

  m_stats$CALADERO <- fishing_ground

  return(m_stats)
  
}

#' @name main_function
#' @description this is the function where you can do all the 
#' tasks that are necessary in order to obtain the size data:
#' make the boxplots to check the discordant measures and 
#' obtain the final dataset with the max and min length for 
#' each specie according to the fishing ground and the sex
#' @param measure_ranges: dataframe with the different measures
#' for the species.
#' @param fishing_ground: a unitary string vector with the acronym
#' of the fishing ground
#' @param result_directory: string value with the name of the directory
#' where the results will be stored
#' @param ver: string value if you want to name the plots in another way
#' if you are drawing more than one
#' @param discarded_sizes: dataframe with the discarded sizes from the 
#' check made with the boxplots
#' @param way_plot: boolean value to select if you want to make or not
#' the boxplot of the sizes
#' @param way_dataframe: boolean value to select if you want to make or not
#' the dataframe with the max and min sizes
#' @return the boxplot or the max and min dataset, in the function what you need

main_function <- function(fishing_ground, 
                          measure_ranges, 
                          result_directory, 
                          ver = NA,
                          discarded_sizes = NA,
                          way_plot = FALSE,
                          way_dataframe = FALSE){
  
  measure_ranges <- as.data.frame(measure_ranges)
  
  fishing_ground <- as.character(as.vector(fishing_ground))
  
  measure_ranges_filtered <- manipulate_measure_range(measure_ranges, fishing_ground)
  
  #measure_ranges_filtered <- as.data.frame(measure_ranges_filtered)
  
  if (is.data.frame(discarded_sizes) && nrow(discarded_sizes) > 0) {
    
    measure_ranges_filtered$ESP_CAT <- paste0(measure_ranges_filtered$ESP_CAT, 
                                              "_", 
                                              measure_ranges_filtered$TALLA)
    
    discarded_sizes <- manipulate_discarded_sizes(discarded_sizes, fishing_ground)
    
    measure_ranges_filtered <- measure_ranges_filtered[!(measure_ranges_filtered$ESP_CAT %in% discarded_sizes$ESP_CAT), ]
    
    measure_ranges_filtered <- measure_ranges_filtered |> tidyr::separate(col = ESP_CAT,
                                                                          into = c("ESP_CAT", "TALLA"),
                                                                          sep = "_")
    
    measure_ranges_filtered$TALLA <- as.numeric(measure_ranges_filtered$TALLA)
    
  }
  
  
  if(way_plot){
    
    draw_interactive_boxplot(measure_ranges_filtered, ver, result_directory, fishing_ground)
    
  }
  
  if(way_dataframe){
    
    measure_ranges_filtered <- max_min_sizes(measure_ranges_filtered)
    
  }
  
}


#' @name manipulate_measure_range
#' @description function to filter the measure_ranges dataframe
#' according to fishing ground, deleting the NA values and taking
#' and the one register's measures
#' @param measure_ranges: dataframe with the different measures
#' for the species.
#' @param fishing_ground: a unitary string vector with the acronym
#' of the fishing ground
#' @return the measure_ranges dataframe according to fishing ground,
#' where the value of ESP_CAT and SEXO are pasted


manipulate_measure_range <- function(measure_ranges, fishing_ground) {
  
  measure_ranges_filtered <- measure_ranges[measure_ranges$CALADERO == fishing_ground & 
                                              measure_ranges$PROCEDENCIA == "IEO" &
                                              measure_ranges$COD_TIPO_MUE == "2", 
                                            c("COD_ESP_CAT", "ESP_CAT", "SEXO", "TALLA", "CALADERO")]
  
  measure_ranges_filtered <- measure_ranges_filtered[!is.na(measure_ranges_filtered$TALLA), ]
  
  measure_ranges_filtered <- measure_ranges_filtered |>
    dplyr::group_by(ESP_CAT, SEXO) |>
    dplyr::mutate(TOTAL_TALLAS = dplyr::n_distinct(TALLA))
  
  measure_ranges_filtered <- measure_ranges_filtered[measure_ranges_filtered$TOTAL_TALLAS != 1, ]
  
  
  measure_ranges_filtered$ESP_CAT <- paste0(measure_ranges_filtered$ESP_CAT, 
                                            "-", 
                                            measure_ranges_filtered$SEXO)
  
  return(measure_ranges_filtered)
  
}

#' @name manipulate_discarded_sizes
#' @description function to manipulate de discarded sizes according
#' to the fishing ground
#' @param discarded_sizes: dataframe of all discarded sizes from all
#' fishing grounds
#' @param fishing_ground: a unitary string vector with the acronym
#' of the fishing ground
#' @return the discarded_sizes dataframe according to fishing ground,
#' where the value of ESP_CAT and TALLA are pasted


manipulate_discarded_sizes <- function(discarded_sizes, fishing_ground) {
  
  discarded_sizes <- discarded_sizes[discarded_sizes$CALADERO==fishing_ground, ]
  
  discarded_sizes$CALADERO <- NA

  discarded_sizes$ESP_CAT <- paste0(discarded_sizes$ESP_CAT, "_", discarded_sizes$TALLA)
  
}

#' @name max_min_sizes
#' @description function to create the final dataframe with the 
#' max and min values for the sizes for the historical dataset
#' acocording to the fishing ground
#' @param measure_ranges: dataframe with the different measures
#' for the species.
#' @return the dataframe with the max and min values 
#' for the sizes for the historical dataset
#' acocording to the fishing ground


max_min_sizes <- function(measure_ranges) {
  
  
  measure_ranges <- measure_ranges |> 
    dplyr::group_by(COD_ESP_CAT, SEXO, CALADERO) |> 
    dplyr::summarise(TALLA_MAX = max(TALLA),
                     TALLA_MIN = min(TALLA))
  
  measure_ranges$COD_ESP <- measure_ranges$COD_ESP_CAT
  
  measure_ranges$COD_ESP_CAT <- NA
  
  measure_ranges <- measure_ranges[, c("COD_ESP", "CALADERO", "SEXO", 
                                       "TALLA_MIN", "TALLA_MAX")]
  
}


