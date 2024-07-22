#' Principal functions

#' @name draw_interactive_boxplot
#' @description Function to draw interactive box plots for
#' the distribution of the lengths for the diverse
#' fishing grounds.
#' @param measure_ranges: dataframe with the different measures
#' for the species.
#' @param fishing_ground: a unitary string vector with the acronym
#' of the fishing ground
#' @param result_directory: a string value with the name of the
#' directory where the interactive graphics will be stored
#' @return the interactive boxplot for species according to
#' fishing ground

draw_interactive_boxplot <- function(measure_ranges, fishing_ground, result_directory) {
  measure_ranges <- manipulate_measure_range(measure_ranges, fishing_ground)

  plot <- ggplot2::ggplot(data = measure_ranges, aes(x = ESP_CAT, y = TALLA)) +
    ggplot2::stat_boxplot(geom = "errorbar", width = 0.5) +
    ggplot2::geom_boxplot(fill = "#4271AE", outlier.colour = "red", alpha = 0.9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::ggtitle(paste0("Distribución tallas caladero ", fishing_ground))

  p <- plotly::ggplotly(plot)

  htmlwidgets::saveWidget(
    p,
    paste0(
      getwd(),
      "/",
      result_directory,
      "/grafico_interactivo_tallas_",
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

  # Step 2: Make the boxplot from the measures
  m_graphic <- boxplot(TALLA ~ ESP_CAT, data = measure_ranges)

  # Step 3: Take the dataframe with the stats
  m_stats <- as.data.frame(t(m_graphic$stats))

  # Step 4: modify the m_stats dataframe
  m_stats$Species <- m_graphic$names

  columns_names <- c("Down_Whisker", "First_Quartile", "Median", "Third_Quartile", "Upper_Whisker", "Species")

  colnames(m_stats) <- columns_names

  m_stats <- m_stats[, c("Species", "Down_Whisker", "Upper_Whisker")]

  m_stats <- m_stats[!is.na(m_stats$Down_Whisker), ]

  m_stats$CALADERO <- fishing_ground
  
  m_stats$Species <- gsub(".U", "(U)", m_stats$Species)
  m_stats$Species <- gsub(".M", "(M)", m_stats$Species)
  m_stats$Species <- gsub(".H", "(H)", m_stats$Species)

  return(m_stats)
}

#' @name lengths_and_graphics
#' @description Function to obtain the final dataset and the
#' interactive boxplot for all the species from the working
#' fishing grounds
#' @param measure_ranges: dataframe with the different measures
#' for the species.
#' @param fishing_environment: dataframe with the name, acronym and
#' code of the fishing grounds
#' @param result_directory: a string value with the name of the
#' directory where the interactive graphics will be stored
#' @return The final dataset and the
#' interactive boxplot for all the species from the working
#' fishing grounds


lengths_and_graphics <- function(measure_ranges, fishing_environment, result_directory) {
  measure_ranges <- merge(measure_ranges, 
                          fishing_environment[, c("CALADERO", "COD_ORIGEN")], 
                          by = "COD_ORIGEN", 
                          all.x = TRUE)

  fishing_grounds <- unique(measure_ranges$CALADERO)

  fishing_grounds <- fishing_grounds[!is.na(fishing_grounds)]

  lista_outliers <- lapply(fishing_grounds, function(i) {
    filter_measure_dataframe(measure_ranges, i)
  })

  df_outliers <- do.call(rbind.data.frame, lista_outliers)

  df_outliers[, -c(1)]

  sapmuebase::exportCsvSAPMUEBASE(df_outliers, "outliers_especies_2019_2023.csv", path = paste0(getwd(), "/", result_directory))

  df_outliers

  lapply(fishing_grounds, function(i) {
    draw_interactive_boxplot(measure_ranges, i, result_directory)
  })
}


#' @name manipulate_measure_range
#' @description function to filter the measure_ranges dataframe
#' according to fishing ground, deleting the NA values and taking
#' only the columns "ESP_CAT" and "TALLA"
#' @param measure_ranges: dataframe with the different measures
#' for the species.
#' @param fishing_ground: a unitary string vector with the acronym
#' of the fishing ground
#' @return the measure_ranges dataframe according to fishing ground,
#' deleting the NA values and taking
#' only the columns "ESP_CAT" and "TALLA"


manipulate_measure_range <- function(measure_ranges, fishing_ground) {
  fishing_ground <- as.character(fishing_ground)

  measure_ranges <- measure_ranges[measure_ranges$CALADERO == fishing_ground & 
                                     measure_ranges$PROCEDENCIA == "IEO", 
                                   c("COD_ESP_CAT", "ESP_CAT", "SEXO", "TALLA")]

  measure_ranges <- measure_ranges[!is.na(measure_ranges$TALLA), ]

  return(measure_ranges)
}
