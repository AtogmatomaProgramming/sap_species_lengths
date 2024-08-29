
##### SCRIPT DESCRIPTION #####

#' This script is oriented to create the master data of the 
#' historical ranges for the fishing grounds "AC", "CN" and "GC"
#' in a range of years 

##### IMPORT WORK FUNCTIONS #####

source('functions.R')

##### WORK DIRECTORIES #####

data_source <- paste0(getwd(), "/data/")

result_directory <- paste0(getwd(), "/results")

#' check_work_directories: function to check or create
#' the directories that we need to work

check_work_directories(data_source, result_directory)


##### IMPORT THE DATA SETS #####

#' measure_ranges: data set with the ranges of the species

measure_ranges <- readRDS(paste0(data_source,
                                 "rim_lengths_2019_2023.rds"))

#' fishing_environment: data set from the the master data
#' "caladero.csv" 

fishing_environment <- read.table(file = paste0(data_source,"caladero.csv"), 
                                  head = TRUE, 
                                  sep = ";",
                                  fill = TRUE, 
                                  colClasses = c("character", "factor", "character"))

# Add the value of the fishing ground to the dataset measure_ranges

measure_ranges <- merge(measure_ranges, 
                        fishing_environment[, c("CALADERO", "COD_ORIGEN")], 
                        by = "COD_ORIGEN", 
                        all.x = TRUE)

# Extract the values of the fishing ground and add them in a list

fishing_grounds <- unique(measure_ranges$CALADERO)

fishing_grounds <- fishing_grounds[!is.na(fishing_grounds)]

list_fishing_grounds <- as.list(fishing_grounds)

##### USE OF "MAIN_FUNCTION" #####

#### OBTAIN DATAFRAME ####

#' Create a list where we catch the values of 
#' apply the "main_function" at the "list_fishing_ground" 

ranges_list <- lapply(list_fishing_grounds, 
       function(fg) {
         main_function(fg, measure_ranges, 
                       result_directory, way_dataframe = TRUE)
       })

#### BOXPLOT MEASURES ####

lapply(list_fishing_grounds, 
       function(fg) {main_function(fg,
                                   measure_ranges, 
                                   result_directory, 
                                   ver = "prueba", 
                                   way_plot = TRUE)
         })



##### CREATE DATAFRAME #####

data_ranges <- do.call(rbind.data.frame, ranges_list)

##### EXPORT FINAL DATAFRAME #####

write.csv(data_ranges, paste0(getwd(),"/", "rango_tallas_historico_caladero_beta_08.csv"))
