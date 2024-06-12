#Step 1: Import the data sets from the folder "data"

#Define the data directory source

data_source <- paste0(getwd(), "/data/")

#Import the work files

measure_ranges <- readRDS(paste0(data_source,"rim_lengths_2019_2023.rds"))

fishing_environment <- read.table(file = paste0(data_source,"caladero.csv"), head = TRUE, sep = ";",
                                  fill = TRUE, colClasses = c("character", "factor", "character"))

#Import functions

source('functions.R')

#Apply the function to obtain the dataframe with values of the outliers

a <- take_range_measures(measure_ranges, fishing_environment, "results")
