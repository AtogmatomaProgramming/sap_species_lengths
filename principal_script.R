#Import libraries

library(ggplot2)

#Define the data directory source

data_source <- paste0(getwd(), "/data/")

#Import the work files

measure_ranges <- readRDS(paste0(data_source,
                                 "rim_lengths_2019_2023.rds"))

fishing_environment <- read.table(file = paste0(data_source,"caladero.csv"), 
                                  head = TRUE, 
                                  sep = ";",
                                  fill = TRUE, 
                                  colClasses = c("character", "factor", "character"))

#Import functions

source('functions.R')

#Apply the function to obtain the dataframe with values of the outliers

a <- lengths_and_graphics(measure_ranges, fishing_environment, "results")
