install.packages("readxl")
install.packages("xlsx")
rm(list = ls(all.names = TRUE)) 
library(dplyr)
library(tidyverse)
library(xlsx)
library(readxl)
ghed <- read_excel("ghed.xlsx")
######################################################
######################################################
######################################################
ghed_filtered_columns <- ghed[, c("country", "che_gdp", "che_pc_usd", "gghed", "gghed_gge", "gghed_pc_usd", "pvtd_pc_usd", "oop_pc_usd", "ext_pc_usd", "gdp_pc_usd", "pop", "gge_gdp")]
ghed_filtered_countries <- ghed_filtered_columns %>%
  filter(country %in% c("Estonia", "Türkiye", "Ukraine", "Georgia", "Armenia", "Iceland", "Luxembourg", "Switzerland", "Norway", "Denmark"))

columns_to_calculate <- c("che_gdp", "che_pc_usd", "gghed_gge", "gghed_pc_usd", "pvtd_pc_usd", "oop_pc_usd", "gge_gdp")

for (col in columns_to_calculate) {
  ghed_filtered_countries[[paste0(col, "_log_growth")]] <- 
    log(ghed_filtered_countries[[col]]) - log(dplyr::lag(ghed_filtered_countries[[col]]))
}
ghed_filtered_countries <- ghed_filtered_countries[-1, ]

ghed_filtered_countries <- aggregate(. ~ country, data = ghed_filtered_countries, FUN = mean, na.rm = TRUE)

write.xlsx2(ghed_filtered_countries, file = "output.xlsx")

