library(tidyr)
library(ggplot2)
library(dplyr)
library(zoo)

rm(list = ls())
country <- "LUX"

#load files
pub_capita <- read.csv("pub_capita.csv")
pub_gdp <- read.csv("pub_gdp.csv")
tot_gdp <- read.csv("tot_gdp.csv")
gdp <- read.csv("gdp.csv")

#select estonia
gdp <- gdp[gdp$COUNTRY == country, ]
pub_capita <- pub_capita[pub_capita$COUNTRY == country, ]
pub_gdp <- pub_gdp[pub_gdp$COUNTRY == country, ]
tot_gdp <- tot_gdp[tot_gdp$COUNTRY == country, ]

#select columns
gdp <- gdp[, c("YEAR", "VALUE")]
pub_capita <- pub_capita[, c("YEAR", "VALUE")]
pub_gdp <- pub_gdp[, c("YEAR", "VALUE")]
tot_gdp <- tot_gdp[, c("YEAR", "VALUE")]

pub_capita <- subset(pub_capita, YEAR >= 1995 & YEAR <= 2014)
pub_gdp <- subset(pub_gdp, YEAR >= 1995 & YEAR <= 2014)
tot_gdp <- subset(tot_gdp, YEAR >= 1995 & YEAR <= 2014)
gdp <- subset(gdp, YEAR >= 1995 & YEAR <= 2014)

#rename columns
colnames(gdp)[colnames(gdp) == "VALUE"] <- "gdp"
colnames(pub_capita)[colnames(pub_capita) == "VALUE"] <- "pub_capita"
colnames(pub_gdp)[colnames(pub_gdp) == "VALUE"] <- "pub_gdp"
colnames(tot_gdp)[colnames(tot_gdp) == "VALUE"] <- "tot_gdp"

#merge all df
merged_df <- data.frame(
  pub_capita = pub_capita$pub_capita,
  pub_gdp = pub_gdp$pub_gdp,
  tot_gdp = tot_gdp$tot_gdp,
  gdp = gdp$gdp
)

#calc averages
pub_capita_avg <- mean(merged_df$pub_capita, na.rm = TRUE)
pub_gdp_avg <- mean(merged_df$pub_gdp, na.rm = TRUE)
tot_gdp_avg <- mean(merged_df$tot_gdp, na.rm = TRUE)
gdp_avg <- mean(merged_df$gdp, na.rm = TRUE)

#calc the other 2 variables
pub_total_avg <- pub_gdp_avg * gdp_avg
tot_capita_avg <- tot_gdp_avg * gdp_avg

log_pub_capita_avg <- log(pub_capita_avg)
log_pub_gdp_avg <- log(pub_gdp_avg)
log_pub_total_avg <- log(pub_total_avg)


#final table
overall_df <- data.frame(
  country = country,
  pub_capita = pub_capita_avg,
  pub_gdp = pub_gdp_avg,
  pub_total_avg = pub_total_avg,
  tot_capita_avg = tot_capita_avg,
  tot_gdp = tot_gdp_avg,
  gdp = gdp_avg,
  log_pub_capita_avg = log_pub_capita_avg,
  log_pub_gdp_avg = log_pub_gdp_avg,
  log_pub_total_avg = log_pub_total_avg
)

#output table
overall_df
