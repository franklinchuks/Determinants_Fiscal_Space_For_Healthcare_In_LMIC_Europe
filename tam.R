library(tidyr)
library(ggplot2)
library(dplyr)
library(zoo)

rm(list = ls())
country <- "EST"

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

empty_rows <- data.frame(YEAR = c("2015", "2016", "2017"), VALUE = rep(NA, 3))
if (ncol(pub_gdp) > 2) {
  empty_rows <- cbind(empty_rows, rep(NA, ncol(pub_gdp) - 2))
}
pub_gdp <- rbind(pub_gdp, empty_rows)
pub_capita <- rbind(pub_capita, empty_rows)
tot_gdp <- rbind(tot_gdp, empty_rows)

####################################################
####################################################
####################################################
new_2015 <- c("2015", "")
new_2016 <- c("2016", "")
new_2017 <- c("2017", "")
pub_gdp <- rbind(pub_gdp, new_2015)
pub_gdp <- rbind(pub_gdp, new_2016)
pub_gdp <- rbind(pub_gdp, new_2017)
pub_capita <- rbind(pub_capita, new_2015)
pub_capita <- rbind(pub_capita, new_2016)
pub_capita <- rbind(pub_capita, new_2017)
tot_gdp <- rbind(tot_gdp, new_2015)
tot_gdp <- rbind(tot_gdp, new_2016)
tot_gdp <- rbind(tot_gdp, new_2017)
####################################################
####################################################
####################################################

pub_gdp$VALUE <- as.numeric(pub_gdp$VALUE)
pub_gdp$VALUE <- na.aggregate(pub_gdp$VALUE, FUN = mean, na.rm = TRUE)

pub_capita$VALUE <- as.numeric(pub_capita$VALUE)
pub_capita$VALUE <- na.aggregate(pub_capita$VALUE, FUN = mean, na.rm = TRUE)

tot_gdp$VALUE <- as.numeric(tot_gdp$VALUE)
tot_gdp$VALUE <- na.aggregate(tot_gdp$VALUE, FUN = mean, na.rm = TRUE)

#rename columns
colnames(gdp)[colnames(gdp) == "VALUE"] <- "gdp"
colnames(pub_capita)[colnames(pub_capita) == "VALUE"] <- "pub_capita"
colnames(pub_gdp)[colnames(pub_gdp) == "VALUE"] <- "pub_gdp"
colnames(tot_gdp)[colnames(tot_gdp) == "VALUE"] <- "tot_gdp"

#resize the no. of rows
gdp <- gdp[1:(nrow(gdp)-4), , drop = FALSE]
gdp <- gdp[-(1:5), , drop = FALSE]
pub_capita <- pub_capita[-(1:5), , drop = FALSE]
pub_gdp <- pub_gdp[-(1:5), , drop = FALSE]
tot_gdp <- tot_gdp[-(1:5), , drop = FALSE]

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
pub_total_avg <- ((tot_gdp_avg / pub_gdp_avg) - 1)
tot_capita_avg <- tot_gdp_avg + gdp_avg

#final table
overall_df <- data.frame(
  country = country,
  pub_capita = pub_capita_avg,
  pub_gdp = pub_gdp_avg,
  tot_gdp = tot_gdp_avg,
  gdp = gdp_avg
)

#output table
overall_df

