## Zone of Influence
## 140 m by 25 m 
## September 2022

#### Each bin is a potential ZOI ####

setwd("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

library(tidyverse)
library(dplyr)

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)
area <- 140*25

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

all_zoi <- load_data("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

#### Obtaining abundances ####

for (i in 1:length(all_zoi$PRC_ABC)) {
  all_zoi$abundance[i] <- NA
  all_zoi$abundance[i] <- all_zoi$PRC_ABC[i]*area/sigma_bs_herring
}

#### Creating new data frames for filtering ####

each_zoi <- all_zoi %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

domain_zoi <- all_zoi %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

all_transects <- merge(each_zoi, domain_zoi, by = "Region_name")

division_function <- function(sum_each, sum_region) {
  sum_each/sum_region
}

#### Probabilities ####

all_prob <- division_function(all_transects$Sum_each, all_transects$Sum_region)

## Histograms

hist(all_prob, breaks = 100, xlab = "Probability", main = "Day & Night Probabilities")
mean_day_night <- mean(all_prob)
abline(v = mean_day_night, col = "red")

#### Day ####

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

day_zoi <- load_data("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

#### Filter transects ####

day_zoi <- day_zoi %>% filter(Region_name != "GRID_B_N_NIGHT_6_04_12" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_11" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_10" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_09" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_08" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_07" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_06" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_05" &                                                     
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_04" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_03" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_02" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_01" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_10" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_09" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_08" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_07" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_06" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_05" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_04" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_03" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_02" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_04_01" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_01" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_02" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_03" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_04" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_05" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_06" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_07" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_08" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_09" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_10" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_11" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_09_12" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_01" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_02" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_03" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_04" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_05" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_06" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_07" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_08" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_09" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_10" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_11" &
                                                                  Region_name != "GRID_B_N_NIGHT_6_11_12" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_01" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_02" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_03" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_04" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_05" & 
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_06" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_07" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_08" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_09" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_10" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_11" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_04_12" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_01" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_02" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_03" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_04" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_05" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_06" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_07" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_08" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_09" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_10" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_11" &
                                                                  Region_name != "GRID_A_N_NIGHT_5_07_12")


#### Getting the abundance of each transect and domains ####

for (i in 1:length(day_zoi$PRC_ABC)) {
  day_zoi$abundance[i] <- NA
  day_zoi$abundance[i] <- day_zoi$PRC_ABC[i]*area/sigma_bs_herring
}

#### Creating new data frames for filtering ####

day_each_zoi <- day_zoi %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

day_domain_zoi <- day_zoi %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

day_all_transects <- merge(day_each_zoi, day_domain_zoi, by = "Region_name")

division_function <- function(sum_each, sum_region) {
  sum_each/sum_region
}

#### Probabilities ####

day_all_prob <- division_function(day_all_transects$Sum_each, day_all_transects$Sum_region)

## Histograms

hist(day_all_prob, breaks = 100, xlab = "Probability", main = "Day Probabilities")
mean_day <- mean(day_all_prob)
abline(v = mean_day, col = "red")

#### Night ####

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

night_zoi <- load_data("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

#### Filter transects ####

night_zoi <- night_zoi %>% filter(Region_name %in% c("GRID_B_N_NIGHT_6_04_12", 
                                                                                       "GRID_B_N_NIGHT_6_04_11",
                                                                                       "GRID_B_N_NIGHT_6_04_10",
                                                                                       "GRID_B_N_NIGHT_6_04_09",
                                                                                       "GRID_B_N_NIGHT_6_04_08",
                                                                                       "GRID_B_N_NIGHT_6_04_07",
                                                                                       "GRID_B_N_NIGHT_6_04_06",
                                                                                       "GRID_B_N_NIGHT_6_04_05",
                                                                                       "GRID_B_N_NIGHT_6_04_04",
                                                                                       "GRID_B_N_NIGHT_6_04_03",
                                                                                       "GRID_B_N_NIGHT_6_04_02",
                                                                                       "GRID_B_N_NIGHT_6_04_01",
                                                                                       "GRID_B_N_NIGHT_6_04_10",
                                                                                       "GRID_B_N_NIGHT_6_04_09",
                                                                                       "GRID_B_N_NIGHT_6_04_08",
                                                                                       "GRID_B_N_NIGHT_6_04_07", 
                                                                                       "GRID_B_N_NIGHT_6_04_06",
                                                                                       "GRID_B_N_NIGHT_6_04_05",
                                                                                       "GRID_B_N_NIGHT_6_04_04",
                                                                                       "GRID_B_N_NIGHT_6_04_03",
                                                                                       "GRID_B_N_NIGHT_6_04_02",
                                                                                       "GRID_B_N_NIGHT_6_04_01",
                                                                                       "GRID_B_N_NIGHT_6_09_01",
                                                                                       "GRID_B_N_NIGHT_6_09_02",
                                                                                       "GRID_B_N_NIGHT_6_09_03",
                                                                                       "GRID_B_N_NIGHT_6_09_04", 
                                                                                       "GRID_B_N_NIGHT_6_09_05",
                                                                                       "GRID_B_N_NIGHT_6_09_06",
                                                                                       "GRID_B_N_NIGHT_6_09_07",
                                                                                       "GRID_B_N_NIGHT_6_09_08",
                                                                                       "GRID_B_N_NIGHT_6_09_09",
                                                                                       "GRID_B_N_NIGHT_6_09_10",
                                                                                       "GRID_B_N_NIGHT_6_09_11",
                                                                                       "GRID_B_N_NIGHT_6_09_12",
                                                                                       "GRID_B_N_NIGHT_6_11_01",
                                                                                       "GRID_B_N_NIGHT_6_11_02",
                                                                                       "GRID_B_N_NIGHT_6_11_03",
                                                                                       "GRID_B_N_NIGHT_6_11_04",
                                                                                       "GRID_B_N_NIGHT_6_11_05",
                                                                                       "GRID_B_N_NIGHT_6_11_06",
                                                                                       "GRID_B_N_NIGHT_6_11_07",
                                                                                       "GRID_B_N_NIGHT_6_11_08",
                                                                                       "GRID_B_N_NIGHT_6_11_09",
                                                                                       "GRID_B_N_NIGHT_6_11_10",
                                                                                       "GRID_B_N_NIGHT_6_11_11",
                                                                                       "GRID_B_N_NIGHT_6_11_12",
                                                                                       "GRID_A_N_NIGHT_5_04_01",
                                                                                       "GRID_A_N_NIGHT_5_04_02",
                                                                                       "GRID_A_N_NIGHT_5_04_03",
                                                                                       "GRID_A_N_NIGHT_5_04_04",
                                                                                       "GRID_A_N_NIGHT_5_04_05",
                                                                                       "GRID_A_N_NIGHT_5_04_06",
                                                                                       "GRID_A_N_NIGHT_5_04_07",
                                                                                       "GRID_A_N_NIGHT_5_04_08",
                                                                                       "GRID_A_N_NIGHT_5_04_09",
                                                                                       "GRID_A_N_NIGHT_5_04_10",
                                                                                       "GRID_A_N_NIGHT_5_04_11",
                                                                                       "GRID_A_N_NIGHT_5_04_12",
                                                                                       "GRID_A_N_NIGHT_5_07_01",
                                                                                       "GRID_A_N_NIGHT_5_07_02",
                                                                                       "GRID_A_N_NIGHT_5_07_03",
                                                                                       "GRID_A_N_NIGHT_5_07_04",
                                                                                       "GRID_A_N_NIGHT_5_07_05",
                                                                                       "GRID_A_N_NIGHT_5_07_06",
                                                                                       "GRID_A_N_NIGHT_5_07_07",
                                                                                       "GRID_A_N_NIGHT_5_07_08",
                                                                                       "GRID_A_N_NIGHT_5_07_09",
                                                                                       "GRID_A_N_NIGHT_5_07_10",
                                                                                       "GRID_A_N_NIGHT_5_07_12"))


#### Getting the abundance of each transect and domains ####

for (i in 1:length(night_zoi$PRC_ABC)) {
  night_zoi$abundance[i] <- NA
  night_zoi$abundance[i] <- night_zoi$PRC_ABC[i]*area/sigma_bs_herring
}

#### Creating new data frames for filtering ####

night_each_zoi <- night_zoi %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

night_domain_zoi <- night_zoi %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

night_all_transects <- merge(night_each_zoi, night_domain_zoi, by = "Region_name")

division_function <- function(sum_each, sum_region) {
  sum_each/sum_region
}

#### Probabilities ####

night_all_prob <- division_function(night_all_transects$Sum_each, night_all_transects$Sum_region)

## Histograms

hist(night_all_prob, breaks = 100, xlab = "Probability", main = "Night Probabilities")
mean_night <- mean(night_all_prob)
abline(v = mean_night, col = "red")
