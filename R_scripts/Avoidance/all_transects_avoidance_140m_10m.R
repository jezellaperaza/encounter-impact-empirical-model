## Active Avoidance
## 140 m by 10 m 
## September-October 2022

library(tidyverse)
library(dplyr)

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)
area <- 140*10

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

all_avoidance <- load_data("~/UW Summer 2022/EV Exports/Mobile/Avoidance")

#### Obtaining abundances ####

for (i in 1:length(all_avoidance$PRC_ABC)) {
  all_avoidance$abundance[i] <- NA
  all_avoidance$abundance[i] <- all_avoidance$PRC_ABC[i]*area/sigma_bs_herring
}

#### Applying avoidance factor from Shen et al. 2016 ####

## Shen

num_avoidance <- vector()

for (i in 1:length(all_avoidance$abundance)) {
  num_avoidance[i] <- NA
  num_avoidance[i] <- all_avoidance$abundance[i]*0.372
}

all_avoidance$num_avoidance <- num_avoidance

#### Putting new avoidance values into data frame ####

abundance_new <- vector()
for (i in 1:length(all_avoidance$abundance)) {
  abundance_new[i] <- NA
  abundance_new[i] <- all_avoidance$abundance[i] - all_avoidance$num_avoidance[i]
}

all_avoidance$abundance_new <- abundance_new

### Creating new data frames for filtering ####

each_avoid <- all_avoidance %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_avoided = sum(num_avoidance))

domain_avoid <- all_avoidance %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

all_transects_avoidance <- merge(each_avoid, domain_avoid, by = "Region_name")

division_function <- function(sum_avoided, sum_region) {
  sum_avoided/sum_region
}

#### Probabilities ####

all_prob_avoid <- division_function(all_transects_avoidance$Sum_avoided, all_transects_avoidance$Sum_region)
all_prob_avoid <- na.omit(all_prob_avoid)

## Histograms

hist(all_prob_avoid, breaks = 100, xlab = "Probability", main = "Day & Night Probabilities")
mean_avoid <- mean(all_prob_avoid)
abline(v = mean_avoid, col = "red")

#### Day ####

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

day_avoid <- load_data("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

#### Filter transects ####

day_avoid <- day_avoid %>% filter(Region_name != "GRID_B_N_NIGHT_6_04_12" &
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

for (i in 1:length(day_avoid$PRC_ABC)) {
  day_avoid$abundance[i] <- NA
  day_avoid$abundance[i] <- day_avoid$PRC_ABC[i]*area/sigma_bs_herring
}

#### Applying avoidance factor from Shen et al. 2016 ####

## Shen

day_num_avoidance <- vector()

for (i in 1:length(day_avoid$abundance)) {
  day_num_avoidance[i] <- NA
  day_num_avoidance[i] <- day_avoid$abundance[i]*0.372
}

day_avoid$day_num_avoidance <- day_num_avoidance

#### Putting new avoidance values into data frame ####

day_abundance_new <- vector()

for (i in 1:length(day_avoid$abundance)) {
  day_abundance_new[i] <- NA
  day_abundance_new[i] <- day_avoid$abundance[i] - day_avoid$day_num_avoidance[i]
}

day_avoid$day_abundance_new <- day_abundance_new

#### Creating new data frames for filtering ####

day_each_avoid <- day_avoid %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_avoided = sum(day_num_avoidance))

day_domain_avoid <- day_avoid %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

day_all_transects <- merge(day_each_avoid, day_domain_avoid, by = "Region_name")

division_function <- function(Sum_avoided, sum_region) {
  Sum_avoided/sum_region
}

#### Probabilities ####

day_all_prob_avoid <- division_function(day_all_transects$Sum_avoided, day_all_transects$Sum_region)

## Histograms

hist(day_all_prob_avoid, breaks = 100, xlab = "Probability", main = "Day Probabilities")
mean_day_avoid <- mean(day_all_prob_avoid)
abline(v = mean_day_avoid, col = "red")

#### Night ####

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

night_avoid <- load_data("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

#### Filter transects ####

night_avoid <- night_avoid %>% filter(Region_name %in% c("GRID_B_N_NIGHT_6_04_12", 
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

for (i in 1:length(night_avoid$PRC_ABC)) {
  night_avoid$abundance[i] <- NA
  night_avoid$abundance[i] <- night_avoid$PRC_ABC[i]*area/sigma_bs_herring
}

#### Applying avoidance factor from Shen et al. 2016 ####

## Shen

night_num_avoidance <- vector()

for (i in 1:length(night_avoid$abundance)) {
  night_num_avoidance[i] <- NA
  night_num_avoidance[i] <- night_avoid$abundance[i]*0.372
}

night_avoid$night_num_avoidance <- night_num_avoidance

#### Putting new avoidance values into data frame ####

night_abundance_new <- vector(length = length(night_avoid$abundance))

for (i in 1:length(night_avoid$abundance)) {
  night_abundance_new[i] <- NA
  night_abundance_new[i] <- night_avoid$abundance[i] - night_avoid$night_num_avoidance[i]
}

night_avoid$night_abundance_new <- night_abundance_new

#### Creating new data frames for filtering ####

night_each_avoid <- night_avoid %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_avoided = sum(night_num_avoidance))

night_domain_avoid <- night_avoid %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

night_all_transects <- merge(night_each_avoid, night_domain_avoid, by = "Region_name")

division_function <- function(Sum_avoided, sum_region) {
  Sum_avoided/sum_region
}

#### Probabilities ####

night_all_prob <- division_function(night_all_transects$Sum_avoided, night_all_transects$Sum_region)

## Histograms

hist(night_all_prob, breaks = 100, xlab = "Probability", main = "Night Probabilities")
mean_night <- mean(night_all_prob)
abline(v = mean_night, col = "red")
