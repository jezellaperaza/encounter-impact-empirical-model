## Passive Avoidance
## 140 m by 25 m minus 140 m by 10 m 
## October 2022

#### 140 m by 25 m ####

setwd("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

library(tidyverse)
library(dplyr)

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)
area_zoi <- 140*25
area_avoid <- 140*10

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
  all_zoi$abundance[i] <- all_zoi$PRC_ABC[i]*area_zoi/sigma_bs_herring
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

#### 140 m by 10 m ####

#### Each bin is a potential ZOI ####

setwd("~/UW Summer 2022/EV Exports/Mobile/Avoidance")

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

all_zoi_2 <- load_data("~/UW Summer 2022/EV Exports/Mobile/Avoidance")

#### Obtaining abundances ####

for (i in 1:length(all_zoi_2$PRC_ABC)) {
  all_zoi_2$abundance[i] <- NA
  all_zoi_2$abundance[i] <- all_zoi_2$PRC_ABC[i]*area_avoid/sigma_bs_herring
}

#### Creating new data frames for filtering ####

each_zoi_2 <- all_zoi_2 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

domain_zoi_2 <- all_zoi_2 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

all_transects_2 <- merge(each_zoi_2, domain_zoi_2, by = "Region_name")

complete_transect <- merge(all_transects, all_transects_2, by = "Region_name")

#### Computing for passive avoidance ####

fish_passing <- vector()

for (i in 1:length(complete_transect$Region_name)) {
  fish_passing[i] <- NA
  fish_passing[i] <- complete_transect$Sum_region.x[i] - complete_transect$Sum_region.y[i]
}

complete_transect$fish_passing <- fish_passing

passing_probs <- complete_transect$fish_passing / complete_transect$Sum_region.x

hist(passing_probs)
mean_passing <- mean(passing_probs)
abline(v = mean_passing, col = "red")

## Day

#### 140 m by 25 m ####

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

day_pass <- load_data("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

day_pass <- day_pass %>% filter(Region_name != "GRID_B_N_NIGHT_6_04_12" &
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

#### Obtaining abundances ####

for (i in 1:length(day_pass$PRC_ABC)) {
  day_pass$abundance[i] <- NA
  day_pass$abundance[i] <- day_pass$PRC_ABC[i]*area_zoi/sigma_bs_herring
}

#### Creating new data frames for filtering ####

day_each_zoi <- day_pass %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

day_domain_zoi <- day_pass %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

day_all_transects <- merge(day_each_zoi, day_domain_zoi, by = "Region_name")

#### 140 m by 10 m ####

#### Each bin is a potential ZOI ####

setwd("~/UW Summer 2022/EV Exports/Mobile/Avoidance")

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

day_pass_2 <- load_data("~/UW Summer 2022/EV Exports/Mobile/Avoidance")

day_pass_2 <- day_pass_2 %>% filter(Region_name != "GRID_B_N_NIGHT_6_04_12" &
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

#### Obtaining abundances ####

for (i in 1:length(day_pass_2$PRC_ABC)) {
  day_pass_2$abundance[i] <- NA
  day_pass_2$abundance[i] <- day_pass_2$PRC_ABC[i]*area_avoid/sigma_bs_herring
}

#### Creating new data frames for filtering ####

day_each_zoi_2 <- day_pass_2 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

day_domain_zoi_2 <- day_pass_2 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

day_all_transects_2 <- merge(day_each_zoi_2, day_domain_zoi_2, by = "Region_name")

day_complete_transect <- merge(day_all_transects, day_all_transects_2, by = "Region_name")

#### Computing for passive avoidance ####

fish_passing <- vector()

for (i in 1:length(day_complete_transect$Region_name)) {
  fish_passing[i] <- NA
  fish_passing[i] <- day_complete_transect$Sum_region.x[i] - day_complete_transect$Sum_region.y[i]
}

day_complete_transect$fish_passing <- fish_passing

day_passing_probs <- day_complete_transect$fish_passing / day_complete_transect$Sum_region.x

hist(day_passing_probs)
day_mean_passing <- mean(day_passing_probs)
abline(v = day_mean_passing, col = "red")

## Night

#### 140 m by 25 m ####

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

night_pass <- load_data("~/UW Summer 2022/EV Exports/Mobile/Zone of Influence")

night_pass <- night_pass %>% filter(Region_name %in% c("GRID_B_N_NIGHT_6_04_12", 
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

#### Obtaining abundances ####

for (i in 1:length(night_pass$PRC_ABC)) {
  night_pass$abundance[i] <- NA
  night_pass$abundance[i] <- night_pass$PRC_ABC[i]*area_zoi/sigma_bs_herring
}

#### Creating new data frames for filtering ####

night_each_zoi <- night_pass %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

night_domain_zoi <- night_pass %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

night_all_transects <- merge(night_each_zoi, night_domain_zoi, by = "Region_name")

#### 140 m by 10 m ####

#### Each bin is a potential ZOI ####

setwd("~/UW Summer 2022/EV Exports/Mobile/Avoidance")

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

night_pass_2 <- load_data("~/UW Summer 2022/EV Exports/Mobile/Avoidance")

night_pass_2 <- night_pass_2 %>% filter(Region_name %in% c("GRID_B_N_NIGHT_6_04_12", 
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

#### Obtaining abundances ####

for (i in 1:length(night_pass_2$PRC_ABC)) {
  night_pass_2$abundance[i] <- NA
  night_pass_2$abundance[i] <- night_pass_2$PRC_ABC[i]*area_avoid/sigma_bs_herring
}

#### Creating new data frames for filtering ####

night_each_zoi_2 <- night_pass_2 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

night_domain_zoi_2 <- night_pass_2 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

#### Combine data frames and division function ####

night_all_transects_2 <- merge(night_each_zoi_2, night_domain_zoi_2, by = "Region_name")

night_complete_transect <- merge(night_all_transects, night_all_transects_2, by = "Region_name")

#### Computing for passive avoidance ####

fish_passing <- vector()

for (i in 1:length(night_complete_transect$Region_name)) {
  fish_passing[i] <- NA
  fish_passing[i] <- night_complete_transect$Sum_region.x[i] - night_complete_transect$Sum_region.y[i]
}

night_complete_transect$fish_passing <- fish_passing

night_passing_probs <- night_complete_transect$fish_passing / night_complete_transect$Sum_region.x

hist(night_passing_probs)
night_mean_passing <- mean(night_passing_probs)
abline(v = night_mean_passing, col = "red")
