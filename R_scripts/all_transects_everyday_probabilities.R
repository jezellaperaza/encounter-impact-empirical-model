### Script to obtain probabilities in each bin of each transect across all days
### Treats every 15 meters to be the ZOI
### Abundance of each bin is divided by the total number of fish in entire transect domain
### August 2022

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")

library(tidyverse)

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)
transect_area <- 1500*60

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

all_acoustic_data <- load_data("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")

#### Filter the bottom (60 m and 30 m) ####

all_acoustic_data_30 <- all_acoustic_data %>% filter(Layer_depth_max <= 30)
all_acoustic_data_60 <- all_acoustic_data %>% filter(Layer_depth_max <= 60)

#### Getting the abundance of each transect and domains ####

## 30 m

for (i in 1:length(all_acoustic_data_30$PRC_ABC)) {
  all_acoustic_data_30$abundance[i] <- NA
  all_acoustic_data_30$abundance[i] <- all_acoustic_data_30$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## 60 m

for (i in 1:length(all_acoustic_data_60$PRC_ABC)) {
  all_acoustic_data_60$abundance[i] <- NA
  all_acoustic_data_60$abundance[i] <- all_acoustic_data_60$PRC_ABC[i]*transect_area/sigma_bs_herring
}

library(dplyr)

## 30 m

each_transect_each_bin_30 <- all_acoustic_data_30 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

domain_abundance_region_30 <- all_acoustic_data_30 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

domain_abundance_30 <- all_acoustic_data_30 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

all_data_abundances_30 <- all_acoustic_data_30 %>%
  group_by(Dist_S) %>%
  dplyr::summarise(Sum = sum(abundance))

## 60 m

each_transect_each_bin_60 <- all_acoustic_data_60 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

domain_abundance_region_60 <- all_acoustic_data_60 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

domain_abundance_60 <- all_acoustic_data_60 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

all_data_abundances_60 <- all_acoustic_data_60 %>%
  group_by(Dist_S) %>%
  dplyr::summarise(Sum = sum(abundance))

#### Combine data frames and division function ####

all_transects_30 <- merge(each_transect_each_bin_30, domain_abundance_region_30, by = "Region_name")
all_transects_60 <- merge(each_transect_each_bin_60, domain_abundance_region_60, by = "Region_name")

division_function <- function(sum_each, sum_region) {
  sum_each/sum_region
}

#### Probabilities ####

## 30 

all_30_prob <- division_function(all_transects_30$Sum_each, all_transects_30$Sum_region)
# all_transects_sum_30 <- sum(all_data_abundances_30$Sum)

## 60 m

all_60_prob <- division_function(all_transects_60$Sum_each, all_transects_60$Sum_region)
# all_transects_sum_60 <- sum(all_data_abundances_60$Sum)

## Histograms

## 30 m

weighted_data_30 <- as.data.frame(plyr::count(all_30_prob))
weight_mean_30 <- weighted.mean(weighted_data_30$x, weighted_data_30$freq)

hist(all_30_prob)
abline(v = weight_mean_30, col = "red")

## 60 m

weighted_data_60 <- as.data.frame(plyr::count(all_60_prob))
weight_mean_60 <- weighted.mean(weighted_data_60$x, weighted_data_60$freq)

hist(all_60_prob)
abline(v = weight_mean_60, col = "red")

#### Day ####

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

all_acoustic_data <- load_data("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")

#### Filter the bottom (60 m and 30 m) and transects ####

day_all_acoustic_data_30 <- all_acoustic_data %>% filter(Layer_depth_max <= 30)
day_all_acoustic_data_60 <- all_acoustic_data %>% filter(Layer_depth_max <= 60)

day_all_acoustic_data_30 <- day_all_acoustic_data_30 %>% filter(Region_name != "GRID_B_N_NIGHT_6_04_12" &
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

day_all_acoustic_data_60 <- day_all_acoustic_data_60 %>% filter(Region_name != "GRID_B_N_NIGHT_6_04_12" &
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

## 30 m

for (i in 1:length(day_all_acoustic_data_30$PRC_ABC)) {
  day_all_acoustic_data_30$abundance[i] <- NA
  day_all_acoustic_data_30$abundance[i] <- day_all_acoustic_data_30$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## 60 m

for (i in 1:length(day_all_acoustic_data_60$PRC_ABC)) {
  day_all_acoustic_data_60$abundance[i] <- NA
  day_all_acoustic_data_60$abundance[i] <- day_all_acoustic_data_60$PRC_ABC[i]*transect_area/sigma_bs_herring
}

library(dplyr)

## 30 m

day_each_transect_each_bin_30 <- day_all_acoustic_data_30 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

day_domain_abundance_region_30 <- day_all_acoustic_data_30 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

day_domain_abundance_30 <- day_all_acoustic_data_30 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

day_all_data_abundances_30 <- day_all_acoustic_data_30 %>%
  group_by(Dist_S) %>%
  dplyr::summarise(Sum = sum(abundance))

## 60 m

day_each_transect_each_bin_60 <- day_all_acoustic_data_60 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

day_domain_abundance_region_60 <- day_all_acoustic_data_60 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

day_domain_abundance_60 <- day_all_acoustic_data_60 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

day_all_data_abundances_60 <- day_all_acoustic_data_60 %>%
  group_by(Dist_S) %>%
  dplyr::summarise(Sum = sum(abundance))

#### Combine data frames and division function ####

day_all_transects_30 <- merge(day_each_transect_each_bin_30, day_domain_abundance_region_30, by = "Region_name")
day_all_transects_60 <- merge(day_each_transect_each_bin_60, day_domain_abundance_region_60, by = "Region_name")

division_function <- function(sum_each, sum_region) {
  sum_each/sum_region
}

#### Probabilities ####

## 30 

day_all_30_prob <- division_function(day_all_transects_30$Sum_each, day_all_transects_30$Sum_region)
# all_transects_sum_30 <- sum(all_data_abundances_30$Sum)

## 60 m

day_all_60_prob <- division_function(day_all_transects_60$Sum_each, day_all_transects_60$Sum_region)
# all_transects_sum_60 <- sum(all_data_abundances_60$Sum)

## Histograms

## 30 m

day_weighted_data_30 <- as.data.frame(plyr::count(day_all_30_prob))
day_weight_mean_30 <- weighted.mean(day_weighted_data_30$x, day_weighted_data_30$freq)

hist(day_all_30_prob, breaks = 100)
abline(v = day_weight_mean_30, col = "red")

## 60 m

day_weighted_data_60 <- as.data.frame(plyr::count(day_all_60_prob))
day_weight_mean_60 <- weighted.mean(day_weighted_data_60$x, day_weighted_data_60$freq)

hist(day_all_60_prob, breaks = 100)
abline(v = day_weight_mean_60, col = "red")


#### Night ####

#### Read in data ####

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

all_acoustic_data <- load_data("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")

#### Filter the bottom (60 m and 30 m) and transects ####

night_all_acoustic_data_30 <- all_acoustic_data %>% filter(Layer_depth_max <= 30)
night_all_acoustic_data_60 <- all_acoustic_data %>% filter(Layer_depth_max <= 60)

night_all_acoustic_data_30 <- night_all_acoustic_data_30 %>% filter(Region_name %in% c("GRID_B_N_NIGHT_6_04_12", 
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

night_all_acoustic_data_60 <- night_all_acoustic_data_60 %>% filter(Region_name %in% c("GRID_B_N_NIGHT_6_04_12", 
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

## 30 m

for (i in 1:length(night_all_acoustic_data_30$PRC_ABC)) {
  night_all_acoustic_data_30$abundance[i] <- NA
  night_all_acoustic_data_30$abundance[i] <- night_all_acoustic_data_30$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## 60 m

for (i in 1:length(night_all_acoustic_data_60$PRC_ABC)) {
  night_all_acoustic_data_60$abundance[i] <- NA
  night_all_acoustic_data_60$abundance[i] <- night_all_acoustic_data_60$PRC_ABC[i]*transect_area/sigma_bs_herring
}

library(dplyr)

## 30 m

night_each_transect_each_bin_30 <- night_all_acoustic_data_30 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

night_domain_abundance_region_30 <- night_all_acoustic_data_30 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

night_domain_abundance_30 <- night_all_acoustic_data_30 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

night_all_data_abundances_30 <- night_all_acoustic_data_30 %>%
  group_by(Dist_S) %>%
  dplyr::summarise(Sum = sum(abundance))

## 60 m

night_each_transect_each_bin_60 <- night_all_acoustic_data_60 %>%
  group_by(Region_name, Dist_S) %>%
  dplyr::summarise(Sum_each = sum(abundance))

night_domain_abundance_region_60 <- night_all_acoustic_data_60 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

night_domain_abundance_60 <- night_all_acoustic_data_60 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

night_all_data_abundances_60 <- night_all_acoustic_data_60 %>%
  group_by(Dist_S) %>%
  dplyr::summarise(Sum = sum(abundance))

#### Combine data frames and division function ####

night_all_transects_30 <- merge(night_each_transect_each_bin_30, night_domain_abundance_region_30, by = "Region_name")
night_all_transects_60 <- merge(night_each_transect_each_bin_60, night_domain_abundance_region_60, by = "Region_name")

division_function <- function(sum_each, sum_region) {
  sum_each/sum_region
}

#### Probabilities ####

## 30 

night_all_30_prob <- division_function(night_all_transects_30$Sum_each, night_all_transects_30$Sum_region)
# all_transects_sum_30 <- sum(all_data_abundances_30$Sum)

## 60 m

night_all_60_prob <- division_function(night_all_transects_60$Sum_each, night_all_transects_60$Sum_region)
# all_transects_sum_60 <- sum(all_data_abundances_60$Sum)

## Histograms

## 30 m

night_weighted_data_30 <- as.data.frame(plyr::count(night_all_30_prob))
night_weight_mean_30 <- weighted.mean(night_weighted_data_30$x, night_weighted_data_30$freq)

hist(night_all_30_prob, breaks = 100)
abline(v = night_weight_mean_30, col = "red")

## 60 m

night_weighted_data_60 <- as.data.frame(plyr::count(night_all_60_prob))
night_weight_mean_60 <- weighted.mean(night_weighted_data_60$x, night_weighted_data_60$freq)

hist(night_all_60_prob, breaks = 100)
abline(v = night_weight_mean_60, col = "red")
