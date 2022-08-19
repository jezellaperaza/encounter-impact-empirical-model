## Next steps: include separate day and night distributions
## August 2022

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

acoustic_data <- load_data("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")

#### Filter the bottom (60 m and 30 m) and for 2, 3, 4 transects ####

acoustic_data_30 <- acoustic_data %>% filter(Layer_depth_max <= 30)
acoustic_data_60 <- acoustic_data %>% filter(Layer_depth_max <= 60)

acoustic_data_30 <- acoustic_data_30 %>% filter(Region_name %in% c("GRID_B_N_6_03_02", "GRID_B_N_6_03_03", "GRID_B_N_6_03_04",
                                                                   "GRID_B_N_6_04_02", "GRID_B_N_6_04_03", "GRID_B_N_6_04_04",
                                                                   "GRID_B_N_NIGHT_6_04_02", "GRID_B_N_NIGHT_6_04_03", "GRID_B_N_NIGHT_6_04_04",
                                                                   "GRID_B_N_NIGHT_6_04_03", "GRID_B_N_6_06_04", "GRID_B_N_6_06_02", 
                                                                   "GRID_B_N_6_07_04", "GRID_B_N_6_07_03", "GRID_B_N_6_07_02",
                                                                   "GRID_B_N_6_08_02", "GRID_B_N_6_08_03", "GRID_B_N_6_08_04",
                                                                   "GRID_B_N_6_09_02", "GRID_B_N_6_09_03", "GRID_B_N_6_09_04",
                                                                   "GRID_B_N_REP_6_09_0", "GRID_B_N_REP_6_09_03", "GRID_B_N_REP_6_09_04",
                                                                   "GRID_B_N_NIGHT_6_09_02", "GRID_B_N_NIGHT_6_09_03", "GRID_B_N_NIGHT_6_09_04",
                                                                   "GRID_B_N_6_11_02", "GRID_B_N_6_11_03", "GRID_B_N_6_11_04",
                                                                   "GRID_B_N_REP_6_11_02", "GRID_B_N_REP_6_11_03", "GRID_B_N_REP_6_11_04",
                                                                   "GRID_B_N_NIGHT_6_11_02", "GRID_B_N_NIGHT_6_11_03", "GRID_B_N_NIGHT_6_11_04",
                                                                   "GRID_B_N_6_13_02", "GRID_B_N_6_13_03", "GRID_B_N_6_13_04",
                                                                   "GRID_B_N_REP_6_13_02", "GRID_B_N_REP_6_13_03", "GRID_B_N_REP_6_13_04",
                                                                   "Grid_A_N_5_02_02", "Grid_A_N_5_02_03", "Grid_A_N_5_02_04",
                                                                   "GRID_A_N_REP_5_04_02", "GRID_A_N_REP_5_04_03", "GRID_A_N_REP_5_04_04",
                                                                   "GRID_A_N_NIGHT_5_04_02", "GRID_A_N_NIGHT_5_04_03", "GRID_A_N_NIGHT_5_04_04",
                                                                   "GRID_A_N_5_06_02", "GRID_A_N_5_06_03", "GRID_A_N_5_06_04",
                                                                   "GRID_A_N_REP_5_06_02", "GRID_A_N_REP_5_06_03", "GRID_A_N_REP_5_06_04",
                                                                   "GRID_A_N_5_07_02", "GRID_A_N_5_07_03", "GRID_A_N_5_07_04",
                                                                   "GRID_A_N_REP_5_07_02", "GRID_A_N_REP_5_07_03", "GRID_A_N_REP_5_07_04",
                                                                   "GRID_A_N_NIGHT_5_07_02", "GRID_A_N_NIGHT_5_07_03", "GRID_A_N_NIGHT_5_07_04",
                                                                   "GRID_A_N_5_08_02", "GRID_A_N_5_08_03", "GRID_A_N_5_08_04",
                                                                   "GRID_A_N_5_09_04", "GRID_A_N_5_09_03", "GRID_A_N_5_09_02",
                                                                   "GRID_A_N_5_10_04", "GRID_A_N_5_10_03", "GRID_A_N_5_10_02",
                                                                   "GRID_A_N_5_13_0", "GRID_A_N_5_13_03"))

acoustic_data_60 <- acoustic_data_60 %>% filter(Region_name %in% c("GRID_B_N_6_03_02", "GRID_B_N_6_03_03", "GRID_B_N_6_03_04",
                                                                   "GRID_B_N_6_04_02", "GRID_B_N_6_04_03", "GRID_B_N_6_04_04",
                                                                   "GRID_B_N_NIGHT_6_04_02", "GRID_B_N_NIGHT_6_04_03", "GRID_B_N_NIGHT_6_04_04",
                                                                   "GRID_B_N_NIGHT_6_04_03", "GRID_B_N_6_06_04", "GRID_B_N_6_06_02", 
                                                                   "GRID_B_N_6_07_04", "GRID_B_N_6_07_03", "GRID_B_N_6_07_02",
                                                                   "GRID_B_N_6_08_02", "GRID_B_N_6_08_03", "GRID_B_N_6_08_04",
                                                                   "GRID_B_N_6_09_02", "GRID_B_N_6_09_03", "GRID_B_N_6_09_04",
                                                                   "GRID_B_N_REP_6_09_0", "GRID_B_N_REP_6_09_03", "GRID_B_N_REP_6_09_04",
                                                                   "GRID_B_N_NIGHT_6_09_02", "GRID_B_N_NIGHT_6_09_03", "GRID_B_N_NIGHT_6_09_04",
                                                                   "GRID_B_N_6_11_02", "GRID_B_N_6_11_03", "GRID_B_N_6_11_04",
                                                                   "GRID_B_N_REP_6_11_02", "GRID_B_N_REP_6_11_03", "GRID_B_N_REP_6_11_04",
                                                                   "GRID_B_N_NIGHT_6_11_02", "GRID_B_N_NIGHT_6_11_03", "GRID_B_N_NIGHT_6_11_04",
                                                                   "GRID_B_N_6_13_02", "GRID_B_N_6_13_03", "GRID_B_N_6_13_04",
                                                                   "GRID_B_N_REP_6_13_02", "GRID_B_N_REP_6_13_03", "GRID_B_N_REP_6_13_04",
                                                                   "Grid_A_N_5_02_02", "Grid_A_N_5_02_03", "Grid_A_N_5_02_04",
                                                                   "GRID_A_N_REP_5_04_02", "GRID_A_N_REP_5_04_03", "GRID_A_N_REP_5_04_04",
                                                                   "GRID_A_N_NIGHT_5_04_02", "GRID_A_N_NIGHT_5_04_03", "GRID_A_N_NIGHT_5_04_04",
                                                                   "GRID_A_N_5_06_02", "GRID_A_N_5_06_03", "GRID_A_N_5_06_04",
                                                                   "GRID_A_N_REP_5_06_02", "GRID_A_N_REP_5_06_03", "GRID_A_N_REP_5_06_04",
                                                                   "GRID_A_N_5_07_02", "GRID_A_N_5_07_03", "GRID_A_N_5_07_04",
                                                                   "GRID_A_N_REP_5_07_02", "GRID_A_N_REP_5_07_03", "GRID_A_N_REP_5_07_04",
                                                                   "GRID_A_N_NIGHT_5_07_02", "GRID_A_N_NIGHT_5_07_03", "GRID_A_N_NIGHT_5_07_04",
                                                                   "GRID_A_N_5_08_02", "GRID_A_N_5_08_03", "GRID_A_N_5_08_04",
                                                                   "GRID_A_N_5_09_04", "GRID_A_N_5_09_03", "GRID_A_N_5_09_02",
                                                                   "GRID_A_N_5_10_04", "GRID_A_N_5_10_03", "GRID_A_N_5_10_02",
                                                                   "GRID_A_N_5_13_0", "GRID_A_N_5_13_03"))


#### Getting the abundance of each transect and domains ####
## 30 m

for (i in 1:length(acoustic_data_30$PRC_ABC)) {
  acoustic_data_30$abundance[i] <- NA
  acoustic_data_30$abundance[i] <- acoustic_data_30$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## 60 m

for (i in 1:length(acoustic_data_60$PRC_ABC)) {
  acoustic_data_60$abundance[i] <- NA
  acoustic_data_60$abundance[i] <- acoustic_data_60$PRC_ABC[i]*transect_area/sigma_bs_herring
}

library(dplyr)

## 30 m

transect_bin_30 <- acoustic_data_30 %>%
  group_by(Region_name, Dist_S) %>%
  summarise(Sum_each = sum(abundance))

domain_abundance_region_30 <- acoustic_data_30 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

domain_abundance_30 <- acoustic_data_30 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

data_abundances_30 <- acoustic_data_30 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

## 60 m

transect_bin_60 <- acoustic_data_60 %>%
  group_by(Region_name, Dist_S) %>%
  summarise(Sum_each = sum(abundance))

domain_abundance_region_60 <- acoustic_data_60 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

domain_abundance_60 <- acoustic_data_60 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

data_abundances_60 <- acoustic_data_60 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### Combine data frames and division function ####

transects_30 <- merge(transect_bin_30, domain_abundance_region_30, by = "Region_name")
transects_60 <- merge(transect_bin_60, domain_abundance_region_60, by = "Region_name")

division_function <- function(sum_each, sum_region) {
  sum_each/sum_region
}

#### Probabilities ####

## 30 

two_four_30_prob <- division_function(transects_30$Sum_each, transects_30$Sum_region)

## 60 m

two_four_60_prob <- division_function(transects_60$Sum_each, transects_60$Sum_region)

## Histograms

## 30 m

weighted_data_30 <- as.data.frame(plyr::count(two_four_30_prob))
weight_mean_30 <- weighted.mean(weighted_data_30$x, weighted_data_30$freq)

hist(two_four_30_prob, breaks = 100)
abline(v = weight_mean_30, col = "red")

## 60 m

weighted_data_60 <- as.data.frame(plyr::count(two_four_60_prob))
weight_mean_60 <- weighted.mean(weighted_data_60$x, weighted_data_60$freq)

hist(two_four_60_prob, breaks = 100)
abline(v = weight_mean_60, col = "red")
