### Script to obtain probabilities in each bin of each transect across all days
### Does not yet distinguish between day and night - TBD
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
  summarise(Sum_each = sum(abundance))

domain_abundance_region_30 <- all_acoustic_data_30 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

domain_abundance_30 <- all_acoustic_data_30 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

all_data_abundances_30 <- all_acoustic_data_30 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

## 60 m

each_transect_each_bin_60 <- all_acoustic_data_60 %>%
  group_by(Region_name, Dist_S) %>%
  summarise(Sum_each = sum(abundance))

domain_abundance_region_60 <- all_acoustic_data_60 %>%
  group_by(Region_name) %>%
  dplyr::summarise(Sum_region = sum(abundance))

domain_abundance_60 <- all_acoustic_data_60 %>%
  group_by(EV_filename) %>%
  dplyr::summarise(Sum = sum(abundance))

all_data_abundances_60 <- all_acoustic_data_60 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

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