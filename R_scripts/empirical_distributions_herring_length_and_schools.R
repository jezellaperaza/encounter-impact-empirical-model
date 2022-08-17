## Distributions
## August 2022

library(plyr)
library(tidyverse)

#### Fish length distribution ####

setwd("~/UW Summer 2022/R Scripts")
herring_lengths <- read.csv("Herring_lengths.csv")

plot(ecdf(herring_lengths$Length))

vector_lengths <- herring_lengths$Length
mean_length <- mean(vector_lengths)

hist(herring_lengths$Length, breaks = 10)
abline(v = mean_length, col = "red")

# weighted_lengths <- as.data.frame(count(herring_lengths$Length))
# weighted.mean(weighted_lengths$x, weighted_lengths$freq)

#### Default school distribution - INDIVIDUALLY ####

setwd("~/UW Summer 2022/R Scripts")
school_default <- read.csv("Schools_detection_default.csv")

### All

## Weighted mean

weighted_data_all <- as.data.frame(count(school_default$Total.number.of.schools))
weight_mean_all <- weighted.mean(weighted_data_all$x, weighted_data_all$freq)

## Plot

plot(ecdf(school_default$Total.number.of.schools))
hist(school_default$Total.number.of.schools)
abline(v = weight_mean_all, col = "red")

### Day

day_default <- school_default %>% filter(Type != "Night")

## Weighted mean

weighted_data_day <- as.data.frame(plyr::count(day_default$Total.number.of.schools))
weight_mean_day <- weighted.mean(weighted_data_day$x, weighted_data_day$freq)

## Plot

plot(ecdf(day_default$Total.number.of.schools))
hist(day_default$Total.number.of.schools)
abline(v = weight_mean_day, col = "red")

### Night

night_default <- school_default %>% filter(Type != "Day" & Type != "Repeat")

## Weighted mean

weighted_data_night <- as.data.frame(plyr::count(night_default$Total.number.of.schools))
weight_mean_night <- weighted.mean(weighted_data_night$x, weighted_data_night$freq)

## Plot

plot(ecdf(night_default$Total.number.of.schools))
hist(night_default$Total.number.of.schools)
abline(v = weight_mean_night, col = "red")

#### Default school distribution - BY TRANSECT & DAILY ####

setwd("~/UW Summer 2022/R Scripts")
school_default <- read.csv("Schools_detection_default.csv")

hist(school_default$Total)

## Daily

schools_daily <- c(8, 10, 13, 11, 2, 1, 1, 4, 9, 21, 33, 4, 2, 39, 33, 22)
hist(schools_daily, breaks = 10)

## Transect 2 Day

transect_2 <- school_default %>% filter(Transect == 2) %>%
  filter(Type == "Day" | Type == "Repeat")

hist(transect_2$Total.number.of.schools)

## Transect 3 Day

transect_3 <- school_default %>% filter(Transect == 3) %>%
  filter(Type == "Day" | Type == "Repeat")

hist(transect_3$Total.number.of.schools)

## Transect 4 Day

transect_4 <- school_default %>% filter(Transect == 4) %>%
  filter(Type == "Day" | Type == "Repeat")

hist(transect_4$Total.number.of.schools)

## Transect 2 Night

transect_2_night <- school_default %>% filter(Transect == 2) %>%
  filter(Type == "Night")

hist(transect_2_night$Total.number.of.schools)

## Transect 3 Night

transect_3_night <- school_default %>% filter(Transect == 3) %>%
  filter(Type == "Night")

hist(transect_3_night$Total.number.of.schools)

## Transect 4 Night

transect_4_night <- school_default %>% filter(Transect == 4) %>%
  filter(Type == "Night")

hist(transect_4_night$Total.number.of.schools)
