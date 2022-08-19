### Organizing data for simulation
### July 2022

library(tidyverse)
library(dplyr)
library(plyr)
library(readr)
library(here)

setwd("~/UW Summer 2022/EV Exports/Mobile/Transects Integrated by Depth/Turbulence Removed")
sim_dat <- read_csv("May_June_transects_turbulence_removed.csv")

transect_width <- 1500
transect_height <- 60
transect_area <- transect_width*transect_height

new_dat <- sim_dat[, c(2, 5, 24:25)]

transects_two <- new_dat[1:28,]
transects_three <- new_dat[29:56,]
transects_four <- new_dat[57:84,]

avg_herring_length_cm <- 15.5

TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)

ABC_two <- transects_two$ABC
ABC_three <- transects_three$ABC
ABC_four <- transects_four$ABC

two_vec <- vector(length = length(ABC_two))

for (i in 1:length(ABC_two)) {
  two_vec[i] <- ABC_two[i]*transect_area / sigma_bs_herring
}

three_vec <- vector(length = length(ABC_three))

for (i in 1:length(ABC_three)) {
  three_vec[i] <- ABC_three[i]*transect_area / sigma_bs_herring
}

four_vec <- vector(length = length(ABC_four))

for (i in 1:length(ABC_four)) {
  four_vec[i] <- ABC_four[i]*transect_area / sigma_bs_herring
}

all_vector_transects <- vector()
all_vector_transects <- c(two_vec, three_vec, four_vec)

weighted_vectors <- as.data.frame(plyr::count(all_vector_transects))
weight_vectors_all <- weighted.mean(weighted_vectors$x, weighted_vectors$freq)

hist(all_vector_transects)
abline(v = weight_vectors_all, col = "red")

a <- mean(two_vec)
b <- mean(three_vec)
c <- mean(four_vec)

mean(c(a, b, c))
