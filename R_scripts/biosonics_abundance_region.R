### BioSonics abundance per region (every 12 minutes = 2 hours)
### 70 dB threshold
### August 2022

library(tidyverse)
library(timeDate)

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)

setwd('~/UW Summer 2022/EV Exports/BioSonics')
biosonics_region <- read.csv("NOPP_By_Region_Bins_Aug2022.csv")

bio_Sv <- biosonics_region$Sv_mean

sv_abundance <- function(SV) {
  lil_sv <- 10^(SV/10)
  abundance <- lil_sv/sigma_bs_herring
  return(abundance)
}

bio_Sv_new <- sv_abundance(bio_Sv)
bio_abundance <- bio_Sv_new/sigma_bs_herring

weighted_bio <- as.data.frame(plyr::count(bio_abundance))
weight_bio <- weighted.mean(weighted_bio$x, weighted_bio$freq)

# All

hist(bio_abundance)
abline(v = weight_bio, col = "red")

hour_by_sv <- biosonics_region %>% group_by(Region_name) %>% dplyr::summarise(Sv_mean)
hour_by_sv$abundance <- NA
hour_by_sv$abundance <- bio_abundance

Time <- timeDate(paste(biosonics_region$Time_M), format="%H")
hour_by_sv$Hour <- paste(hours(as.numeric(Time)))

hour_by_sv['Day_Night'] <- NA

hour_by_sv <- hour_by_sv %>% mutate(Day_Night = 
                                      case_when(Hour >= 21 & Hour < 6 ~ "Night",
                                                Hour == 2 ~ "Night",
                                                Hour == 0 ~ "Night",
                                                is.na(Day_Night) ~ "Day"))

# Day

hour_by_sv_day <- hour_by_sv %>% 
  filter(Day_Night == "Day")

weighted_day <- as.data.frame(plyr::count(hour_by_sv_day$abundance))
weight_day <- weighted.mean(weighted_day$x, weighted_day$freq)

hist(hour_by_sv_day$abundance)
abline(v = weight_day, col = "red")

# Night

hour_by_sv_night <- hour_by_sv %>% 
  filter(Day_Night == "Night")

weighted_night <- as.data.frame(plyr::count(hour_by_sv_night$abundance))
weight_night <- weighted.mean(weighted_night$x, weighted_night$freq)

hist(hour_by_sv_night$abundance)
abline(v = weight_night, col = "red")