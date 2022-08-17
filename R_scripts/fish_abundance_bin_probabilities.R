#### Probability distribution for each bin (30 m deep & 15 m across)
#### August 2022

library(dplyr)

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)
transect_area <- 60*1500

#### Filtering

#### May 02 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_02_full <- read.csv("May_02_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_02_02 <- May_02_full[May_02_full$Region_name == "Grid_A_N_5_02_02",]
May_02_03 <- May_02_full[May_02_full$Region_name == "Grid_A_N_5_02_03",]
May_02_04 <- May_02_full[May_02_full$Region_name == "Grid_A_N_5_02_04",]

## filter to not include above 60 m

May_02_02 <- May_02_02 %>% filter(Layer_depth_max <= 30)
May_02_03 <- May_02_03 %>% filter(Layer_depth_max <= 30)
May_02_04 <- May_02_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(May_02_02$PRC_ABC)) {
  May_02_02$abundance[i] <- NA
  May_02_02$abundance[i] <- May_02_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(May_02_03$PRC_ABC)) {
  May_02_03$abundance[i] <- NA
  May_02_03$abundance[i] <- May_02_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(May_02_04$PRC_ABC)) {
  May_02_04$abundance[i] <- NA
  May_02_04$abundance[i] <- May_02_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### May 04 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_04_full <- read.csv("May_04_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_04_02 <- May_04_full[May_04_full$Region_name == "GRID_A_N_5_04_02",]
May_04_03 <- May_04_full[May_04_full$Region_name == "GRID_A_N_5_04_03",]
May_04_04 <- May_04_full[May_04_full$Region_name == "GRID_A_N_5_04_04",]

May_04_02_rep <- May_04_full[May_04_full$Region_name == "GRID_A_N_REP_5_04_02",]
May_04_03_rep <- May_04_full[May_04_full$Region_name == "GRID_A_N_REP_5_04_03",]
May_04_04_rep <- May_04_full[May_04_full$Region_name == "GRID_A_N_REP_5_04_04",]

May_04_02_night <- May_04_full[May_04_full$Region_name == "GRID_A_N_NIGHT_5_04_02",]
May_04_03_night <- May_04_full[May_04_full$Region_name == "GRID_A_N_NIGHT_5_04_03",]
May_04_04_night <- May_04_full[May_04_full$Region_name == "GRID_A_N_NIGHT_5_04_04",]

## filter to not include above 60 m

May_04_02 <- May_04_02 %>% filter(Layer_depth_max <= 30)
May_04_03 <- May_04_03 %>% filter(Layer_depth_max <= 30)
May_04_04 <- May_04_04 %>% filter(Layer_depth_max <= 30)

May_04_02_rep <- May_04_02_rep %>% filter(Layer_depth_max <= 30)
May_04_03_rep <- May_04_03_rep %>% filter(Layer_depth_max <= 30)
May_04_04_rep <- May_04_04_rep %>% filter(Layer_depth_max <= 30)

May_04_02_night <- May_04_02_night %>% filter(Layer_depth_max <= 30)
May_04_03_night <- May_04_03_night %>% filter(Layer_depth_max <= 30)
May_04_04_night <- May_04_04_night %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(May_04_02$PRC_ABC)) {
  May_04_02$abundance[i] <- NA
  May_04_02$abundance[i] <- May_04_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(May_04_03$PRC_ABC)) {
  May_04_03$abundance[i] <- NA
  May_04_03$abundance[i] <- May_04_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(May_04_04$PRC_ABC)) {
  May_04_04$abundance[i] <- NA
  May_04_04$abundance[i] <- May_04_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 2

for (i in 1:length(May_04_02_rep$PRC_ABC)) {
  May_04_02_rep$abundance[i] <- NA
  May_04_02_rep$abundance[i] <- May_04_02_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 3

for (i in 1:length(May_04_03_rep$PRC_ABC)) {
  May_04_03_rep$abundance[i] <- NA
  May_04_03_rep$abundance[i] <- May_04_03_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 4

for (i in 1:length(May_04_04_rep$PRC_ABC)) {
  May_04_04_rep$abundance[i] <- NA
  May_04_04_rep$abundance[i] <- May_04_04_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 2

for (i in 1:length(May_04_02_night$PRC_ABC)) {
  May_04_02_night$abundance[i] <- NA
  May_04_02_night$abundance[i] <- May_04_02_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 3

for (i in 1:length(May_04_03_night$PRC_ABC)) {
  May_04_03_night$abundance[i] <- NA
  May_04_03_night$abundance[i] <- May_04_03_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 4

for (i in 1:length(May_04_04_night$PRC_ABC)) {
  May_04_04_night$abundance[i] <- NA
  May_04_04_night$abundance[i] <- May_04_04_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### May 06 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_06_full <- read.csv("May_06_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_06_02 <- May_06_full[May_06_full$Region_name == "GRID_A_N_5_06_02",]
May_06_03 <- May_06_full[May_06_full$Region_name == "GRID_A_N_5_06_03",]
May_06_04 <- May_06_full[May_06_full$Region_name == "GRID_A_N_5_06_04",]

May_06_02_rep <- May_06_full[May_06_full$Region_name == "GRID_A_N_REP_5_06_02",]
May_06_03_rep <- May_06_full[May_06_full$Region_name == "GRID_A_N_REP_5_06_03",]
May_06_04_rep <- May_06_full[May_06_full$Region_name == "GRID_A_N_REP_5_06_04",]

## filter to not include above 60 m

May_06_02 <- May_06_02 %>% filter(Layer_depth_max <= 30)
May_06_03 <- May_06_03 %>% filter(Layer_depth_max <= 30)
May_06_04 <- May_06_04 %>% filter(Layer_depth_max <= 30)

May_06_02_rep <- May_06_02_rep %>% filter(Layer_depth_max <= 30)
May_06_03_rep <- May_06_03_rep %>% filter(Layer_depth_max <= 30)
May_06_04_rep <- May_06_04_rep %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(May_06_02$PRC_ABC)) {
  May_06_02$abundance[i] <- NA
  May_06_02$abundance[i] <- May_06_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(May_06_03$PRC_ABC)) {
  May_06_03$abundance[i] <- NA
  May_06_03$abundance[i] <- May_06_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(May_06_04$PRC_ABC)) {
  May_06_04$abundance[i] <- NA
  May_06_04$abundance[i] <- May_06_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 2

for (i in 1:length(May_06_02_rep$PRC_ABC)) {
  May_06_02_rep$abundance[i] <- NA
  May_06_02_rep$abundance[i] <- May_06_02_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 3

for (i in 1:length(May_06_03_rep$PRC_ABC)) {
  May_06_03_rep$abundance[i] <- NA
  May_06_03_rep$abundance[i] <- May_06_03_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 4

for (i in 1:length(May_06_04_rep$PRC_ABC)) {
  May_06_04_rep$abundance[i] <- NA
  May_06_04_rep$abundance[i] <- May_06_04_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### May 07 ####

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_07_full <- read.csv("May_07_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_07_02 <- May_07_full[May_07_full$Region_name == "GRID_A_N_5_07_02",]
May_07_03 <- May_07_full[May_07_full$Region_name == "GRID_A_N_5_07_03",]
May_07_04 <- May_07_full[May_07_full$Region_name == "GRID_A_N_5_07_04",]

May_07_02_rep <- May_07_full[May_07_full$Region_name == "GRID_A_N_REP_5_07_02",]
May_07_03_rep <- May_07_full[May_07_full$Region_name == "GRID_A_N_REP_5_07_03",]
May_07_04_rep <- May_07_full[May_07_full$Region_name == "GRID_A_N_REP_5_07_04",]

May_07_02_night <- May_07_full[May_07_full$Region_name == "GRID_A_N_NIGHT_5_07_02",]
May_07_03_night <- May_07_full[May_07_full$Region_name == "GRID_A_N_NIGHT_5_07_03",]
May_07_04_night <- May_07_full[May_07_full$Region_name == "GRID_A_N_NIGHT_5_07_04",]

## filter to not include above 60 m

May_07_02 <- May_07_02 %>% filter(Layer_depth_max <= 30)
May_07_03 <- May_07_03 %>% filter(Layer_depth_max <= 30)
May_07_04 <- May_07_04 %>% filter(Layer_depth_max <= 30)

May_07_02_rep <- May_07_02_rep %>% filter(Layer_depth_max <= 30)
May_07_03_rep <- May_07_03_rep %>% filter(Layer_depth_max <= 30)
May_07_04_rep <- May_07_04_rep %>% filter(Layer_depth_max <= 30)

May_07_02_night <- May_07_02_night %>% filter(Layer_depth_max <= 30)
May_07_03_night <- May_07_03_night %>% filter(Layer_depth_max <= 30)
May_07_04_night <- May_07_04_night %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(May_07_02$PRC_ABC)) {
  May_07_02$abundance[i] <- NA
  May_07_02$abundance[i] <- May_07_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(May_07_03$PRC_ABC)) {
  May_07_03$abundance[i] <- NA
  May_07_03$abundance[i] <- May_07_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(May_07_04$PRC_ABC)) {
  May_07_04$abundance[i] <- NA
  May_07_04$abundance[i] <- May_07_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 2

for (i in 1:length(May_07_02_rep$PRC_ABC)) {
  May_07_02_rep$abundance[i] <- NA
  May_07_02_rep$abundance[i] <- May_07_02_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 3

for (i in 1:length(May_07_03_rep$PRC_ABC)) {
  May_07_03_rep$abundance[i] <- NA
  May_07_03_rep$abundance[i] <- May_07_03_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 4

for (i in 1:length(May_07_04_rep$PRC_ABC)) {
  May_07_04_rep$abundance[i] <- NA
  May_07_04_rep$abundance[i] <- May_07_04_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 2

for (i in 1:length(May_07_02_night$PRC_ABC)) {
  May_07_02_night$abundance[i] <- NA
  May_07_02_night$abundance[i] <- May_07_02_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 3

for (i in 1:length(May_07_03_night$PRC_ABC)) {
  May_07_03_night$abundance[i] <- NA
  May_07_03_night$abundance[i] <- May_07_03_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 4

for (i in 1:length(May_07_04_night$PRC_ABC)) {
  May_07_04_night$abundance[i] <- NA
  May_07_04_night$abundance[i] <- May_07_04_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### May 08 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_08_full <- read.csv("May_08_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_08_02 <- May_08_full[May_08_full$Region_name == "GRID_A_N_5_08_02",]
May_08_03 <- May_08_full[May_08_full$Region_name == "GRID_A_N_5_08_03",]
May_08_04 <- May_08_full[May_08_full$Region_name == "GRID_A_N_5_08_04",]

## filter to not include above 60 m

May_08_02 <- May_08_02 %>% filter(Layer_depth_max <= 30)
May_08_03 <- May_08_03 %>% filter(Layer_depth_max <= 30)
May_08_04 <- May_08_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(May_08_02$PRC_ABC)) {
  May_08_02$abundance[i] <- NA
  May_08_02$abundance[i] <- May_08_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(May_08_03$PRC_ABC)) {
  May_08_03$abundance[i] <- NA
  May_08_03$abundance[i] <- May_08_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(May_08_04$PRC_ABC)) {
  May_08_04$abundance[i] <- NA
  May_08_04$abundance[i] <- May_08_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### May 09 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_09_full <- read.csv("May_09_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_09_02 <- May_09_full[May_09_full$Region_name == "GRID_A_N_5_09_02",]
May_09_03 <- May_09_full[May_09_full$Region_name == "GRID_A_N_5_09_03",]
May_09_04 <- May_09_full[May_09_full$Region_name == "GRID_A_N_5_09_04",]

## filter to not include above 60 m

May_09_02 <- May_09_02 %>% filter(Layer_depth_max <= 30)
May_09_03 <- May_09_03 %>% filter(Layer_depth_max <= 30)
May_09_04 <- May_09_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(May_09_02$PRC_ABC)) {
  May_09_02$abundance[i] <- NA
  May_09_02$abundance[i] <- May_09_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(May_09_03$PRC_ABC)) {
  May_09_03$abundance[i] <- NA
  May_09_03$abundance[i] <- May_09_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(May_09_04$PRC_ABC)) {
  May_09_04$abundance[i] <- NA
  May_09_04$abundance[i] <- May_08_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### May 10 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_10_full <- read.csv("May_10_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_10_02 <- May_10_full[May_10_full$Region_name == "GRID_A_N_5_10_02",]
May_10_03 <- May_10_full[May_10_full$Region_name == "GRID_A_N_5_10_03",]
May_10_04 <- May_10_full[May_10_full$Region_name == "GRID_A_N_5_10_04",]

## filter to not include above 60 m

May_10_02 <- May_10_02 %>% filter(Layer_depth_max <= 30)
May_10_03 <- May_10_03 %>% filter(Layer_depth_max <= 30)
May_10_04 <- May_10_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(May_10_02$PRC_ABC)) {
  May_10_02$abundance[i] <- NA
  May_10_02$abundance[i] <- May_10_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(May_10_03$PRC_ABC)) {
  May_10_03$abundance[i] <- NA
  May_10_03$abundance[i] <- May_10_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(May_10_04$PRC_ABC)) {
  May_10_04$abundance[i] <- NA
  May_10_04$abundance[i] <- May_08_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### May 13 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_13_full <- read.csv("May_13_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_13_02 <- May_13_full[May_13_full$Region_name == "GRID_A_N_5_13_0",]
May_13_03 <- May_13_full[May_13_full$Region_name == "GRID_A_N_5_13_03",]
May_13_04 <- May_13_full[May_13_full$Region_name == "GRID_A_N_5_13_04",]

## filter to not include above 60 m

May_13_02 <- May_13_02 %>% filter(Layer_depth_max <= 30)
May_13_03 <- May_13_03 %>% filter(Layer_depth_max <= 30)
May_13_04 <- May_13_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(May_13_02$PRC_ABC)) {
  May_13_02$abundance[i] <- NA
  May_13_02$abundance[i] <- May_13_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(May_13_03$PRC_ABC)) {
  May_13_03$abundance[i] <- NA
  May_13_03$abundance[i] <- May_13_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(May_13_04$PRC_ABC)) {
  May_13_04$abundance[i] <- NA
  May_13_04$abundance[i] <- May_08_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### June 03 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
June_03_full <- read.csv("June_03_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

June_03_02 <- June_03_full[June_03_full$Region_name == "GRID_B_N_6_03_02",]
June_03_03 <- June_03_full[June_03_full$Region_name == "GRID_B_N_6_03_03",]
June_03_04 <- June_03_full[June_03_full$Region_name == "GRID_B_N_6_03_04",]

## filter to not include above 60 m

June_03_02 <- June_03_02 %>% filter(Layer_depth_max <= 30)
June_03_03 <- June_03_03 %>% filter(Layer_depth_max <= 30)
June_03_04 <- June_03_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(June_03_02$PRC_ABC)) {
  June_03_02$abundance[i] <- NA
  June_03_02$abundance[i] <- June_03_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(June_03_03$PRC_ABC)) {
  June_03_03$abundance[i] <- NA
  June_03_03$abundance[i] <- June_03_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(June_03_04$PRC_ABC)) {
  June_03_04$abundance[i] <- NA
  June_03_04$abundance[i] <- June_03_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### June 04 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
June_04_full <- read.csv("June_04_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

June_04_02 <- June_04_full[June_04_full$Region_name == "GRID_B_N_6_04_02",]
June_04_03 <- June_04_full[June_04_full$Region_name == "GRID_B_N_6_04_03",]
June_04_04 <- June_04_full[June_04_full$Region_name == "GRID_B_N_6_04_04",]

June_04_02_night <- June_04_full[June_04_full$Region_name == "GRID_B_N_NIGHT_6_04_02",]
June_04_03_night <- June_04_full[June_04_full$Region_name == "GRID_B_N_NIGHT_6_04_03",]
June_04_04_night <- June_04_full[June_04_full$Region_name == "GRID_B_N_NIGHT_6_04_04",]

## filter to not include above 60 m

June_04_02 <- June_04_02 %>% filter(Layer_depth_max <= 30)
June_04_03 <- June_04_03 %>% filter(Layer_depth_max <= 30)
June_04_04 <- June_04_04 %>% filter(Layer_depth_max <= 30)

June_04_02_night <- June_04_02_night %>% filter(Layer_depth_max <= 30)
June_04_03_night <- June_04_03_night %>% filter(Layer_depth_max <= 30)
June_04_04_night <- June_04_04_night %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(June_04_02$PRC_ABC)) {
  June_04_02$abundance[i] <- NA
  June_04_02$abundance[i] <- June_04_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(June_04_03$PRC_ABC)) {
  June_04_03$abundance[i] <- NA
  June_04_03$abundance[i] <- June_04_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(June_04_04$PRC_ABC)) {
  June_04_04$abundance[i] <- NA
  June_04_04$abundance[i] <- June_04_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 2

for (i in 1:length(June_04_02_night$PRC_ABC)) {
  June_04_02_night$abundance[i] <- NA
  June_04_02_night$abundance[i] <- June_04_02_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 3

for (i in 1:length(June_04_03_night$PRC_ABC)) {
  June_04_03_night$abundance[i] <- NA
  June_04_03_night$abundance[i] <- June_04_03_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 4

for (i in 1:length(June_04_04_night$PRC_ABC)) {
  June_04_04_night$abundance[i] <- NA
  June_04_04_night$abundance[i] <- June_04_04_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### June 06 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
June_06_full <- read.csv("June_06_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

June_06_02 <- June_06_full[June_06_full$Region_name == "GRID_B_N_6_06_02",]
June_06_03 <- June_06_full[June_06_full$Region_name == "GRID_B_N_6_06_03",]
June_06_04 <- June_06_full[June_06_full$Region_name == "GRID_B_N_6_06_04",]

## filter to not include above 60 m

June_06_02 <- June_06_02 %>% filter(Layer_depth_max <= 30)
June_06_03 <- June_06_03 %>% filter(Layer_depth_max <= 30)
June_06_04 <- June_06_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(June_06_02$PRC_ABC)) {
  June_06_02$abundance[i] <- NA
  June_06_02$abundance[i] <- June_06_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(June_06_03$PRC_ABC)) {
  June_06_03$abundance[i] <- NA
  June_06_03$abundance[i] <- June_06_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(June_06_04$PRC_ABC)) {
  June_06_04$abundance[i] <- NA
  June_06_04$abundance[i] <- June_06_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### June 07 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
June_07_full <- read.csv("June_07_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

June_07_02 <- June_07_full[June_07_full$Region_name == "GRID_B_N_6_07_02",]
June_07_03 <- June_07_full[June_07_full$Region_name == "GRID_B_N_6_07_03",]
June_07_04 <- June_07_full[June_07_full$Region_name == "GRID_B_N_6_07_04",]

## filter to not include above 60 m

June_07_02 <- June_07_02 %>% filter(Layer_depth_max <= 30)
June_07_03 <- June_07_03 %>% filter(Layer_depth_max <= 30)
June_07_04 <- June_07_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(June_07_02$PRC_ABC)) {
  June_07_02$abundance[i] <- NA
  June_07_02$abundance[i] <- June_07_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(June_07_03$PRC_ABC)) {
  June_07_03$abundance[i] <- NA
  June_07_03$abundance[i] <- June_07_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(June_07_04$PRC_ABC)) {
  June_07_04$abundance[i] <- NA
  June_07_04$abundance[i] <- June_07_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### June 08 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
June_08_full <- read.csv("June_08_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

June_08_02 <- June_08_full[June_08_full$Region_name == "GRID_B_N_6_08_02",]
June_08_03 <- June_08_full[June_08_full$Region_name == "GRID_B_N_6_08_03",]
June_08_04 <- June_08_full[June_08_full$Region_name == "GRID_B_N_6_08_04",]

## filter to not include above 60 m

June_08_02 <- June_08_02 %>% filter(Layer_depth_max <= 30)
June_08_03 <- June_08_03 %>% filter(Layer_depth_max <= 30)
June_08_04 <- June_08_04 %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(June_08_02$PRC_ABC)) {
  June_08_02$abundance[i] <- NA
  June_08_02$abundance[i] <- June_08_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(June_08_03$PRC_ABC)) {
  June_08_03$abundance[i] <- NA
  June_08_03$abundance[i] <- June_08_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(June_08_04$PRC_ABC)) {
  June_08_04$abundance[i] <- NA
  June_08_04$abundance[i] <- June_08_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### June 09 ####

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
June_09_full <- read.csv("June_09_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

June_09_02 <- June_09_full[June_09_full$Region_name == "GRID_B_N_6_09_02",]
June_09_03 <- June_09_full[June_09_full$Region_name == "GRID_B_N_6_09_03",]
June_09_04 <- June_09_full[June_09_full$Region_name == "GRID_B_N_6_09_04",]

June_09_02_rep <- June_09_full[June_09_full$Region_name == "GRID_B_N_REP_6_09_0",]
June_09_03_rep <- June_09_full[June_09_full$Region_name == "GRID_B_N_REP_6_09_03",]
June_09_04_rep <- June_09_full[June_09_full$Region_name == "GRID_B_N_REP_6_09_04",]

June_09_02_night <- June_09_full[June_09_full$Region_name == "GRID_B_N_NIGHT_6_09_02",]
June_09_03_night <- June_09_full[June_09_full$Region_name == "GRID_B_N_NIGHT_6_09_03",]
June_09_04_night <- June_09_full[June_09_full$Region_name == "GRID_B_N_NIGHT_6_09_04",]

## filter to not include above 60 m

June_09_02 <- June_09_02 %>% filter(Layer_depth_max <= 30)
June_09_03 <- June_09_03 %>% filter(Layer_depth_max <= 30)
June_09_04 <- June_09_04 %>% filter(Layer_depth_max <= 30)

June_09_02_rep <- June_09_02_rep %>% filter(Layer_depth_max <= 30)
June_09_03_rep <- June_09_03_rep %>% filter(Layer_depth_max <= 30)
June_09_04_rep <- June_09_04_rep %>% filter(Layer_depth_max <= 30)

June_09_02_night <- June_09_02_night %>% filter(Layer_depth_max <= 30)
June_09_03_night <- June_09_03_night %>% filter(Layer_depth_max <= 30)
June_09_04_night <- June_09_04_night %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(June_09_02$PRC_ABC)) {
  June_09_02$abundance[i] <- NA
  June_09_02$abundance[i] <- June_09_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(June_09_03$PRC_ABC)) {
  June_09_03$abundance[i] <- NA
  June_09_03$abundance[i] <- June_09_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(June_09_04$PRC_ABC)) {
  June_09_04$abundance[i] <- NA
  June_09_04$abundance[i] <- June_09_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 2

for (i in 1:length(June_09_02_rep$PRC_ABC)) {
  June_09_02_rep$abundance[i] <- NA
  June_09_02_rep$abundance[i] <- June_09_02_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 3

for (i in 1:length(June_09_03_rep$PRC_ABC)) {
  June_09_03_rep$abundance[i] <- NA
  June_09_03_rep$abundance[i] <- June_09_03_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 4

for (i in 1:length(June_09_04_rep$PRC_ABC)) {
  June_09_04_rep$abundance[i] <- NA
  June_09_04_rep$abundance[i] <- June_09_04_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 2

for (i in 1:length(June_09_02_night$PRC_ABC)) {
  June_09_02_night$abundance[i] <- NA
  June_09_02_night$abundance[i] <- June_09_02_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 3

for (i in 1:length(June_09_03_night$PRC_ABC)) {
  June_09_03_night$abundance[i] <- NA
  June_09_03_night$abundance[i] <- June_09_03_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 4

for (i in 1:length(June_09_04_night$PRC_ABC)) {
  June_09_04_night$abundance[i] <- NA
  June_09_04_night$abundance[i] <- June_09_04_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### June 11 ####

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
June_11_full <- read.csv("June_11_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

June_11_02 <- June_11_full[June_11_full$Region_name == "GRID_B_N_6_11_02",]
June_11_03 <- June_11_full[June_11_full$Region_name == "GRID_B_N_6_11_03",]
June_11_04 <- June_11_full[June_11_full$Region_name == "GRID_B_N_6_11_04",]

June_11_02_rep <- June_11_full[June_11_full$Region_name == "GRID_B_N_REP_6_11_02",]
June_11_03_rep <- June_11_full[June_11_full$Region_name == "GRID_B_N_REP_6_11_03",]
June_11_04_rep <- June_11_full[June_11_full$Region_name == "GRID_B_N_REP_6_11_04",]

June_11_02_night <- June_11_full[June_11_full$Region_name == "GRID_B_N_NIGHT_6_11_02",]
June_11_03_night <- June_11_full[June_11_full$Region_name == "GRID_B_N_NIGHT_6_11_03",]
June_11_04_night <- June_11_full[June_11_full$Region_name == "GRID_B_N_NIGHT_6_11_04",]

## filter to not include above 60 m

June_11_02 <- June_11_02 %>% filter(Layer_depth_max <= 30)
June_11_03 <- June_11_03 %>% filter(Layer_depth_max <= 30)
June_11_04 <- June_11_04 %>% filter(Layer_depth_max <= 30)

June_11_02_rep <- June_11_02_rep %>% filter(Layer_depth_max <= 30)
June_11_03_rep <- June_11_03_rep %>% filter(Layer_depth_max <= 30)
June_11_04_rep <- June_11_04_rep %>% filter(Layer_depth_max <= 30)

June_11_02_night <- June_11_02_night %>% filter(Layer_depth_max <= 30)
June_11_03_night <- June_11_03_night %>% filter(Layer_depth_max <= 30)
June_11_04_night <- June_11_04_night %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(June_11_02$PRC_ABC)) {
  June_11_02$abundance[i] <- NA
  June_11_02$abundance[i] <- June_11_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(June_11_03$PRC_ABC)) {
  June_11_03$abundance[i] <- NA
  June_11_03$abundance[i] <- June_11_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(June_11_04$PRC_ABC)) {
  June_11_04$abundance[i] <- NA
  June_11_04$abundance[i] <- June_11_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 2

for (i in 1:length(June_11_02_rep$PRC_ABC)) {
  June_11_02_rep$abundance[i] <- NA
  June_11_02_rep$abundance[i] <- June_11_02_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 3

for (i in 1:length(June_11_03_rep$PRC_ABC)) {
  June_11_03_rep$abundance[i] <- NA
  June_11_03_rep$abundance[i] <- June_11_03_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 4

for (i in 1:length(June_11_04_rep$PRC_ABC)) {
  June_11_04_rep$abundance[i] <- NA
  June_11_04_rep$abundance[i] <- June_11_04_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 2

for (i in 1:length(June_11_02_night$PRC_ABC)) {
  June_11_02_night$abundance[i] <- NA
  June_11_02_night$abundance[i] <- June_11_02_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 3

for (i in 1:length(June_11_03_night$PRC_ABC)) {
  June_11_03_night$abundance[i] <- NA
  June_11_03_night$abundance[i] <- June_11_03_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Night - Transect 4

for (i in 1:length(June_11_04_night$PRC_ABC)) {
  June_11_04_night$abundance[i] <- NA
  June_11_04_night$abundance[i] <- June_11_04_night$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### June 13 ####

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
June_13_full <- read.csv("June_13_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

June_13_02 <- June_13_full[June_13_full$Region_name == "GRID_B_N_6_13_02",]
June_13_03 <- June_13_full[June_13_full$Region_name == "GRID_B_N_6_13_03",]
June_13_04 <- June_13_full[June_13_full$Region_name == "GRID_B_N_6_13_04",]

June_13_02_rep <- June_13_full[June_13_full$Region_name == "GRID_B_N_REP_6_13_02",]
June_13_03_rep <- June_13_full[June_13_full$Region_name == "GRID_B_N_REP_6_13_03",]
June_13_04_rep <- June_13_full[June_13_full$Region_name == "GRID_B_N_REP_6_13_04",]

## filter to not include above 60 m

June_13_02 <- June_13_02 %>% filter(Layer_depth_max <= 30)
June_13_03 <- June_13_03 %>% filter(Layer_depth_max <= 30)
June_13_04 <- June_13_04 %>% filter(Layer_depth_max <= 30)

June_13_02_rep <- June_13_02_rep %>% filter(Layer_depth_max <= 30)
June_13_03_rep <- June_13_03_rep %>% filter(Layer_depth_max <= 30)
June_13_04_rep <- June_13_04_rep %>% filter(Layer_depth_max <= 30)

## Day - Transect 2

for (i in 1:length(June_13_02$PRC_ABC)) {
  June_13_02$abundance[i] <- NA
  June_13_02$abundance[i] <- June_13_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 3

for (i in 1:length(June_13_03$PRC_ABC)) {
  June_13_03$abundance[i] <- NA
  June_13_03$abundance[i] <- June_13_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Day - Transect 4

for (i in 1:length(June_13_04$PRC_ABC)) {
  June_13_04$abundance[i] <- NA
  June_13_04$abundance[i] <- June_13_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 2

for (i in 1:length(June_13_02_rep$PRC_ABC)) {
  June_13_02_rep$abundance[i] <- NA
  June_13_02_rep$abundance[i] <- June_13_02_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 3

for (i in 1:length(June_13_03_rep$PRC_ABC)) {
  June_13_03_rep$abundance[i] <- NA
  June_13_03_rep$abundance[i] <- June_13_03_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}

## Repeat - Transect 4

for (i in 1:length(June_13_04_rep$PRC_ABC)) {
  June_13_04_rep$abundance[i] <- NA
  June_13_04_rep$abundance[i] <- June_13_04_rep$PRC_ABC[i]*transect_area/sigma_bs_herring
}


#### Abundance per bin




#### Abundance per bin

#### May 02 ####

May_02_Transect_2 <- May_02_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_02_Transect_3 <- May_02_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_02_Transect_4 <- May_02_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### May 04 ####

# Day

May_04_Transect_2 <- May_04_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_04_Transect_3 <- May_04_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_04_Transect_4 <- May_04_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Repeat

May_04_Transect_2_Repeat <- May_04_02_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_04_Transect_3_Repeat <- May_04_03_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_04_Transect_4_Repeat <- May_04_04_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Night

May_04_Transect_2_Night <- May_04_02_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_04_Transect_3_Night <- May_04_03_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_04_Transect_4_Night <- May_04_04_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### May 06 ####

# Day

May_06_Transect_2 <- May_06_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_06_Transect_3 <- May_06_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_06_Transect_4 <- May_06_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Repeat

May_06_Transect_2_Repeat <- May_06_02_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_06_Transect_3_Repeat <- May_06_03_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_06_Transect_4_Repeat <- May_06_04_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### May 07 ####

# Day

May_07_Transect_2 <- May_07_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_07_Transect_3 <- May_07_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_07_Transect_4 <- May_07_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Repeat

May_07_Transect_2_Repeat <- May_07_02_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_07_Transect_3_Repeat <- May_07_03_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_07_Transect_4_Repeat <- May_07_04_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Night

May_07_Transect_2_Night <- May_07_02_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_07_Transect_3_Night <- May_07_03_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_07_Transect_4_Night <- May_07_04_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### May 08 ####

May_08_Transect_2 <- May_08_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_08_Transect_3 <- May_08_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_08_Transect_4 <- May_08_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### May 09 ####

May_09_Transect_2 <- May_09_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_09_Transect_3 <- May_09_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_09_Transect_4 <- May_09_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### May 10 ####

May_10_Transect_2 <- May_10_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_10_Transect_3 <- May_10_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_10_Transect_4 <- May_10_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### May 13 ####

May_13_Transect_2 <- May_13_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_13_Transect_3 <- May_13_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

May_13_Transect_4 <- May_13_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### June 03 ####

June_03_Transect_2 <- June_03_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_03_Transect_3 <- June_03_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_03_Transect_4 <- June_03_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### June 04 ####

# Day

June_04_Transect_2 <- June_04_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_04_Transect_3 <- June_04_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_04_Transect_4 <- June_04_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Night

June_04_Transect_2_Night <- June_04_02_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_04_Transect_3_Night <- June_04_03_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_04_Transect_4_Night <- June_04_04_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### June 06 ####

June_06_Transect_2 <- June_06_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_06_Transect_3 <- June_06_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_06_Transect_4 <- June_06_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### June 07 ####

June_07_Transect_2 <- June_07_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_07_Transect_3 <- June_07_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_07_Transect_4 <- June_07_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### June 08 ####

June_08_Transect_2 <- June_08_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_08_Transect_3 <- June_08_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_08_Transect_4 <- June_08_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### June 09 ####

# Day

June_09_Transect_2 <- June_09_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_09_Transect_3 <- June_09_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_09_Transect_4 <- June_09_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Repeat

June_09_Transect_2_Repeat <- June_09_02_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_09_Transect_3_Repeat <- June_09_03_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_09_Transect_4_Repeat <- June_09_04_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Night

June_09_Transect_2_Night <- June_09_02_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_09_Transect_3_Night <- June_09_03_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_09_Transect_4_Night <- June_09_04_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### June 11 ####

# Day

June_11_Transect_2 <- June_11_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_11_Transect_3 <- June_11_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_11_Transect_4 <- June_11_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Repeat

June_11_Transect_2_Repeat <- June_11_02_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_11_Transect_3_Repeat <- June_11_03_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_11_Transect_4_Repeat <- June_11_04_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Night

June_11_Transect_2_Night <- June_11_02_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_11_Transect_3_Night <- June_11_03_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_11_Transect_4_Night <- June_11_04_night %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

#### June 13 ####

# Day

June_13_Transect_2 <- June_13_02 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_13_Transect_3 <- June_13_03 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_13_Transect_4 <- June_13_04 %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

# Repeat

June_13_Transect_2_Repeat <- June_13_02_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_13_Transect_3_Repeat <- June_13_03_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))

June_13_Transect_4_Repeat <- June_13_04_rep %>%
  group_by(Dist_S) %>%
  summarise(Sum = sum(abundance))




#### Total abundance along transect ####

May_02_2_Abundance <- sum(May_02_Transect_2$Sum)
May_02_3_Abundance <- sum(May_02_Transect_3$Sum)
May_02_4_Abundance <- sum(May_02_Transect_4$Sum)

May_04_2_Abundance <- sum(May_04_Transect_2$Sum)
May_04_3_Abundance <- sum(May_04_Transect_3$Sum)
May_04_4_Abundance <- sum(May_04_Transect_4$Sum)
May_04_2_Abundance_Night <- sum(May_04_Transect_2_Night$Sum)
May_04_3_Abundance_Night <- sum(May_04_Transect_3_Night$Sum)
May_04_4_Abundance_Night <- sum(May_04_Transect_4_Night$Sum)
May_04_2_Abundance_Repeat <- sum(May_04_Transect_2_Repeat$Sum)
May_04_3_Abundance_Repeat <- sum(May_04_Transect_3_Repeat$Sum)
May_04_4_Abundance_Repeat <- sum(May_04_Transect_4_Repeat$Sum)

May_06_2_Abundance <- sum(May_06_Transect_2$Sum)
May_06_3_Abundance <- sum(May_06_Transect_3$Sum)
May_06_4_Abundance <- sum(May_06_Transect_4$Sum)
May_06_2_Abundance_Repeat <- sum(May_06_Transect_2_Repeat$Sum)
May_06_3_Abundance_Repeat <- sum(May_06_Transect_3_Repeat$Sum)
May_06_4_Abundance_Repeat <- sum(May_06_Transect_4_Repeat$Sum)

May_07_2_Abundance <- sum(May_07_Transect_2$Sum)
May_07_3_Abundance <- sum(May_07_Transect_3$Sum)
May_07_4_Abundance <- sum(May_07_Transect_4$Sum)
May_07_2_Abundance_Night <- sum(May_07_Transect_2_Night$Sum)
May_07_3_Abundance_Night <- sum(May_07_Transect_3_Night$Sum)
May_07_4_Abundance_Night <- sum(May_07_Transect_4_Night$Sum)
May_07_2_Abundance_Repeat <- sum(May_07_Transect_2_Repeat$Sum)
May_07_3_Abundance_Repeat <- sum(May_07_Transect_3_Repeat$Sum)
May_07_4_Abundance_Repeat <- sum(May_07_Transect_4_Repeat$Sum)

May_08_2_Abundance <- sum(May_08_Transect_2$Sum)
May_08_3_Abundance <- sum(May_08_Transect_3$Sum)
May_08_4_Abundance <- sum(May_08_Transect_4$Sum)

May_09_2_Abundance <- sum(May_09_Transect_2$Sum)
May_09_3_Abundance <- sum(May_09_Transect_3$Sum)
May_09_4_Abundance <- sum(May_09_Transect_4$Sum)

May_10_2_Abundance <- sum(May_10_Transect_2$Sum)
May_10_3_Abundance <- sum(May_10_Transect_3$Sum)
May_10_4_Abundance <- sum(May_10_Transect_4$Sum)

May_13_2_Abundance <- sum(May_13_Transect_2$Sum)
May_13_3_Abundance <- sum(May_13_Transect_3$Sum)
May_13_4_Abundance <- sum(May_13_Transect_4$Sum)

June_03_2_Abundance <- sum(June_03_Transect_2$Sum)
June_03_3_Abundance <- sum(June_03_Transect_3$Sum)
June_03_4_Abundance <- sum(June_03_Transect_4$Sum)

June_04_2_Abundance <- sum(June_04_Transect_2$Sum)
June_04_3_Abundance <- sum(June_04_Transect_3$Sum)
June_04_4_Abundance <- sum(June_04_Transect_4$Sum)
June_04_2_Abundance_Night <- sum(June_04_Transect_2_Night$Sum)
June_04_3_Abundance_Night <- sum(June_04_Transect_3_Night$Sum)
June_04_4_Abundance_Night <- sum(June_04_Transect_4_Night$Sum)

June_06_2_Abundance <- sum(June_06_Transect_2$Sum)
June_06_3_Abundance <- sum(June_06_Transect_3$Sum)
June_06_4_Abundance <- sum(June_06_Transect_4$Sum)

June_07_2_Abundance <- sum(June_07_Transect_2$Sum)
June_07_3_Abundance <- sum(June_07_Transect_3$Sum)
June_07_4_Abundance <- sum(June_07_Transect_4$Sum)

June_08_2_Abundance <- sum(June_08_Transect_2$Sum)
June_08_3_Abundance <- sum(June_08_Transect_3$Sum)
June_08_4_Abundance <- sum(June_08_Transect_4$Sum)

June_09_2_Abundance <- sum(June_09_Transect_2$Sum)
June_09_3_Abundance <- sum(June_09_Transect_3$Sum)
June_09_4_Abundance <- sum(June_09_Transect_4$Sum)
June_09_2_Abundance_Repeat <- sum(June_09_Transect_2_Repeat$Sum)
June_09_3_Abundance_Repeat <- sum(June_09_Transect_3_Repeat$Sum)
June_09_4_Abundance_Repeat <- sum(June_09_Transect_4_Repeat$Sum)
June_09_2_Abundance_Night <- sum(June_09_Transect_2_Night$Sum)
June_09_3_Abundance_Night <- sum(June_09_Transect_3_Night$Sum)
June_09_4_Abundance_Night <- sum(June_09_Transect_4_Night$Sum)

June_11_2_Abundance <- sum(June_11_Transect_2$Sum)
June_11_3_Abundance <- sum(June_11_Transect_3$Sum)
June_11_4_Abundance <- sum(June_11_Transect_4$Sum)
June_11_2_Abundance_Repeat <- sum(June_11_Transect_2_Repeat$Sum)
June_11_3_Abundance_Repeat <- sum(June_11_Transect_3_Repeat$Sum)
June_11_4_Abundance_Repeat <- sum(June_11_Transect_4_Repeat$Sum)
June_11_2_Abundance_Night <- sum(June_11_Transect_2_Night$Sum)
June_11_3_Abundance_Night <- sum(June_11_Transect_3_Night$Sum)
June_11_4_Abundance_Night <- sum(June_11_Transect_4_Night$Sum)

June_13_2_Abundance <- sum(June_13_Transect_2$Sum)
June_13_3_Abundance <- sum(June_13_Transect_3$Sum)
June_13_4_Abundance <- sum(June_13_Transect_4$Sum)
June_13_2_Abundance_Repeat <- sum(June_13_Transect_2_Repeat$Sum)
June_13_3_Abundance_Repeat <- sum(June_13_Transect_3_Repeat$Sum)
June_13_4_Abundance_Repeat <- sum(June_13_Transect_4_Repeat$Sum)

abun_vector <- vector()
abun_vector <- c(May_02_2_Abundance, May_02_3_Abundance, May_02_4_Abundance,
                 May_04_2_Abundance, May_04_3_Abundance, May_04_4_Abundance, May_04_2_Abundance_Night, May_04_3_Abundance_Night, May_04_4_Abundance_Night, May_04_2_Abundance_Repeat, May_04_3_Abundance_Repeat, May_04_4_Abundance_Repeat,
                 May_06_2_Abundance, May_06_3_Abundance, May_06_4_Abundance, May_06_2_Abundance_Repeat, May_06_3_Abundance_Repeat, May_06_4_Abundance_Repeat,
                 May_07_2_Abundance, May_07_3_Abundance, May_07_4_Abundance, May_07_2_Abundance_Night, May_07_3_Abundance_Night, May_07_4_Abundance_Night, May_07_2_Abundance_Repeat, May_07_3_Abundance_Repeat, May_07_4_Abundance_Repeat,
                 May_08_2_Abundance, May_08_3_Abundance, May_08_4_Abundance,
                 May_09_2_Abundance, May_09_3_Abundance, May_09_4_Abundance,
                 May_10_2_Abundance, May_10_3_Abundance, May_10_4_Abundance,
                 May_13_2_Abundance, May_13_3_Abundance, May_13_4_Abundance, 
                 June_03_2_Abundance, June_03_3_Abundance, June_03_4_Abundance,
                 June_04_2_Abundance, June_04_3_Abundance, June_04_4_Abundance, June_04_2_Abundance_Night, June_04_3_Abundance_Night, June_04_4_Abundance_Night,
                 June_06_2_Abundance, June_06_3_Abundance, June_06_4_Abundance,
                 June_07_2_Abundance, June_07_3_Abundance, June_07_4_Abundance,
                 June_08_2_Abundance, June_08_3_Abundance, June_08_4_Abundance,
                 June_09_2_Abundance, June_09_3_Abundance, June_09_4_Abundance, June_09_2_Abundance_Repeat, June_09_3_Abundance_Repeat, June_09_4_Abundance_Repeat, June_09_2_Abundance_Night, June_09_3_Abundance_Night, June_09_4_Abundance_Night,
                 June_11_2_Abundance, June_11_3_Abundance, June_11_4_Abundance, June_11_2_Abundance_Repeat, June_11_3_Abundance_Repeat, June_11_4_Abundance_Repeat, June_11_2_Abundance_Night, June_11_3_Abundance_Night, June_11_4_Abundance_Night,
                 June_13_2_Abundance, June_13_3_Abundance, June_13_4_Abundance, June_13_2_Abundance_Repeat, June_13_3_Abundance_Repeat, June_13_4_Abundance_Repeat)


#### Probabilities of each bin ####

# May 2
May_2_ZOI_2_Prob <- May_02_Transect_2[,2]/May_02_2_Abundance
May_2_ZOI_3_Prob <- May_02_Transect_3[,2]/May_02_3_Abundance
May_2_ZOI_4_Prob <- May_02_Transect_4[,2]/May_02_4_Abundance

# May 4
May_4_ZOI_2_Prob <- May_04_Transect_2[,2]/May_04_2_Abundance
May_4_ZOI_3_Prob <- May_04_Transect_3[,2]/May_04_3_Abundance
May_4_ZOI_4_Prob <- May_04_Transect_4[,2]/May_04_4_Abundance
May_4_ZOI_2_Prob_Night <- May_04_Transect_2_Night[,2]/May_04_2_Abundance_Night
May_4_ZOI_3_Prob_Night <- May_04_Transect_3_Night[,2]/May_04_3_Abundance_Night
May_4_ZOI_4_Prob_Night <- May_04_Transect_4_Night[,2]/May_04_4_Abundance_Night
May_4_ZOI_2_Prob_Repeat <- May_04_Transect_2_Repeat[,2]/May_04_2_Abundance_Repeat
May_4_ZOI_3_Prob_Repeat <- May_04_Transect_3_Repeat[,2]/May_04_3_Abundance_Repeat
May_4_ZOI_4_Prob_Repeat <- May_04_Transect_4_Repeat[,2]/May_04_4_Abundance_Repeat

# May 6
May_6_ZOI_2_Prob <- May_06_Transect_2[,2]/May_06_2_Abundance
May_6_ZOI_3_Prob <- May_06_Transect_3[,2]/May_06_3_Abundance
May_6_ZOI_4_Prob <- May_06_Transect_4[,2]/May_06_4_Abundance
May_6_ZOI_2_Prob_Repeat <- May_06_Transect_2_Repeat[,2]/May_06_2_Abundance_Repeat
May_6_ZOI_3_Prob_Repeat <- May_06_Transect_3_Repeat[,2]/May_06_3_Abundance_Repeat
May_6_ZOI_4_Prob_Repeat <- May_06_Transect_4_Repeat[,2]/May_06_4_Abundance_Repeat

# May 7
May_7_ZOI_2_Prob <- May_07_Transect_2[,2]/May_07_2_Abundance
May_7_ZOI_3_Prob <- May_07_Transect_3[,2]/May_07_3_Abundance
May_7_ZOI_4_Prob <- May_07_Transect_4[,2]/May_07_4_Abundance
May_7_ZOI_2_Prob_Night <- May_07_Transect_2_Night[,2]/May_07_2_Abundance_Night
May_7_ZOI_3_Prob_Night <- May_07_Transect_3_Night[,2]/May_07_3_Abundance_Night
May_7_ZOI_4_Prob_Night <- May_07_Transect_4_Night[,2]/May_07_4_Abundance_Night
May_7_ZOI_2_Prob_Repeat <- May_07_Transect_2_Repeat[,2]/May_07_2_Abundance_Repeat
May_7_ZOI_3_Prob_Repeat <- May_07_Transect_3_Repeat[,2]/May_07_3_Abundance_Repeat
May_7_ZOI_4_Prob_Repeat <- May_07_Transect_4_Repeat[,2]/May_07_4_Abundance_Repeat

# May 8
May_8_ZOI_2_Prob <- May_08_Transect_2[,2]/May_08_2_Abundance
May_8_ZOI_3_Prob <- May_08_Transect_2[,2]/May_08_2_Abundance
May_8_ZOI_4_Prob <- May_08_Transect_2[,2]/May_08_2_Abundance

# May 9
May_9_ZOI_2_Prob <- May_09_Transect_2[,2]/May_09_2_Abundance
May_9_ZOI_3_Prob <- May_09_Transect_3[,2]/May_09_3_Abundance
May_9_ZOI_4_Prob <- May_09_Transect_4[,2]/May_09_4_Abundance

# May 10 
May_10_ZOI_2_Prob <- May_10_Transect_2[,2]/May_10_2_Abundance
May_10_ZOI_3_Prob <- May_10_Transect_3[,2]/May_10_3_Abundance
May_10_ZOI_4_Prob <- May_10_Transect_4[,2]/May_10_4_Abundance

# May 13
May_13_ZOI_2_Prob <- May_13_Transect_2[,2]/May_13_2_Abundance
May_13_ZOI_3_Prob <- May_13_Transect_3[,2]/May_13_3_Abundance
May_13_ZOI_4_Prob <- May_13_Transect_4[,2]/May_13_4_Abundance

# June 03
June_03_ZOI_2_Prob <- June_03_Transect_2[,2]/June_03_2_Abundance
June_03_ZOI_3_Prob <- June_03_Transect_3[,2]/June_03_3_Abundance
June_03_ZOI_4_Prob <- June_03_Transect_4[,2]/June_03_4_Abundance

# June 04 
June_04_ZOI_2_Prob <- June_04_Transect_2[,2]/June_04_2_Abundance
June_04_ZOI_3_Prob <- June_04_Transect_3[,2]/June_04_3_Abundance
June_04_ZOI_4_Prob <- June_04_Transect_4[,2]/June_04_4_Abundance
June_04_ZOI_2_Prob_Night <- June_04_Transect_2_Night[,2]/June_04_2_Abundance_Night
June_04_ZOI_3_Prob_Night <- June_04_Transect_3_Night[,2]/June_04_3_Abundance_Night
June_04_ZOI_4_Prob_Night <- June_04_Transect_4_Night[,2]/June_04_4_Abundance_Night

# June 06
June_06_ZOI_2_Prob <- June_06_Transect_2[,2]/June_06_2_Abundance
June_06_ZOI_3_Prob <- June_06_Transect_3[,2]/June_06_3_Abundance
June_06_ZOI_4_Prob <- June_06_Transect_4[,2]/June_06_4_Abundance

# June 07
June_07_ZOI_2_Prob <- June_07_Transect_2[,2]/June_07_2_Abundance
June_07_ZOI_3_Prob <- June_07_Transect_3[,2]/June_07_3_Abundance
June_07_ZOI_4_Prob <- June_07_Transect_4[,2]/June_07_4_Abundance

# June 08
June_08_ZOI_2_Prob <- June_08_Transect_2[,2]/June_08_2_Abundance
June_08_ZOI_3_Prob <- June_08_Transect_3[,2]/June_08_3_Abundance
June_08_ZOI_4_Prob <- June_08_Transect_4[,2]/June_08_4_Abundance

# June 09
June_09_ZOI_2_Prob <- June_09_Transect_2[,2]/June_09_2_Abundance
June_09_ZOI_3_Prob <- June_09_Transect_3[,2]/June_09_3_Abundance
June_09_ZOI_4_Prob <- June_09_Transect_4[,2]/June_09_4_Abundance
June_09_ZOI_2_Prob_Night <- June_09_Transect_2_Night[,2]/June_09_2_Abundance_Night
June_09_ZOI_3_Prob_Night <- June_09_Transect_3_Night[,2]/June_09_3_Abundance_Night
June_09_ZOI_4_Prob_Night <- June_09_Transect_4_Night[,2]/June_09_4_Abundance_Night
June_09_ZOI_2_Prob_Repeat <- June_09_Transect_2_Repeat[,2]/June_09_2_Abundance_Repeat
June_09_ZOI_3_Prob_Repeat <- June_09_Transect_3_Repeat[,2]/June_09_3_Abundance_Repeat
June_09_ZOI_4_Prob_Repeat <- June_09_Transect_4_Repeat[,2]/June_09_4_Abundance_Repeat

# June 11
June_11_ZOI_2_Prob <- June_11_Transect_2[,2]/June_11_2_Abundance
June_11_ZOI_3_Prob <- June_11_Transect_3[,2]/June_11_3_Abundance
June_11_ZOI_4_Prob <- June_11_Transect_4[,2]/June_11_4_Abundance
June_11_ZOI_2_Prob_Night <- June_11_Transect_2_Night[,2]/June_11_2_Abundance_Night
June_11_ZOI_3_Prob_Night <- June_11_Transect_3_Night[,2]/June_11_3_Abundance_Night
June_11_ZOI_4_Prob_Night <- June_11_Transect_4_Night[,2]/June_11_4_Abundance_Night
June_11_ZOI_2_Prob_Repeat <- June_11_Transect_2_Repeat[,2]/June_11_2_Abundance_Repeat
June_11_ZOI_3_Prob_Repeat <- June_11_Transect_3_Repeat[,2]/June_11_3_Abundance_Repeat
June_11_ZOI_4_Prob_Repeat <- June_11_Transect_4_Repeat[,2]/June_11_4_Abundance_Repeat

# June 13
June_13_ZOI_2_Prob <- June_13_Transect_2[,2]/June_13_2_Abundance
June_13_ZOI_3_Prob <- June_13_Transect_3[,2]/June_13_3_Abundance
June_13_ZOI_4_Prob <- June_13_Transect_4[,2]/June_13_4_Abundance
June_13_ZOI_2_Prob_Repeat <- June_13_Transect_2_Repeat[,2]/June_13_2_Abundance_Repeat
June_13_ZOI_3_Prob_Repeat <- June_13_Transect_3_Repeat[,2]/June_13_3_Abundance_Repeat
June_13_ZOI_4_Prob_Repeat <- June_13_Transect_4_Repeat[,2]/June_13_4_Abundance_Repeat

All_ZOI_Probs <- c(May_2_ZOI_2_Prob[,1], May_2_ZOI_3_Prob[,1], May_2_ZOI_4_Prob[,1],
                   May_4_ZOI_2_Prob[,1], May_4_ZOI_3_Prob[,1], May_4_ZOI_4_Prob[,1], May_4_ZOI_2_Prob_Night[,1], May_4_ZOI_3_Prob_Night[,1], May_4_ZOI_4_Prob_Night[,1], May_4_ZOI_2_Prob_Repeat[,1], May_4_ZOI_3_Prob_Repeat[,1], May_4_ZOI_4_Prob_Repeat[,1],
                   May_6_ZOI_2_Prob[,1], May_6_ZOI_3_Prob[,1], May_6_ZOI_4_Prob[,1], May_6_ZOI_2_Prob_Repeat[,1], May_6_ZOI_3_Prob_Repeat[,1], May_6_ZOI_4_Prob_Repeat[,1],
                   May_7_ZOI_2_Prob[,1], May_7_ZOI_3_Prob[,1], May_7_ZOI_4_Prob[,1], May_7_ZOI_2_Prob_Night[,1], May_7_ZOI_3_Prob_Night[,1], May_7_ZOI_4_Prob_Night[,1], May_7_ZOI_2_Prob_Repeat[,1], May_7_ZOI_3_Prob_Repeat[,1], May_7_ZOI_4_Prob_Repeat[,1],
                   May_8_ZOI_2_Prob[,1], May_8_ZOI_3_Prob[,1], May_8_ZOI_4_Prob[,1],
                   May_9_ZOI_2_Prob[,1], May_9_ZOI_3_Prob[,1], May_9_ZOI_4_Prob[,1],
                   May_10_ZOI_2_Prob[,1], May_10_ZOI_3_Prob[,1], May_10_ZOI_4_Prob[,1],
                   May_13_ZOI_2_Prob[,1], May_13_ZOI_3_Prob[,1], May_13_ZOI_4_Prob[,1],
                   June_03_ZOI_2_Prob[,1], June_03_ZOI_3_Prob[,1], June_03_ZOI_4_Prob[,1],
                   June_04_ZOI_2_Prob[,1], June_04_ZOI_3_Prob[,1], June_04_ZOI_4_Prob[,1], June_04_ZOI_2_Prob_Night[,1], June_04_ZOI_3_Prob_Night[,1], June_04_ZOI_4_Prob_Night[,1],
                   June_06_ZOI_2_Prob[,1], June_06_ZOI_3_Prob[,1], June_06_ZOI_4_Prob[,1],
                   June_07_ZOI_2_Prob[,1], June_07_ZOI_3_Prob[,1], June_07_ZOI_4_Prob[,1],
                   June_08_ZOI_2_Prob[,1], June_08_ZOI_3_Prob[,1], June_08_ZOI_4_Prob[,1],
                   June_09_ZOI_2_Prob[,1], June_09_ZOI_3_Prob[,1], June_09_ZOI_4_Prob[,1], June_09_ZOI_2_Prob_Night[,1], June_09_ZOI_3_Prob_Night[,1], June_09_ZOI_4_Prob_Night[,1], June_09_ZOI_2_Prob_Repeat[,1], June_09_ZOI_3_Prob_Repeat[,1], June_09_ZOI_4_Prob_Repeat[,1],
                   June_11_ZOI_2_Prob[,1], June_11_ZOI_3_Prob[,1], June_11_ZOI_4_Prob[,1], June_11_ZOI_2_Prob_Night[,1], June_11_ZOI_3_Prob_Night[,1], June_11_ZOI_4_Prob_Night[,1], June_11_ZOI_2_Prob_Repeat[,1], June_11_ZOI_3_Prob_Repeat[,1], June_11_ZOI_4_Prob_Repeat[,1],
                   June_13_ZOI_2_Prob[,1], June_13_ZOI_3_Prob[,1], June_13_ZOI_4_Prob[,1], June_13_ZOI_2_Prob_Repeat[,1], June_13_ZOI_3_Prob_Repeat[,1], June_13_ZOI_4_Prob_Repeat[,1])

hist(All_ZOI_Probs, breaks = 200)
