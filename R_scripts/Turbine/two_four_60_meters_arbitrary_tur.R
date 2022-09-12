## Obtaining abundances per day per transect (2-4)
## Arbitrary ZOI
## August 2022

#### Variables ####

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)
transect_area <- 60*1500

## Obtaining Abundances ## 

# Filtering

#### May 02 ####

## 3m VBins 15m HBins 60m from bottom

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 60m bottom")
May_02_full <- read.csv("May_02_3m_VBins_15m_HBins_60m_bottom.csv")

## filtering transects

May_02_02 <- May_02_full[May_02_full$Region_name == "Grid_A_N_5_02_02",]
May_02_03 <- May_02_full[May_02_full$Region_name == "Grid_A_N_5_02_03",]
May_02_04 <- May_02_full[May_02_full$Region_name == "Grid_A_N_5_02_04",]

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


#### Sums of Abundance along Transect #### 

library(dplyr)

# Summing vertically

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



#### Total number of fish in each transect ####

sum1 <- sum(May_02_Transect_2$Sum)
sum2 <- sum(May_02_Transect_3$Sum)
sum3 <- sum(May_02_Transect_4$Sum)

sum4 <- sum(May_04_Transect_2$Sum)
sum5 <- sum(May_04_Transect_3$Sum)
sum6 <- sum(May_04_Transect_4$Sum)
sum7 <- sum(May_04_Transect_2_Night$Sum)
sum8 <- sum(May_04_Transect_3_Night$Sum)
sum9 <- sum(May_04_Transect_4_Night$Sum)
sum10 <- sum(May_04_Transect_2_Repeat$Sum)
sum11 <- sum(May_04_Transect_3_Repeat$Sum)
sum12 <- sum(May_04_Transect_4_Repeat$Sum)

sum13 <- sum(May_06_Transect_2$Sum)
sum14 <- sum(May_06_Transect_3$Sum)
sum15 <- sum(May_06_Transect_4$Sum)
sum16 <- sum(May_06_Transect_2_Repeat$Sum)
sum17 <- sum(May_06_Transect_3_Repeat$Sum)
sum18 <- sum(May_06_Transect_4_Repeat$Sum)

sum19 <- sum(May_07_Transect_2$Sum)
sum20 <- sum(May_07_Transect_3$Sum)
sum21 <- sum(May_07_Transect_4$Sum)
sum22 <- sum(May_07_Transect_2_Night$Sum)
sum23 <- sum(May_07_Transect_3_Night$Sum)
sum24 <- sum(May_07_Transect_4_Night$Sum)
sum25 <- sum(May_07_Transect_2_Repeat$Sum)
sum26 <- sum(May_07_Transect_3_Repeat$Sum)
sum27 <- sum(May_07_Transect_4_Repeat$Sum)

sum28 <- sum(May_08_Transect_2$Sum)
sum29 <- sum(May_08_Transect_3$Sum)
sum30 <- sum(May_08_Transect_4$Sum)

sum31 <- sum(May_09_Transect_2$Sum)
sum32 <- sum(May_09_Transect_3$Sum)
sum33 <- sum(May_09_Transect_4$Sum)

sum34 <- sum(May_10_Transect_2$Sum)
sum35 <- sum(May_10_Transect_3$Sum)
sum36 <- sum(May_10_Transect_4$Sum)

sum37 <- sum(May_13_Transect_2$Sum)
sum38 <- sum(May_13_Transect_3$Sum)
sum39 <- sum(May_13_Transect_4$Sum)

sum40 <- sum(June_03_Transect_2$Sum)
sum41 <- sum(June_03_Transect_3$Sum)
sum42 <- sum(June_03_Transect_4$Sum)

sum43 <- sum(June_04_Transect_2$Sum)
sum44 <- sum(June_04_Transect_3$Sum)
sum45 <- sum(June_04_Transect_4$Sum)
sum46 <- sum(June_04_Transect_2_Night$Sum)
sum47 <- sum(June_04_Transect_3_Night$Sum)
sum48 <- sum(June_04_Transect_4_Night$Sum)

sum49 <- sum(June_06_Transect_2$Sum)
sum50 <- sum(June_06_Transect_3$Sum)
sum51 <- sum(June_06_Transect_4$Sum)

sum52 <- sum(June_07_Transect_2$Sum)
sum53 <- sum(June_07_Transect_3$Sum)
sum54 <- sum(June_07_Transect_4$Sum)

sum55 <- sum(June_08_Transect_2$Sum)
sum56 <- sum(June_08_Transect_3$Sum)
sum57 <- sum(June_08_Transect_4$Sum)

sum58 <- sum(June_09_Transect_2$Sum)
sum59 <- sum(June_09_Transect_3$Sum)
sum60 <- sum(June_09_Transect_4$Sum)
sum61 <- sum(June_09_Transect_2_Repeat$Sum)
sum62 <- sum(June_09_Transect_3_Repeat$Sum)
sum63 <- sum(June_09_Transect_4_Repeat$Sum)
sum64 <- sum(June_09_Transect_2_Night$Sum)
sum65 <- sum(June_09_Transect_3_Night$Sum)
sum66 <- sum(June_09_Transect_4_Night$Sum)

sum67 <- sum(June_11_Transect_2$Sum)
sum68 <- sum(June_11_Transect_3$Sum)
sum69 <- sum(June_11_Transect_4$Sum)
sum70 <- sum(June_11_Transect_2_Repeat$Sum)
sum71 <- sum(June_11_Transect_3_Repeat$Sum)
sum72 <- sum(June_11_Transect_4_Repeat$Sum)
sum73 <- sum(June_11_Transect_2_Night$Sum)
sum74 <- sum(June_11_Transect_3_Night$Sum)
sum75 <- sum(June_11_Transect_4_Night$Sum)

sum76 <- sum(June_13_Transect_2$Sum)
sum77 <- sum(June_13_Transect_3$Sum)
sum78 <- sum(June_13_Transect_4$Sum)
sum79 <- sum(June_13_Transect_2_Repeat$Sum)
sum80 <- sum(June_13_Transect_3_Repeat$Sum)
sum81 <- sum(June_13_Transect_4_Repeat$Sum)

sum_all <- c(sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8, sum9, sum10,
             sum11, sum12, sum13, sum14, sum15, sum16, sum17, sum18, sum19, sum20,
             sum21, sum22, sum23, sum24, sum25, sum26, sum27, sum28, sum29, sum30,
             sum31, sum32, sum33, sum34, sum35, sum36, sum37, sum38, sum39, sum40,
             sum41, sum42, sum43, sum44, sum45, sum46, sum47, sum48, sum49, sum50,
             sum51, sum52, sum53, sum54, sum55, sum56, sum57, sum58, sum59, sum60,
             sum61, sum62, sum63, sum64, sum65, sum66, sum67, sum68, sum79, sum70,
             sum71, sum72, sum73, sum74, sum75, sum76, sum77, sum78, sum79, sum80, 
             sum81)
hist(sum_all, breaks = 20)


#### ZOI Probabilities ####

# May 2
May_2_ZOI_2 <- May_02_Transect_2[51:60,]
May_2_ZOI_2_sum <- sum(May_2_ZOI_2$Sum)
May_2_ZOI_2_Prob <- May_2_ZOI_2_sum/sum1

May_2_ZOI_3 <- May_02_Transect_3[51:60,]
May_2_ZOI_3_sum <- sum(May_2_ZOI_3$Sum)
May_2_ZOI_3_Prob <- May_2_ZOI_3_sum/sum2

May_2_ZOI_4 <- May_02_Transect_4[51:60,]
May_2_ZOI_4_sum <- sum(May_2_ZOI_4$Sum)
May_2_ZOI_4_Prob <- May_2_ZOI_4_sum/sum3

# May 4
May_4_ZOI_2 <- May_04_Transect_2[51:60,]
May_4_ZOI_2_sum <- sum(May_4_ZOI_2$Sum)
May_4_ZOI_2_Prob <- May_4_ZOI_2_sum/sum4

May_4_ZOI_3 <- May_04_Transect_3[51:60,]
May_4_ZOI_3_sum <- sum(May_4_ZOI_3$Sum)
May_4_ZOI_3_Prob <- May_4_ZOI_3_sum/sum5

May_4_ZOI_4 <- May_04_Transect_4[51:60,]
May_4_ZOI_4_sum <- sum(May_4_ZOI_4$Sum)
May_4_ZOI_4_Prob <- May_4_ZOI_4_sum/sum6

May_4_ZOI_2_Night <- May_04_Transect_2_Night[51:60,]
May_4_ZOI_2_sum_Night <- sum(May_4_ZOI_2_Night$Sum)
May_4_ZOI_2_Prob_Night <- May_4_ZOI_2_sum_Night/sum7

May_4_ZOI_3_Night <- May_04_Transect_3_Night[51:60,]
May_4_ZOI_3_sum_Night <- sum(May_4_ZOI_3_Night$Sum)
May_4_ZOI_3_Prob_Night <- May_4_ZOI_3_sum_Night/sum8

May_4_ZOI_4_Night <- May_04_Transect_4_Night[51:60,]
May_4_ZOI_4_sum_Night <- sum(May_4_ZOI_4$Sum)
May_4_ZOI_4_Prob_Night <- May_4_ZOI_4_sum_Night/sum9

May_4_ZOI_2_Repeat <- May_04_Transect_2_Repeat[51:60,]
May_4_ZOI_2_sum_Repeat <- sum(May_4_ZOI_2_Repeat$Sum)
May_4_ZOI_2_Prob_Repeat <- May_4_ZOI_2_sum_Repeat/sum10

May_4_ZOI_3_Repeat <- May_04_Transect_3_Repeat[51:60,]
May_4_ZOI_3_sum_Repeat <- sum(May_4_ZOI_3_Repeat$Sum)
May_4_ZOI_3_Prob_Repeat <- May_4_ZOI_3_sum_Repeat/sum11

May_4_ZOI_4_Repeat <- May_04_Transect_4_Repeat[51:60,]
May_4_ZOI_4_sum_Repeat <- sum(May_4_ZOI_4$Sum)
May_4_ZOI_4_Prob_Repeat <- May_4_ZOI_4_sum_Repeat/sum12

# May 6
May_6_ZOI_2 <- May_06_Transect_2[51:60,]
May_6_ZOI_2_sum <- sum(May_6_ZOI_2$Sum)
May_6_ZOI_2_Prob <- May_6_ZOI_2_sum/sum13

May_6_ZOI_3 <- May_06_Transect_3[51:60,]
May_6_ZOI_3_sum <- sum(May_6_ZOI_3$Sum)
May_6_ZOI_3_Prob <- May_6_ZOI_3_sum/sum14

May_6_ZOI_4 <- May_06_Transect_4[51:60,]
May_6_ZOI_4_sum <- sum(May_6_ZOI_4$Sum)
May_6_ZOI_4_Prob <- May_6_ZOI_4_sum/sum15

May_6_ZOI_2_Repeat <- May_06_Transect_2_Repeat[51:60,]
May_6_ZOI_2_sum_Repeat <- sum(May_6_ZOI_2_Repeat$Sum)
May_6_ZOI_2_Prob_Repeat <- May_6_ZOI_2_sum_Repeat/sum16

May_6_ZOI_3_Repeat <- May_06_Transect_3_Repeat[51:60,]
May_6_ZOI_3_sum_Repeat <- sum(May_6_ZOI_3_Repeat$Sum)
May_6_ZOI_3_Prob_Repeat <- May_6_ZOI_3_sum_Repeat/sum17

May_6_ZOI_4_Repeat <- May_06_Transect_4_Repeat[51:60,]
May_6_ZOI_4_sum_Repeat <- sum(May_6_ZOI_4$Sum)
May_6_ZOI_4_Prob_Repeat <- May_6_ZOI_4_sum_Repeat/sum18

# May 7
May_7_ZOI_2 <- May_07_Transect_2[51:60,]
May_7_ZOI_2_sum <- sum(May_7_ZOI_2$Sum)
May_7_ZOI_2_Prob <- May_7_ZOI_2_sum/sum19

May_7_ZOI_3 <- May_07_Transect_3[51:60,]
May_7_ZOI_3_sum <- sum(May_7_ZOI_3$Sum)
May_7_ZOI_3_Prob <- May_7_ZOI_3_sum/sum20

May_7_ZOI_4 <- May_07_Transect_4[51:60,]
May_7_ZOI_4_sum <- sum(May_7_ZOI_4$Sum)
May_7_ZOI_4_Prob <- May_7_ZOI_4_sum/sum21

May_7_ZOI_2_Night <- May_07_Transect_2_Night[51:60,]
May_7_ZOI_2_sum_Night <- sum(May_7_ZOI_2_Night$Sum)
May_7_ZOI_2_Prob_Night <- May_7_ZOI_2_sum_Night/sum22

May_7_ZOI_3_Night <- May_07_Transect_3_Night[51:60,]
May_7_ZOI_3_sum_Night <- sum(May_7_ZOI_3_Night$Sum)
May_7_ZOI_3_Prob_Night <- May_7_ZOI_3_sum_Night/sum23

May_7_ZOI_4_Night <- May_07_Transect_4_Night[51:60,]
May_7_ZOI_4_sum_Night <- sum(May_7_ZOI_4$Sum)
May_7_ZOI_4_Prob_Night <- May_7_ZOI_4_sum_Night/sum24

May_7_ZOI_2_Repeat <- May_07_Transect_2_Repeat[51:60,]
May_7_ZOI_2_sum_Repeat <- sum(May_7_ZOI_2_Repeat$Sum)
May_7_ZOI_2_Prob_Repeat <- May_7_ZOI_2_sum_Repeat/sum25

May_7_ZOI_3_Repeat <- May_07_Transect_3_Repeat[51:60,]
May_7_ZOI_3_sum_Repeat <- sum(May_7_ZOI_3_Repeat$Sum)
May_7_ZOI_3_Prob_Repeat <- May_7_ZOI_3_sum_Repeat/sum26

May_7_ZOI_4_Repeat <- May_07_Transect_4_Repeat[51:60,]
May_7_ZOI_4_sum_Repeat <- sum(May_7_ZOI_4$Sum)
May_7_ZOI_4_Prob_Repeat <- May_7_ZOI_4_sum_Repeat/sum27

# May 8
May_8_ZOI_2 <- May_08_Transect_2[51:60,]
May_8_ZOI_2_sum <- sum(May_8_ZOI_2$Sum)
May_8_ZOI_2_Prob <- May_8_ZOI_2_sum/sum28

May_8_ZOI_3 <- May_08_Transect_3[51:60,]
May_8_ZOI_3_sum <- sum(May_8_ZOI_3$Sum)
May_8_ZOI_3_Prob <- May_8_ZOI_3_sum/sum29

May_8_ZOI_4 <- May_08_Transect_4[51:60,]
May_8_ZOI_4_sum <- sum(May_8_ZOI_4$Sum)
May_8_ZOI_4_Prob <- May_8_ZOI_4_sum/sum30

# May 9
May_9_ZOI_2 <- May_09_Transect_2[51:60,]
May_9_ZOI_2_sum <- sum(May_9_ZOI_2$Sum)
May_9_ZOI_2_Prob <- May_9_ZOI_2_sum/sum31

May_9_ZOI_3 <- May_09_Transect_3[51:60,]
May_9_ZOI_3_sum <- sum(May_9_ZOI_3$Sum)
May_9_ZOI_3_Prob <- May_9_ZOI_3_sum/sum32

May_9_ZOI_4 <- May_09_Transect_4[51:60,]
May_9_ZOI_4_sum <- sum(May_9_ZOI_4$Sum)
May_9_ZOI_4_Prob <- May_9_ZOI_4_sum/sum33

# May 10
May_10_ZOI_2 <- May_10_Transect_2[51:60,]
May_10_ZOI_2_sum <- sum(May_10_ZOI_2$Sum)
May_10_ZOI_2_Prob <- May_10_ZOI_2_sum/sum34

May_10_ZOI_3 <- May_10_Transect_3[51:60,]
May_10_ZOI_3_sum <- sum(May_10_ZOI_3$Sum)
May_10_ZOI_3_Prob <- May_10_ZOI_3_sum/sum35

May_10_ZOI_4 <- May_10_Transect_4[51:60,]
May_10_ZOI_4_sum <- sum(May_10_ZOI_4$Sum)
May_10_ZOI_4_Prob <- May_10_ZOI_4_sum/sum36

# May 13
May_13_ZOI_2 <- May_13_Transect_2[51:60,]
May_13_ZOI_2_sum <- sum(May_13_ZOI_2$Sum)
May_13_ZOI_2_Prob <- May_13_ZOI_2_sum/sum37

May_13_ZOI_3 <- May_13_Transect_3[51:60,]
May_13_ZOI_3_sum <- sum(May_13_ZOI_3$Sum)
May_13_ZOI_3_Prob <- May_13_ZOI_3_sum/sum38

May_13_ZOI_4 <- May_13_Transect_4[51:60,]
May_13_ZOI_4_sum <- sum(May_13_ZOI_4$Sum)
May_13_ZOI_4_Prob <- May_13_ZOI_4_sum/sum39

# June 03
June_03_ZOI_2 <- June_03_Transect_2[51:60,]
June_03_ZOI_2_sum <- sum(June_03_ZOI_2$Sum)
June_03_ZOI_2_Prob <- June_03_ZOI_2_sum/sum40

June_03_ZOI_3 <- June_03_Transect_3[51:60,]
June_03_ZOI_3_sum <- sum(June_03_ZOI_3$Sum)
June_03_ZOI_3_Prob <- June_03_ZOI_3_sum/sum41

June_03_ZOI_4 <- June_03_Transect_4[51:60,]
June_03_ZOI_4_sum <- sum(June_03_ZOI_4$Sum)
June_03_ZOI_4_Prob <- June_03_ZOI_4_sum/sum42

# June 04
June_04_ZOI_2 <- June_04_Transect_2[51:60,]
June_04_ZOI_2_sum <- sum(June_04_ZOI_2$Sum)
June_04_ZOI_2_Prob <- June_04_ZOI_2_sum/sum43

June_04_ZOI_3 <- June_04_Transect_3[51:60,]
June_04_ZOI_3_sum <- sum(June_04_ZOI_3$Sum)
June_04_ZOI_3_Prob <- June_04_ZOI_3_sum/sum44

June_04_ZOI_4 <- June_04_Transect_4[51:60,]
June_04_ZOI_4_sum <- sum(June_04_ZOI_4$Sum)
June_04_ZOI_4_Prob <- June_04_ZOI_4_sum/sum45

June_04_ZOI_2_Night <- June_04_Transect_2_Night[51:60,]
June_04_ZOI_2_sum_Night <- sum(June_04_ZOI_2_Night$Sum)
June_04_ZOI_2_Prob_Night <- June_04_ZOI_2_sum_Night/sum46

June_04_ZOI_3_Night <- June_04_Transect_3_Night[51:60,]
June_04_ZOI_3_sum_Night <- sum(June_04_ZOI_3_Night$Sum)
June_04_ZOI_3_Prob_Night <- June_04_ZOI_3_sum_Night/sum47

June_04_ZOI_4_Night <- June_04_Transect_4_Night[51:60,]
June_04_ZOI_4_sum_Night <- sum(June_04_ZOI_4_Night$Sum)
June_04_ZOI_4_Prob_Night <- June_04_ZOI_4_sum_Night/sum48

# June 06
June_06_ZOI_2 <- June_06_Transect_2[51:60,]
June_06_ZOI_2_sum <- sum(June_06_ZOI_2$Sum)
June_06_ZOI_2_Prob <- June_06_ZOI_2_sum/sum49

June_06_ZOI_3 <- June_06_Transect_3[51:60,]
June_06_ZOI_3_sum <- sum(June_06_ZOI_3$Sum)
June_06_ZOI_3_Prob <- June_06_ZOI_3_sum/sum50

June_06_ZOI_4 <- June_06_Transect_4[51:60,]
June_06_ZOI_4_sum <- sum(June_06_ZOI_4$Sum)
June_06_ZOI_4_Prob <- June_06_ZOI_4_sum/sum51

# June 07
June_07_ZOI_2 <- June_07_Transect_2[51:60,]
June_07_ZOI_2_sum <- sum(June_07_ZOI_2$Sum)
June_07_ZOI_2_Prob <- June_07_ZOI_2_sum/sum52

June_07_ZOI_3 <- June_07_Transect_3[51:60,]
June_07_ZOI_3_sum <- sum(June_07_ZOI_3$Sum)
June_07_ZOI_3_Prob <- June_07_ZOI_3_sum/sum53

June_07_ZOI_4 <- June_07_Transect_4[51:60,]
June_07_ZOI_4_sum <- sum(June_07_ZOI_4$Sum)
June_07_ZOI_4_Prob <- June_07_ZOI_4_sum/sum54

# June 08
June_08_ZOI_2 <- June_08_Transect_2[51:60,]
June_08_ZOI_2_sum <- sum(June_08_ZOI_2$Sum)
June_08_ZOI_2_Prob <- June_08_ZOI_2_sum/sum55

June_08_ZOI_3 <- June_08_Transect_3[51:60,]
June_08_ZOI_3_sum <- sum(June_08_ZOI_3$Sum)
June_08_ZOI_3_Prob <- June_08_ZOI_3_sum/sum56

June_08_ZOI_4 <- June_08_Transect_4[51:60,]
June_08_ZOI_4_sum <- sum(June_08_ZOI_4$Sum)
June_08_ZOI_4_Prob <- June_08_ZOI_4_sum/sum57

# June 09
June_09_ZOI_2 <- June_09_Transect_2[51:60,]
June_09_ZOI_2_sum <- sum(June_09_ZOI_2$Sum)
June_09_ZOI_2_Prob <- June_09_ZOI_2_sum/sum58

June_09_ZOI_3 <- June_09_Transect_3[51:60,]
June_09_ZOI_3_sum <- sum(June_09_ZOI_3$Sum)
June_09_ZOI_3_Prob <- June_09_ZOI_3_sum/sum59

June_09_ZOI_4 <- June_09_Transect_4[51:60,]
June_09_ZOI_4_sum <- sum(June_09_ZOI_4$Sum)
June_09_ZOI_4_Prob <- June_09_ZOI_4_sum/sum60

June_09_ZOI_2_Repeat <- June_09_Transect_2_Repeat[51:60,]
June_09_ZOI_2_sum_Repeat <- sum(June_09_ZOI_2_Repeat$Sum)
June_09_ZOI_2_Prob_Repeat <- June_09_ZOI_2_sum_Repeat/sum61

June_09_ZOI_3_Repeat <- June_09_Transect_3_Repeat[51:60,]
June_09_ZOI_3_sum_Repeat <- sum(June_09_ZOI_3_Repeat$Sum)
June_09_ZOI_3_Prob_Repeat <- June_09_ZOI_3_sum_Repeat/sum62

June_09_ZOI_4_Repeat <- June_09_Transect_4_Repeat[51:60,]
June_09_ZOI_4_sum_Repeat <- sum(June_09_ZOI_4_Repeat$Sum)
June_09_ZOI_4_Prob_Repeat <- June_09_ZOI_4_sum_Repeat/sum63

June_09_ZOI_2_Night <- June_09_Transect_2_Night[51:60,]
June_09_ZOI_2_sum_Night <- sum(June_09_ZOI_2_Night$Sum)
June_09_ZOI_2_Prob_Night <- June_09_ZOI_2_sum_Night/sum64

June_09_ZOI_3_Night <- June_09_Transect_3_Night[51:60,]
June_09_ZOI_3_sum_Night <- sum(June_09_ZOI_3_Night$Sum)
June_09_ZOI_3_Prob_Night <- June_09_ZOI_3_sum_Night/sum65

June_09_ZOI_4_Night <- June_09_Transect_4_Night[51:60,]
June_09_ZOI_4_sum_Night <- sum(June_09_ZOI_4_Night$Sum)
June_09_ZOI_4_Prob_Night <- June_09_ZOI_4_sum_Night/sum66

# June 11
June_11_ZOI_2 <- June_11_Transect_2[51:60,]
June_11_ZOI_2_sum <- sum(June_11_ZOI_2$Sum)
June_11_ZOI_2_Prob <- June_11_ZOI_2_sum/sum67

June_11_ZOI_3 <- June_11_Transect_3[51:60,]
June_11_ZOI_3_sum <- sum(June_11_ZOI_3$Sum)
June_11_ZOI_3_Prob <- June_11_ZOI_3_sum/sum68

June_11_ZOI_4 <- June_11_Transect_4[51:60,]
June_11_ZOI_4_sum <- sum(June_11_ZOI_4$Sum)
June_11_ZOI_4_Prob <- June_11_ZOI_4_sum/sum69

June_11_ZOI_2_Repeat <- June_11_Transect_2_Repeat[51:60,]
June_11_ZOI_2_sum_Repeat <- sum(June_11_ZOI_2_Repeat$Sum)
June_11_ZOI_2_Prob_Repeat <- June_11_ZOI_2_sum_Repeat/sum70

June_11_ZOI_3_Repeat <- June_11_Transect_3_Repeat[51:60,]
June_11_ZOI_3_sum_Repeat <- sum(June_11_ZOI_3_Repeat$Sum)
June_11_ZOI_3_Prob_Repeat <- June_11_ZOI_3_sum_Repeat/sum71

June_11_ZOI_4_Repeat <- June_11_Transect_4_Repeat[51:60,]
June_11_ZOI_4_sum_Repeat <- sum(June_11_ZOI_4_Repeat$Sum)
June_11_ZOI_4_Prob_Repeat <- June_11_ZOI_4_sum_Repeat/sum72

June_11_ZOI_2_Night <- June_11_Transect_2_Night[51:60,]
June_11_ZOI_2_sum_Night <- sum(June_11_ZOI_2_Night$Sum)
June_11_ZOI_2_Prob_Night <- June_11_ZOI_2_sum_Night/sum73

June_11_ZOI_3_Night <- June_11_Transect_3_Night[51:60,]
June_11_ZOI_3_sum_Night <- sum(June_11_ZOI_3_Night$Sum)
June_11_ZOI_3_Prob_Night <- June_11_ZOI_3_sum_Night/sum74

June_11_ZOI_4_Night <- June_11_Transect_4_Night[51:60,]
June_11_ZOI_4_sum_Night <- sum(June_11_ZOI_4_Night$Sum)
June_11_ZOI_4_Prob_Night <- June_11_ZOI_4_sum_Night/sum75

# June 13
June_13_ZOI_2 <- June_13_Transect_2[51:60,]
June_13_ZOI_2_sum <- sum(June_13_ZOI_2$Sum)
June_13_ZOI_2_Prob <- June_13_ZOI_2_sum/sum76

June_13_ZOI_3 <- June_13_Transect_3[51:60,]
June_13_ZOI_3_sum <- sum(June_13_ZOI_3$Sum)
June_13_ZOI_3_Prob <- June_13_ZOI_3_sum/sum77

June_13_ZOI_4 <- June_13_Transect_4[51:60,]
June_13_ZOI_4_sum <- sum(June_13_ZOI_4$Sum)
June_13_ZOI_4_Prob <- June_13_ZOI_4_sum/sum78

June_13_ZOI_2_Repeat <- June_13_Transect_2_Repeat[51:60,]
June_13_ZOI_2_sum_Repeat <- sum(June_13_ZOI_2_Repeat$Sum)
June_13_ZOI_2_Prob_Repeat <- June_13_ZOI_2_sum_Repeat/sum79

June_13_ZOI_3_Repeat <- June_13_Transect_3_Repeat[51:60,]
June_13_ZOI_3_sum_Repeat <- sum(June_13_ZOI_3_Repeat$Sum)
June_13_ZOI_3_Prob_Repeat <- June_13_ZOI_3_sum_Repeat/sum80

June_13_ZOI_4_Repeat <- June_13_Transect_4_Repeat[51:60,]
June_13_ZOI_4_sum_Repeat <- sum(June_13_ZOI_4_Repeat$Sum)
June_13_ZOI_4_Prob_Repeat <- June_13_ZOI_4_sum_Repeat/sum81

## All

All_ZOI_Probs <- c(May_2_ZOI_2_Prob, May_2_ZOI_3_Prob, May_2_ZOI_4_Prob,
                   May_4_ZOI_2_Prob, May_4_ZOI_3_Prob, May_4_ZOI_4_Prob, May_4_ZOI_2_Prob_Night, May_4_ZOI_3_Prob_Night, May_4_ZOI_4_Prob_Night, May_4_ZOI_2_Prob_Repeat, May_4_ZOI_3_Prob_Repeat, May_4_ZOI_4_Prob_Repeat,
                   May_6_ZOI_2_Prob, May_6_ZOI_3_Prob, May_6_ZOI_4_Prob, May_6_ZOI_2_Prob_Repeat, May_6_ZOI_3_Prob_Repeat, May_6_ZOI_4_Prob_Repeat,
                   May_7_ZOI_2_Prob, May_7_ZOI_3_Prob, May_7_ZOI_4_Prob, May_7_ZOI_2_Prob_Night, May_7_ZOI_3_Prob_Night, May_7_ZOI_4_Prob_Night, May_7_ZOI_2_Prob_Repeat, May_7_ZOI_3_Prob_Repeat, May_7_ZOI_4_Prob_Repeat,
                   May_8_ZOI_2_Prob, May_8_ZOI_3_Prob, May_8_ZOI_4_Prob,
                   May_9_ZOI_2_Prob, May_9_ZOI_3_Prob, May_9_ZOI_4_Prob,
                   May_10_ZOI_2_Prob, May_10_ZOI_3_Prob, May_10_ZOI_4_Prob,
                   May_13_ZOI_2_Prob, May_13_ZOI_3_Prob, May_13_ZOI_4_Prob,
                   June_03_ZOI_2_Prob, June_03_ZOI_3_Prob, June_03_ZOI_4_Prob,
                   June_04_ZOI_2_Prob, June_04_ZOI_3_Prob, June_04_ZOI_4_Prob, June_04_ZOI_2_Prob_Night, June_04_ZOI_3_Prob_Night, June_04_ZOI_4_Prob,
                   June_06_ZOI_2_Prob, June_06_ZOI_3_Prob, June_06_ZOI_4_Prob,
                   June_07_ZOI_2_Prob, June_07_ZOI_3_Prob, June_07_ZOI_4_Prob,
                   June_08_ZOI_2_Prob, June_08_ZOI_3_Prob, June_08_ZOI_4_Prob,
                   June_09_ZOI_2_Prob, June_09_ZOI_3_Prob, June_09_ZOI_4_Prob, June_09_ZOI_2_Prob_Night, June_09_ZOI_3_Prob_Night, June_09_ZOI_4_Prob_Night, June_09_ZOI_2_Prob_Repeat, June_09_ZOI_3_Prob_Repeat, June_09_ZOI_4_Prob_Repeat,
                   June_11_ZOI_2_Prob, June_11_ZOI_3_Prob, June_11_ZOI_4_Prob, June_11_ZOI_2_Prob_Night, June_11_ZOI_3_Prob_Night, June_11_ZOI_4_Prob_Night, June_11_ZOI_2_Prob_Repeat, June_11_ZOI_3_Prob_Repeat, June_11_ZOI_4_Prob_Repeat,
                   June_13_ZOI_2_Prob, June_13_ZOI_3_Prob, June_13_ZOI_4_Prob, June_13_ZOI_2_Prob_Repeat, June_13_ZOI_3_Prob_Repeat, June_13_ZOI_4_Prob_Repeat)

All_ZOI_Probs <- na.omit(All_ZOI_Probs)
weighted <- as.data.frame(plyr::count(All_ZOI_Probs))
weight_mean_s <- weighted.mean(weighted$x, weighted$freq)

hist(All_ZOI_Probs, breaks = 20, xlab = "Probability", main = "Day & Night Probabilities")
abline(v = weight_mean_s, col = "red")

## Day

All_ZOI_Day <- c(May_2_ZOI_2_Prob, May_2_ZOI_3_Prob, May_2_ZOI_4_Prob,
                 May_4_ZOI_2_Prob, May_4_ZOI_3_Prob, May_4_ZOI_4_Prob, May_4_ZOI_2_Prob_Repeat, May_4_ZOI_3_Prob_Repeat, May_4_ZOI_4_Prob_Repeat,
                 May_6_ZOI_2_Prob, May_6_ZOI_3_Prob, May_6_ZOI_4_Prob, May_6_ZOI_2_Prob_Repeat, May_6_ZOI_3_Prob_Repeat, May_6_ZOI_4_Prob_Repeat,
                 May_7_ZOI_2_Prob, May_7_ZOI_3_Prob, May_7_ZOI_4_Prob, May_7_ZOI_2_Prob_Repeat, May_7_ZOI_3_Prob_Repeat, May_7_ZOI_4_Prob_Repeat,
                 May_8_ZOI_2_Prob, May_8_ZOI_3_Prob, May_8_ZOI_4_Prob,
                 May_9_ZOI_2_Prob, May_9_ZOI_3_Prob, May_9_ZOI_4_Prob,
                 May_10_ZOI_2_Prob, May_10_ZOI_3_Prob, May_10_ZOI_4_Prob,
                 May_13_ZOI_2_Prob, May_13_ZOI_3_Prob, May_13_ZOI_4_Prob,
                 June_03_ZOI_2_Prob, June_03_ZOI_3_Prob, June_03_ZOI_4_Prob,
                 June_04_ZOI_2_Prob, June_04_ZOI_3_Prob, June_04_ZOI_4_Prob,
                 June_06_ZOI_2_Prob, June_06_ZOI_3_Prob, June_06_ZOI_4_Prob,
                 June_07_ZOI_2_Prob, June_07_ZOI_3_Prob, June_07_ZOI_4_Prob,
                 June_08_ZOI_2_Prob, June_08_ZOI_3_Prob, June_08_ZOI_4_Prob,
                 June_09_ZOI_2_Prob, June_09_ZOI_3_Prob, June_09_ZOI_4_Prob, June_09_ZOI_2_Prob_Repeat, June_09_ZOI_3_Prob_Repeat, June_09_ZOI_4_Prob_Repeat,
                 June_11_ZOI_2_Prob, June_11_ZOI_3_Prob, June_11_ZOI_4_Prob, June_11_ZOI_2_Prob_Repeat, June_11_ZOI_3_Prob_Repeat, June_11_ZOI_4_Prob_Repeat,
                 June_13_ZOI_2_Prob, June_13_ZOI_3_Prob, June_13_ZOI_4_Prob, June_13_ZOI_2_Prob_Repeat, June_13_ZOI_3_Prob_Repeat, June_13_ZOI_4_Prob_Repeat)

All_ZOI_Day <- na.omit(All_ZOI_Day)
weighted <- as.data.frame(plyr::count(All_ZOI_Day))
weight_mean_d <- weighted.mean(weighted$x, weighted$freq)

hist(All_ZOI_Day, breaks = 20, xlab = "Probability", main = "Day Probabilities")
abline(v = weight_mean_d, col = "red")

## Night

All_ZOI_Night <- c(May_4_ZOI_2_Prob_Night, May_4_ZOI_3_Prob_Night, May_4_ZOI_4_Prob_Night,
                   May_7_ZOI_2_Prob_Night, May_7_ZOI_3_Prob_Night, May_7_ZOI_4_Prob_Night,
                   June_04_ZOI_2_Prob_Night, June_04_ZOI_3_Prob_Night, June_04_ZOI_4_Prob_Night,
                   June_09_ZOI_2_Prob_Night, June_09_ZOI_3_Prob_Night, June_09_ZOI_4_Prob_Night,
                   June_11_ZOI_2_Prob_Night, June_11_ZOI_3_Prob_Night, June_11_ZOI_4_Prob_Night) 

All_ZOI_Night <- na.omit(All_ZOI_Night)
weighted <- as.data.frame(plyr::count(All_ZOI_Night))
weight_mean_n <- weighted.mean(weighted$x, weighted$freq)

hist(All_ZOI_Night, breaks = 10, xlab = "Probability", main = "Night Probabilities")
abline(v = weight_mean_n, col = "red")

