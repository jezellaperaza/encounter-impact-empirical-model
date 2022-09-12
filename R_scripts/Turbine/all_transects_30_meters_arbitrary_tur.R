#### Obtaining abundances within the ZOI
#### Arbitrary ZOI
#### ZOI: 10 x 10 (150 meters across, 30 meters deep)

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)
transect_area <- 1500*60

setwd("~/UW Summer 2022/EV Exports/Mobile/20x Resolution Exports/3m VBins 15m HBins 30m bottom")

#### Obtaining abundances ####

May_02 <- read.csv("May_02_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(May_02$PRC_ABC)) {
  May_02$abundance[i] <- NA
  May_02$abundance[i] <- May_02$PRC_ABC[i]*transect_area/sigma_bs_herring
}

May_04 <- read.csv("May_04_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(May_04$PRC_ABC)) {
  May_04$abundance[i] <- NA
  May_04$abundance[i] <- May_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

May_06 <- read.csv("May_06_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(May_06$PRC_ABC)) {
  May_06$abundance[i] <- NA
  May_06$abundance[i] <- May_06$PRC_ABC[i]*transect_area/sigma_bs_herring
}

May_07 <- read.csv("May_07_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(May_07$PRC_ABC)) {
  May_07$abundance[i] <- NA
  May_07$abundance[i] <- May_07$PRC_ABC[i]*transect_area/sigma_bs_herring
}

May_08 <- read.csv("May_08_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(May_08$PRC_ABC)) {
  May_08$abundance[i] <- NA
  May_08$abundance[i] <- May_08$PRC_ABC[i]*transect_area/sigma_bs_herring
}

May_09 <- read.csv("May_09_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(May_09$PRC_ABC)) {
  May_09$abundance[i] <- NA
  May_09$abundance[i] <- May_09$PRC_ABC[i]*transect_area/sigma_bs_herring
}

May_10 <- read.csv("May_10_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(May_10$PRC_ABC)) {
  May_10$abundance[i] <- NA
  May_10$abundance[i] <- May_10$PRC_ABC[i]*transect_area/sigma_bs_herring
}

May_13 <- read.csv("May_13_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(May_13$PRC_ABC)) {
  May_13$abundance[i] <- NA
  May_13$abundance[i] <- May_13$PRC_ABC[i]*transect_area/sigma_bs_herring
}

June_03 <- read.csv("June_03_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(June_03$PRC_ABC)) {
  June_03$abundance[i] <- NA
  June_03$abundance[i] <- June_03$PRC_ABC[i]*transect_area/sigma_bs_herring
}

June_04 <- read.csv("June_04_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(June_04$PRC_ABC)) {
  June_04$abundance[i] <- NA
  June_04$abundance[i] <- June_04$PRC_ABC[i]*transect_area/sigma_bs_herring
}

June_06 <- read.csv("June_06_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(June_06$PRC_ABC)) {
  June_06$abundance[i] <- NA
  June_06$abundance[i] <- June_06$PRC_ABC[i]*transect_area/sigma_bs_herring
}

June_07 <- read.csv("June_07_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(June_07$PRC_ABC)) {
  June_07$abundance[i] <- NA
  June_07$abundance[i] <- June_07$PRC_ABC[i]*transect_area/sigma_bs_herring
}

June_08 <- read.csv("June_08_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(June_08$PRC_ABC)) {
  June_08$abundance[i] <- NA
  June_08$abundance[i] <- June_08$PRC_ABC[i]*transect_area/sigma_bs_herring
}

June_09 <- read.csv("June_09_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(June_09$PRC_ABC)) {
  June_09$abundance[i] <- NA
  June_09$abundance[i] <- June_09$PRC_ABC[i]*transect_area/sigma_bs_herring
}

June_11 <- read.csv("June_11_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(June_11$PRC_ABC)) {
  June_11$abundance[i] <- NA
  June_11$abundance[i] <- June_11$PRC_ABC[i]*transect_area/sigma_bs_herring
}

June_13 <- read.csv("June_13_3m_VBins_15m_HBins_30m_bottom.csv")

for (i in 1:length(June_13$PRC_ABC)) {
  June_13$abundance[i] <- NA
  June_13$abundance[i] <- June_13$PRC_ABC[i]*transect_area/sigma_bs_herring
}

#### Summarizing ####

summarize_function <- function(Dist_S, abundance, data) {
  data %>%
    group_by(Region_name, Dist_S) %>%
    dplyr::summarise(Sum = sum(abundance))
}

May_02_new <- summarize_function(May_02$Dist_S, May_02$abundance, May_02)
May_04_new <- summarize_function(May_04$Dist_S, May_04$abundance, May_04)
May_06_new <- summarize_function(May_06$Dist_S, May_06$abundance, May_06)
May_07_new <- summarize_function(May_07$Dist_S, May_07$abundance, May_07)
May_08_new <- summarize_function(May_08$Dist_S, May_08$abundance, May_08)
May_09_new <- summarize_function(May_09$Dist_S, May_09$abundance, May_09)
May_10_new <- summarize_function(May_10$Dist_S, May_10$abundance, May_10)
May_13_new <- summarize_function(May_13$Dist_S, May_13$abundance, May_13)

June_03_new <- summarize_function(June_03$Dist_S, June_03$abundance, June_03)
June_04_new <- summarize_function(June_04$Dist_S, June_04$abundance, June_04)
June_06_new <- summarize_function(June_06$Dist_S, June_06$abundance, June_06)
June_07_new <- summarize_function(June_07$Dist_S, June_07$abundance, June_07)
June_08_new <- summarize_function(June_08$Dist_S, June_08$abundance, June_08)
June_09_new <- summarize_function(June_09$Dist_S, June_09$abundance, June_09)
June_11_new <- summarize_function(June_11$Dist_S, June_11$abundance, June_11)
June_13_new <- summarize_function(June_13$Dist_S, June_13$abundance, June_13)

#### Total number of fish in each transect ####

sum_function <- function(data) {
  data %>%
    group_by(Region_name) %>%
    dplyr::summarise(Sum_region = sum(Sum))
}

May_02_Sum <- sum_function(May_02_new)
May_04_Sum <- sum_function(May_04_new)
May_06_Sum <- sum_function(May_06_new)
May_07_Sum <- sum_function(May_07_new)
May_08_Sum <- sum_function(May_08_new)
May_09_Sum <- sum_function(May_09_new)
May_10_Sum <- sum_function(May_10_new)
May_13_Sum <- sum_function(May_13_new)
June_03_Sum <- sum_function(June_03_new)
June_04_Sum <- sum_function(June_04_new)
June_06_Sum <- sum_function(June_06_new)
June_07_Sum <- sum_function(June_07_new)
June_08_Sum <- sum_function(June_08_new)
June_09_Sum <- sum_function(June_09_new)
June_11_Sum <- sum_function(June_11_new)
June_13_Sum <- sum_function(June_13_new)

#### Merge data frames ####

May_02_Transects <- merge(May_02_new, May_02_Sum, by = "Region_name")
May_04_Transects <- merge(May_04_new, May_04_Sum, by = "Region_name")
May_06_Transects <- merge(May_06_new, May_06_Sum, by = "Region_name")
May_07_Transects <- merge(May_07_new, May_07_Sum, by = "Region_name")
May_08_Transects <- merge(May_08_new, May_08_Sum, by = "Region_name")
May_09_Transects <- merge(May_09_new, May_09_Sum, by = "Region_name")
May_10_Transects <- merge(May_10_new, May_10_Sum, by = "Region_name")
May_13_Transects <- merge(May_13_new, May_13_Sum, by = "Region_name")
June_03_Transects <- merge(June_03_new, June_03_Sum, by = "Region_name")
June_04_Transects <- merge(June_04_new, June_04_Sum, by = "Region_name")
June_06_Transects <- merge(June_06_new, June_06_Sum, by = "Region_name")
June_07_Transects <- merge(June_07_new, June_07_Sum, by = "Region_name")
June_08_Transects <- merge(June_08_new, June_08_Sum, by = "Region_name")
June_09_Transects <- merge(June_09_new, June_09_Sum, by = "Region_name")
June_11_Transects <- merge(June_11_new, June_11_Sum, by = "Region_name")
June_13_Transects <- merge(June_13_new, June_13_Sum, by = "Region_name")

#### Extracting ZOI region ####

## May 02 ##

dplyr::count(May_02_Transects, Region_name)

start_pos_may_02 <- list(Grid_A_N_5_02_01 = 0, Grid_A_N_5_02_02 = 88, Grid_A_N_5_02_03 = 178,
                         Grid_A_N_5_02_04 = 280, Grid_A_N_5_02_05 = 385, Grid_A_N_5_02_06 = 495,
                         Grid_A_N_5_02_07 = 608, Grid_A_N_5_02_08 = 757, Grid_A_N_5_02_09 = 918, 
                         Grid_A_N_5_02_10 = 1091, Grid_A_N_5_02_11 = 1262, Grid_A_N_5_02_12 = 1439)

May_02_List <- list()
for (i in 1:length(start_pos_may_02)) {
  May_02_List[i] <- list(May_02_Transects[(start_pos_may_02[[i]] + 50):(start_pos_may_02[[i]] + 61),])
}

May_02_Probs <- vector()
for (i in 1:length(May_02_List)) {
  May_02_Probs[i] <- sum(May_02_List[[i]]$Sum)/May_02_List[[i]]$Sum_region[1]
}

## May 04 ##

dplyr::count(May_04_Transects, Region_name)

start_pos_may_04 <- list(GRID_A_N_5_04_01 = 0, GRID_A_N_5_04_02 = 87, GRID_A_N_5_04_03 = 178,
                         GRID_A_N_5_04_04 = 278, GRID_A_N_5_04_05 = 381, GRID_A_N_5_04_06 = 491,
                         GRID_A_N_5_04_07 = 608, GRID_A_N_5_04_08 = 759, GRID_A_N_5_04_09 = 922, 
                         GRID_A_N_5_04_10 = 1094, GRID_A_N_5_04_11 = 1268, GRID_A_N_5_04_12 = 1447,
                         GRID_A_N_NIGHT_5_04_01 = 1624, GRID_A_N_NIGHT_5_04_02 = 1713, GRID_A_N_NIGHT_5_04_03 = 1805,
                         GRID_A_N_NIGHT_5_04_04 = 1905, GRID_A_N_NIGHT_5_04_05 = 2007, GRID_A_N_NIGHT_5_04_06 = 2116,
                         GRID_A_N_NIGHT_5_04_07 = 2229, GRID_A_N_NIGHT_5_04_08 = 2379, GRID_A_N_NIGHT_5_04_09 = 2539, 
                         GRID_A_N_NIGHT_5_04_10 = 2714, GRID_A_N_NIGHT_5_04_11 = 2889, GRID_A_N_NIGHT_5_04_12 = 3063,
                         GRID_A_N_REP_5_04_01 = 3243, GRID_A_N_REP_5_04_02 = 3333, GRID_A_N_REP_5_04_03 = 3421, 
                         GRID_A_N_REP_5_04_04 = 3519, GRID_A_N_REP_5_04_05 = 3619, GRID_A_N_REP_5_04_06 = 3725,
                         GRID_A_N_REP_5_04_07 = 3843, GRID_A_N_REP_5_04_08 = 3991, GRID_A_N_REP_5_04_09 = 4156,
                         GRID_A_N_REP_5_04_10 = 4328, GRID_A_N_REP_5_04_11 = 4501, GRID_A_N_REP_5_04_12 = 4679)

May_04_List <- list()
for (i in 1:length(start_pos_may_04)) {
  May_04_List[i] <- list(May_04_Transects[(start_pos_may_04[[i]] + 50):(start_pos_may_04[[i]] + 61),])
}

May_04_Probs <- vector()
for (i in 1:length(May_04_List)) {
  May_04_Probs[i] <- sum(May_04_List[[i]]$Sum)/May_04_List[[i]]$Sum_region[1]
}

## May 06 ##

dplyr::count(May_06_Transects, Region_name)

start_pos_may_06 <- list(GRID_A_N_5_06_01 = 0, GRID_A_N_5_06_02 = 92, GRID_A_N_5_06_03 = 183,
                         GRID_A_N_5_06_04 = 285, GRID_A_N_5_06_05 = 387, GRID_A_N_5_06_06 = 498,
                         GRID_A_N_5_06_07 = 616, GRID_A_N_5_06_08 = 766, GRID_A_N_5_06_09 = 928, 
                         GRID_A_N_5_06_10 = 1100, GRID_A_N_5_06_11 = 1274, GRID_A_N_5_06_12 = 1450,
                         GRID_A_N_NIGHT_5_06_12 = 1628, GRID_A_N_REP_5_06_01 = 1763, GRID_A_N_REP_5_06_02 = 1946,
                         GRID_A_N_REP_5_06_03 = 2128, GRID_A_N_REP_5_06_04 = 2329, GRID_A_N_REP_5_06_05 = 2528,
                         GRID_A_N_REP_5_06_06 = 2747, GRID_A_N_REP_5_06_07 = 2985, GRID_A_N_REP_5_06_08 = 3285,
                         GRID_A_N_REP_5_06_09 = 3613, GRID_A_N_REP_5_06_10 = 3899, GRID_A_N_REP_5_06_11 = 4066)

May_06_List <- list()
for (i in 1:length(start_pos_may_06)) {
  May_06_List[i] <- list(May_06_Transects[(start_pos_may_06[[i]] + 50):(start_pos_may_06[[i]] + 61),])
}

May_06_Probs <- vector()
for (i in 1:length(May_06_List)) {
  May_06_Probs[i] <- sum(May_06_List[[i]]$Sum)/May_06_List[[i]]$Sum_region[1]
}

## May 07 ##

dplyr::count(May_07_Transects, Region_name)

start_pos_may_07 <- list(GRID_A_N_5_07_01 = 0, GRID_A_N_5_07_02 = 89, GRID_A_N_5_07_03 = 179,
                         GRID_A_N_5_07_04 = 280, GRID_A_N_5_07_05 = 382, GRID_A_N_5_07_06 = 489,
                         GRID_A_N_5_07_07 = 605, GRID_A_N_5_07_08 = 754, GRID_A_N_5_07_09 = 915, 
                         GRID_A_N_5_07_10 = 1088, GRID_A_N_5_07_11 = 1263, GRID_A_N_5_07_12 = 1441,
                         GRID_A_N_NIGHT_5_07_01 = 1620, GRID_A_N_NIGHT_5_07_02 = 1709, GRID_A_N_NIGHT_5_07_03 = 1806,
                         GRID_A_N_NIGHT_5_07_04 = 1906, GRID_A_N_NIGHT_5_07_05 = 2007, GRID_A_N_NIGHT_5_07_06 = 2118,
                         GRID_A_N_NIGHT_5_07_07 = 2236, GRID_A_N_NIGHT_5_07_08 = 2387, GRID_A_N_NIGHT_5_07_09 = 2549,
                         GRID_A_N_NIGHT_5_07_10 = 2723, GRID_A_N_NIGHT_5_07_11 = 2899, GRID_A_N_NIGHT_5_07_12 = 3108,
                         GRID_A_N_REP_5_07_01 = 3289, GRID_A_N_REP_5_07_02 = 3380, GRID_A_N_REP_5_07_03 = 3473, 
                         GRID_A_N_REP_5_07_04 = 3573, GRID_A_N_REP_5_07_05 = 3677, GRID_A_N_REP_5_07_06 = 3786,
                         GRID_A_N_REP_5_07_07 = 3904, GRID_A_N_REP_5_07_08 = 4054, GRID_A_N_REP_5_07_09 = 4217,
                         GRID_A_N_REP_5_07_10 = 4390, GRID_A_N_REP_5_07_11 = 4564, GRID_A_N_REP_5_07_12 = 4742)

May_07_List <- list()
for (i in 1:length(start_pos_may_07)) {
  May_07_List[i] <- list(May_07_Transects[(start_pos_may_07[[i]] + 50):(start_pos_may_07[[i]] + 61),])
}

May_07_Probs <- vector()
for (i in 1:length(May_07_List)) {
  May_07_Probs[i] <- sum(May_07_List[[i]]$Sum)/May_07_List[[i]]$Sum_region[1]
}

## May 08 ##

dplyr::count(May_08_Transects, Region_name)

start_pos_may_08 <- list(GRID_A_N_5_08_01 = 0, GRID_A_N_5_08_02 = 92, GRID_A_N_5_08_03 = 184,
                         GRID_A_N_5_08_04 = 286, GRID_A_N_5_08_05 = 389, GRID_A_N_5_08_06 = 503,
                         GRID_A_N_5_08_07 = 623, GRID_A_N_5_08_08 = 775, GRID_A_N_5_08_09 = 940,
                         GRID_A_N_5_08_10 = 1120, GRID_A_N_5_08_11 = 1294, GRID_A_N_5_08_12 = 1471)

May_08_List <- list()
for (i in 1:length(start_pos_may_08)) {
  May_08_List[i] <- list(May_08_Transects[(start_pos_may_08[[i]] + 50):(start_pos_may_08[[i]] + 61),])
}

May_08_Probs <- vector()
for (i in 1:length(May_08_List)) {
  May_08_Probs[i] <- sum(May_08_List[[i]]$Sum)/May_08_List[[i]]$Sum_region[1]
}

## May 09 ##

dplyr::count(May_09_Transects, Region_name)

start_pos_may_09 <- list(GRID_A_N_5_09_01 = 0, GRID_A_N_5_09_02 = 87, GRID_A_N_5_09_03 = 178,
                         GRID_A_N_5_09_04 = 278, GRID_A_N_5_09_05 = 386, GRID_A_N_5_09_06 = 496, 
                         GRID_A_N_5_09_12 = 617)

May_09_List <- list()
for (i in 1:length(start_pos_may_09)) {
  May_09_List[i] <- list(May_09_Transects[(start_pos_may_09[[i]] + 50):(start_pos_may_09[[i]] + 61),])
}

May_09_Probs <- vector()
for (i in 1:length(May_09_List)) {
  May_09_Probs[i] <- sum(May_09_List[[i]]$Sum)/May_09_List[[i]]$Sum_region[1]
}

## May 10 ##

dplyr::count(May_10_Transects, Region_name)

start_pos_may_10 <- list(GRID_A_N_5_10_01 = 0 , GRID_A_N_5_10_02 = 87, GRID_A_N_5_10_03 = 173,
                         GRID_A_N_5_10_04 = 277, GRID_A_N_5_10_05 = 380, GRID_A_N_5_10_06 = 492,
                         GRID_A_N_5_10_07 = 611, GRID_A_N_5_10_08 = 760, GRID_A_N_5_10_09 = 922,
                         GRID_A_N_5_10_10 = 1096, GRID_A_N_5_10_11 = 1271, GRID_A_N_5_10_12 = 1448)

May_10_List <- list()
for (i in 1:length(start_pos_may_10)) {
  May_10_List[i] <- list(May_10_Transects[(start_pos_may_10[[i]] + 50):(start_pos_may_10[[i]] + 61),])
}

May_10_Probs <- vector()
for (i in 1:length(May_10_List)) {
  May_10_Probs[i] <- sum(May_10_List[[i]]$Sum)/May_10_List[[i]]$Sum_region[1]
}

## May 13 ##

dplyr::count(May_13_Transects, Region_name)

start_pos_may_13 <- list(GRID_A_N_5_13_0 = 0, GRID_A_N_5_13_01 = 92, GRID_A_N_5_13_03 = 183,
                         GRID_A_N_5_13_04 = 284, GRID_A_N_5_13_05 = 386, GRID_A_N_5_13_06 = 495,
                         GRID_A_N_5_13_07 = 613, GRID_A_N_5_13_08 = 761, GRID_A_N_5_13_09 = 924,
                         GRID_A_N_5_13_10 = 1097, GRID_A_N_5_13_11 = 1271, GRID_A_N_5_13_12 = 1449)

May_13_List <- list()
for (i in 1:length(start_pos_may_13)) {
  May_13_List[i] <- list(May_13_Transects[(start_pos_may_13[[i]] + 50):(start_pos_may_13[[i]] + 61),])
}

May_13_Probs <- vector()
for (i in 1:length(May_13_List)) {
  May_13_Probs[i] <- sum(May_13_List[[i]]$Sum)/May_13_List[[i]]$Sum_region[1]
}

## June 03 ## 

dplyr::count(June_03_Transects, Region_name)

start_pos_June_03 <- list(GRID_B_N_6_03_01 = 0, GRID_B_N_6_03_02 = 92, GRID_B_N_6_03_03 = 184,
                          GRID_B_N_6_03_04 = 286, GRID_B_N_6_03_05 = 383, GRID_B_N_6_03_06 = 493,
                          GRID_B_N_6_03_07 = 611, GRID_B_N_6_03_08 = 760, GRID_B_N_6_03_09 = 923,
                          GRID_B_N_6_03_10 = 1096, GRID_B_N_6_03_11 = 1271, GRID_B_N_6_03_12 = 1447)

June_03_List <- list()
for (i in 1:length(start_pos_June_03)) {
  June_03_List[i] <- list(June_03_Transects[(start_pos_June_03[[i]] + 50):(start_pos_June_03[[i]] + 61),])
}

June_03_Probs <- vector()
for (i in 1:length(June_03_List)) {
  June_03_Probs[i] <- sum(June_03_List[[i]]$Sum)/June_03_List[[i]]$Sum_region[1]
}

## June 04 ## 

dplyr::count(June_04_Transects, Region_name)

start_pos_June_04 <- list(GRID_B_N_6_04_01 = 0, GRID_B_N_6_04_02 = 89, GRID_B_N_6_04_03 = 182,
                          GRID_B_N_6_04_04 = 284, GRID_B_N_6_04_05 = 385, GRID_B_N_6_04_06 = 496,
                          GRID_B_N_6_04_07 = 613, GRID_B_N_6_04_08 = 762, GRID_B_N_6_04_09 = 922, 
                          GRID_B_N_6_04_10 = 1094, GRID_B_N_6_04_11 = 1266, GRID_B_N_6_04_12 = 1442,
                          GRID_B_N_NIGHT_6_04_01 = 1620, GRID_B_N_NIGHT_6_04_02 = 1711, GRID_B_N_NIGHT_6_04_03 = 1802,
                          GRID_B_N_NIGHT_6_04_04 = 1905, GRID_B_N_NIGHT_6_04_05 = 2009, GRID_B_N_NIGHT_6_04_06 = 2119,
                          GRID_B_N_NIGHT_6_04_07 = 2236, GRID_B_N_NIGHT_6_04_08 = 2386, GRID_B_N_NIGHT_6_04_09 = 2551,
                          GRID_B_N_NIGHT_6_04_10 = 2728, GRID_B_N_NIGHT_6_04_11 = 2905, GRID_B_N_NIGHT_6_04_12 = 3082)

June_04_List <- list()
for (i in 1:length(start_pos_June_04)) {
  June_04_List[i] <- list(June_04_Transects[(start_pos_June_04[[i]] + 50):(start_pos_June_04[[i]] + 61),])
}

June_04_Probs <- vector()
for (i in 1:length(June_04_List)) {
  June_04_Probs[i] <- sum(June_04_List[[i]]$Sum)/June_04_List[[i]]$Sum_region[1]
}

## June 06 ## 

dplyr::count(June_06_Transects, Region_name)

start_pos_June_06 <- list(GRID_B_N_6_06_01 = 0, GRID_B_N_6_06_02 = 95, GRID_B_N_6_06_03 = 190,
                          GRID_B_N_6_06_04 = 291, GRID_B_N_6_06_05 = 394, GRID_B_N_6_06_06 = 504,
                          GRID_B_N_6_06_07 = 623, GRID_B_N_6_06_08 = 777, GRID_B_N_6_06_09 = 940,
                          GRID_B_N_6_06_10 = 1114, GRID_B_N_6_06_11 = 1289, GRID_B_N_6_06_12 = 1470)
June_06_List <- list()
for (i in 1:length(start_pos_June_06)) {
  June_06_List[i] <- list(June_06_Transects[(start_pos_June_06[[i]] + 50):(start_pos_June_06[[i]] + 61),])
}

June_06_Probs <- vector()
for (i in 1:length(June_06_List)) {
  June_06_Probs[i] <- sum(June_06_List[[i]]$Sum)/June_06_List[[i]]$Sum_region[1]
}

## June 07 ## 

dplyr::count(June_07_Transects, Region_name)

start_pos_June_07 <- list(GRID_B_N_6_07_01 = 0, GRID_B_N_6_07_02 = 74, GRID_B_N_6_07_03 = 171,
                          GRID_B_N_6_07_04 = 269, GRID_B_N_6_07_05 = 373, GRID_B_N_6_07_06 = 485, 
                          GRID_B_N_6_07_07 = 606, GRID_B_N_6_07_08 = 756, GRID_B_N_6_07_09 = 919, 
                          GRID_B_N_6_07_10 = 1092, GRID_B_N_6_07_11 = 1266, GRID_B_N_6_07_12 = 1444)
June_07_List <- list()
for (i in 1:length(start_pos_June_07)) {
  June_07_List[i] <- list(June_07_Transects[(start_pos_June_07[[i]] + 50):(start_pos_June_07[[i]] + 61),])
}

June_07_Probs <- vector()
for (i in 1:length(June_07_List)) {
  June_07_Probs[i] <- sum(June_07_List[[i]]$Sum)/June_07_List[[i]]$Sum_region[1]
}

## June 08 ## 

dplyr::count(June_08_Transects, Region_name)

start_pos_June_08 <- list(GRID_B_N_6_08_01 = 0, GRID_B_N_6_08_02 = 88, GRID_B_N_6_08_03 = 178,
                          GRID_B_N_6_08_04 = 275, GRID_B_N_6_08_05 = 378, GRID_B_N_6_08_06 = 487,
                          GRID_B_N_6_08_07 = 606, GRID_B_N_6_08_08 = 755, GRID_B_N_6_08_09 = 917, 
                          GRID_B_N_6_08_10 = 1089, GRID_B_N_6_08_11 = 1264, GRID_B_N_6_08_12 = 1441)
June_08_List <- list()
for (i in 1:length(start_pos_June_08)) {
  June_08_List[i] <- list(June_08_Transects[(start_pos_June_08[[i]] + 50):(start_pos_June_08[[i]] + 61),])
}

June_08_Probs <- vector()
for (i in 1:length(June_08_List)) {
  June_08_Probs[i] <- sum(June_08_List[[i]]$Sum)/June_08_List[[i]]$Sum_region[1]
}

## June 09 ## 

dplyr::count(June_09_Transects, Region_name)

start_pos_June_09 <- list(GRID_B_N_6_09_01 = 0, GRID_B_N_6_09_02 = 90, GRID_B_N_6_09_03 = 180,
                          GRID_B_N_6_09_04 = 279, GRID_B_N_6_09_05 = 382, GRID_B_N_6_09_06 = 492,
                          GRID_B_N_6_09_07 = 610, GRID_B_N_6_09_08 = 758, GRID_B_N_6_09_09 = 921, 
                          GRID_B_N_6_09_11 = 1093, GRID_B_N_6_09_12 = 1265, GRID_B_N_NIGHT_6_09_01 = 1441,
                          GRID_B_N_NIGHT_6_09_02 = 1534, GRID_B_N_NIGHT_6_09_03 = 1627, GRID_B_N_NIGHT_6_09_04 = 1730,
                          GRID_B_N_NIGHT_6_09_05 = 1832, GRID_B_N_NIGHT_6_09_06 = 1943, GRID_B_N_NIGHT_6_09_07 = 2061,
                          GRID_B_N_NIGHT_6_09_08 = 2211, GRID_B_N_NIGHT_6_09_09 = 2373, GRID_B_N_NIGHT_6_09_10 = 2547,
                          GRID_B_N_NIGHT_6_09_11 = 2722, GRID_B_N_NIGHT_6_09_12 = 2900, GRID_B_N_REP_6_09_0 = 3078,
                          GRID_B_N_REP_6_09_01 = 3170, GRID_B_N_REP_6_09_03 = 3261, GRID_B_N_REP_6_09_04 = 3362,
                          GRID_B_N_REP_6_09_05 = 3467, GRID_B_N_REP_6_09_06 = 3576, GRID_B_N_REP_6_09_07 = 3694,
                          GRID_B_N_REP_6_09_08 = 3845, GRID_B_N_REP_6_09_09 = 4006, GRID_B_N_REP_6_09_10 = 4179,
                          GRID_B_N_REP_6_09_11 = 4353, GRID_B_N_REP_6_09_12 = 4531)
June_09_List <- list()
for (i in 1:length(start_pos_June_09)) {
  June_09_List[i] <- list(June_09_Transects[(start_pos_June_09[[i]] + 50):(start_pos_June_09[[i]] + 61),])
}

June_09_Probs <- vector()
for (i in 1:length(June_09_List)) {
  June_09_Probs[i] <- sum(June_09_List[[i]]$Sum)/June_09_List[[i]]$Sum_region[1]
}

## June 11 ## 

dplyr::count(June_11_Transects, Region_name)

start_pos_June_11 <- list(GRID_B_N_6_11_01 = 0, GRID_B_N_6_11_02 = 93, GRID_B_N_6_11_03 = 184,
                          GRID_B_N_6_11_04 = 284, GRID_B_N_6_11_05 = 387, GRID_B_N_6_11_06 = 497,
                          GRID_B_N_6_11_07 = 615, GRID_B_N_6_11_08 = 765, GRID_B_N_6_11_09 = 927, 
                          GRID_B_N_6_11_10 = 1099, GRID_B_N_6_11_11 = 1273, GRID_B_N_6_11_12 = 1448,
                          GRID_B_N_NIGHT_6_11_01 = 1625, GRID_B_N_NIGHT_6_11_02 = 1716, GRID_B_N_NIGHT_6_11_03 = 1792,
                          GRID_B_N_NIGHT_6_11_04 = 1893, GRID_B_N_NIGHT_6_11_05 = 1995, GRID_B_N_NIGHT_6_11_06 = 2105,
                          GRID_B_N_NIGHT_6_11_07 = 2222, GRID_B_N_NIGHT_6_11_08 = 2372, GRID_B_N_NIGHT_6_11_09 = 2536,
                          GRID_B_N_NIGHT_6_11_10 = 2711, GRID_B_N_NIGHT_6_11_11 = 2884, GRID_B_N_NIGHT_6_11_12 = 3067,
                          GRID_B_N_REP_6_11_01 = 3245, GRID_B_N_REP_6_11_02 = 3336, GRID_B_N_REP_6_11_03 = 3428,
                          GRID_B_N_REP_6_11_04 = 3526, GRID_B_N_REP_6_11_05 = 3629, GRID_B_N_REP_6_11_06 = 3737,
                          GRID_B_N_REP_6_11_07 = 3856, GRID_B_N_REP_6_11_08 = 4004, GRID_B_N_REP_6_11_09 = 4166,
                          GRID_B_N_REP_6_11_10 = 4338, GRID_B_N_REP_6_11_11 = 4511, GRID_B_N_REP_6_11_12 = 4687)
June_11_List <- list()
for (i in 1:length(start_pos_June_11)) {
  June_11_List[i] <- list(June_11_Transects[(start_pos_June_11[[i]] + 50):(start_pos_June_11[[i]] + 61),])
}

June_11_Probs <- vector()
for (i in 1:length(June_11_List)) {
  June_11_Probs[i] <- sum(June_11_List[[i]]$Sum)/June_11_List[[i]]$Sum_region[1]
}

## June 13 ## 

dplyr::count(June_13_Transects, Region_name)

start_pos_June_13 <- list(GRID_B_N_6_13_01 = 0, GRID_B_N_6_13_02 = 88, GRID_B_N_6_13_03 = 175,
                          GRID_B_N_6_13_04 = 276, GRID_B_N_6_13_05 = 374, GRID_B_N_6_13_06 = 484,
                          GRID_B_N_6_13_07 = 600, GRID_B_N_6_13_08 = 749, GRID_B_N_6_13_09 = 910, 
                          GRID_B_N_6_13_10 = 1082, GRID_B_N_6_13_11 = 1257, GRID_B_N_6_13_12 = 1433,
                          GRID_B_N_REP_6_13_01 = 1609, GRID_B_N_REP_6_13_02 = 1699, GRID_B_N_REP_6_13_03 = 1792,
                          GRID_B_N_REP_6_13_04 = 1893, GRID_B_N_REP_6_13_05 = 1995, GRID_B_N_REP_6_13_06 = 2104,
                          GRID_B_N_REP_6_13_07 = 2221, GRID_B_N_REP_6_13_08 = 2370, GRID_B_N_REP_6_13_09 = 2531,
                          GRID_B_N_REP_6_13_10 = 2702, GRID_B_N_REP_6_13_11 = 2875, GRID_B_N_REP_6_13_12 = 3051)
June_13_List <- list()
for (i in 1:length(start_pos_June_13)) {
  June_13_List[i] <- list(June_13_Transects[(start_pos_June_13[[i]] + 50):(start_pos_June_13[[i]] + 61),])
}

June_13_Probs <- vector()
for (i in 1:length(June_13_List)) {
  June_13_Probs[i] <- sum(June_13_List[[i]]$Sum)/June_13_List[[i]]$Sum_region[1]
}

#### Combining probabilities ####

all_transects_probs <- na.omit(c(May_02_Probs, May_04_Probs, May_06_Probs, May_07_Probs,
                                 May_08_Probs, May_09_Probs, May_10_Probs, May_13_Probs,
                                 June_03_Probs, June_04_Probs, June_06_Probs, June_07_Probs,
                                 June_08_Probs, June_09_Probs, June_11_Probs, June_13_Probs))

weighted_data <- as.data.frame(plyr::count(all_transects_probs))
weight_mean <- weighted.mean(weighted_data$x, weighted_data$freq)

hist(all_transects_probs, breaks = 20, xlab = "Probability", main = "Day & Night Probabilities")
abline(v = weight_mean, col = "red")
