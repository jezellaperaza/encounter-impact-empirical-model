## Abundance of schools
## August 2022
## In progress - 8/16/22

avg_herring_length_cm <- 15.5
TS_herring <- 26.2*log10(avg_herring_length_cm) - 72.5
sigma_bs_herring <- 10^(TS_herring/10)
area <- 1500*60

#### Default (Sv) ####

## Reading in transects

setwd("~/UW Summer 2022/EV Exports/Mobile/School Detection/Default")

June_03 <- read.csv("June_03.csv")
June_04 <- read.csv("June_04.csv")
June_06 <- read.csv("June_06.csv")
June_07 <- read.csv("June_07.csv")
June_08 <- read.csv("June_09.csv")
June_11 <- read.csv("June_11.csv")
June_13 <- read.csv("June_13.csv")
May_02 <- read.csv("May_02.csv")
May_04 <- read.csv("May_04.csv")
May_06 <- read.csv("May_06.csv")
May_07 <- read.csv("May_07.csv")
May_08 <- read.csv("May_08.csv")
May_09 <- read.csv("May_09.csv")
May_10 <- read.csv("May_10.csv")
May_13 <- read.csv("May_13.csv")

## finding abundance of each school in each transect
## create the function

sv_abundance <- function(SV) {
  lil_sv <- 10^(SV/10)
  abundance <- lil_sv/sigma_bs_herring
  return(abundance)
}

abundance <- function(ABC) {
  linear <- 10^(ABC/10)
  for (i in length(ABC)) {
    
  }
  abundance <- linear/sigma_bs_herring
  return(abundance)
}

abundance(June_03$Area_Backscatter_Strength)

## June - Day & Night
vec1 <- sv_abundance(June_03$Sv_mean)
vec2 <- sv_abundance(June_04$Sv_mean)
vec3 <- sv_abundance(June_06$Sv_mean)
vec4 <- sv_abundance(June_07$Sv_mean)
vec5 <- sv_abundance(June_08$Sv_mean)
vec6 <- sv_abundance(June_11$Sv_mean)
vec7 <- sv_abundance(June_13$Sv_mean)

## May - Day & Night
vec8 <- sv_abundance(May_02$Sv_mean)
vec9 <- sv_abundance(May_04$Sv_mean)
vec10 <- sv_abundance(May_06$Sv_mean)
vec11 <- sv_abundance(May_07$Sv_mean)
vec12 <- sv_abundance(May_08$Sv_mean)
vec13 <- sv_abundance(May_09$Sv_mean)
vec14 <- sv_abundance(May_10$Sv_mean)
vec15 <- sv_abundance(May_13$Sv_mean)

total_vecs_all <- c(vec1, vec2, vec3, vec4, vec5, vec6, vec7, vec8, vec9,
                    vec10, vec11, vec12, vec13, vec14, vec15)
hist(total_vecs_all)

## Day only

school_detect_day <- read.csv("School_Detection_Day.csv")

dayz_abun <- sv_abundance(school_detect_day$Sv_mean)
hist(dayz_abun)

## Night only

school_detect_night <- read.csv("School_Detection_Night.csv")

nights_abun <- sv_abundance(school_detect_night$Sv_mean)
hist(nights_abun)

#### Default (ABC and Corrected Area) Approach - Day & Night ####

setwd("~/UW Summer 2022/EV Exports/Mobile/School Detection/Default")

June_03 <- read.csv("June_03.csv")
June_04 <- read.csv("June_04.csv")
June_06 <- read.csv("June_06.csv")
June_07 <- read.csv("June_07.csv")
June_08 <- read.csv("June_09.csv")
June_11 <- read.csv("June_11.csv")
June_13 <- read.csv("June_13.csv")
May_02 <- read.csv("May_02.csv")
May_04 <- read.csv("May_04.csv")
May_06 <- read.csv("May_06.csv")
May_07 <- read.csv("May_07.csv")
May_08 <- read.csv("May_08.csv")
May_09 <- read.csv("May_09.csv")
May_10 <- read.csv("May_10.csv")
May_13 <- read.csv("May_13.csv")

School_Detection_Day <- read.csv("School_Detection_Day.csv")
School_Detection_Night <- read.csv("School_Detection_Night.csv")

# Day

School_Detection_Day_ABC <- School_Detection_Day$ABC
School_Detection_Day_abun <- vector(length = length(School_Detection_Day_ABC))
for (i in 1:length(School_Detection_Day_ABC)) {
  School_Detection_Day_abun[i] <- School_Detection_Day_ABC[i]*area / sigma_bs_herring
}

weighted <- as.data.frame(plyr::count(School_Detection_Day_abun))
weight_mean_day <- weighted.mean(weighted$x, weighted$freq)

hist(School_Detection_Day_abun, breaks = 20)
abline(v = weight_mean_day, col = "red")

# Night

School_Detection_Night_ABC <- School_Detection_Night$ABC
School_Detection_Night_abun <- vector(length = length(School_Detection_Night_ABC))
for (i in 1:length(School_Detection_Night_ABC)) {
  School_Detection_Night_abun[i] <- School_Detection_Night_ABC[i]*area / sigma_bs_herring
}

weighted_n <- as.data.frame(plyr::count(School_Detection_Night_abun))
weight_mean_night <- weighted.mean(weighted_n$x, weighted_n$freq)

hist(School_Detection_Night_abun, breaks = 20)
abline(v = weight_mean_night, col = "red")

# June 03

June_03_ABC <- June_03$ABC
June_03_abun <- vector(length = length(June_03_ABC))
for (i in 1:length(June_03_ABC)) {
  June_03_abun[i] <- June_03_ABC[i]*area / sigma_bs_herring
}

# June 04
June_03_ABC <- June_03$ABC
area <- 1500*60
June_03_abun <- vector(length = length(June_03_ABC))

for (i in 1:length(June_03_ABC)) {
  June_03_abun[i] <- June_03_ABC[i]*area / sigma_bs_herring
}
