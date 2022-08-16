## Fish schooling simulation
## July 2022
## For review

library(dplyr)

width <- 1500 ## width of the transect
height <- 60 ## height of the transect
n <- rpois(1, 10) ## number of schools

## defining the zone of influence
xleft_zoi = 700
xright_zoi = 900
ybottom_zoi = 0
ytop_zoi = 40
box_area_zoi = (xright_zoi - xleft_zoi) * (ytop_zoi - ybottom_zoi)

## determines if a fish is within the zone of influence
in_box_zoi <- function(x, y) {
  (x > xleft_zoi) & (x < xright_zoi) & (y > ybottom_zoi) & (y < ytop_zoi)
}

## defining the turbine
xleft_tur = 755
xright_tur = 790
ybottom_tur = 0
ytop_tur = 30
box_area_tur = (xright_tur - xleft_tur) * (ytop_tur - ybottom_tur)

## determines if a fish is within the turbine structure
in_box_tur <- function(x, y) {
  (x > xleft_tur) & (x < xright_tur) & (y > ybottom_tur) & (y < ytop_tur)
}

## repeats the simulation and keeping track of how many fish land in each box
n_sim = 10000
n_hits_zoi = rep(0, n_sim)
n_hits_tur = rep(0, n_sim)

## determines the seed locations for each school
for (j in 1:n_sim) {
  x = runif(n, 0, width)
  y = runif(n, 0, height)
  
  n_fish <- rpois(n, 170) ## generates a random number of fish per school
  
  a <-list() ## creates a list
  
  for (i in 1:n) {
    a[[i]] <- as.data.frame(matrix(NA, nrow = n_fish[i], ncol = 2)) ## each list being a dataframe
    colnames(a[[i]]) <- c("x", "y") ## naming columns
    a[[i]][, 1] <- rnorm(n_fish[i], x[i], 2) ## generates random x locations for the random number of fish
    a[[i]][, 2] <- rnorm(n_fish[i], y[i], 2) ## generates random y locations for the random number of fish
  }
  
  fishes_all <- bind_rows(a) ## bind entire list into single data frame
  
  fish_vec <- matrix(NA, nrow = n, ncol = 1) ## creates an empty vector
  
  for (k in 1:n) {
    fish_vec[k] <- nrow(a[[k]]) ## singles out how many fish are within each school
  }
  
  total_fish <- sum(fish_vec) ## sums the total number of fish
  
  schools_density = total_fish / (width * height) ## finds the density based on total fish and domain
  
  n_hits_zoi[j] = sum(in_box_zoi(fishes_all[,1], fishes_all[,2])) ## summing the number of fish in the zone of influence across simulation
  n_hits_tur[j] = sum(in_box_tur(fishes_all[,1], fishes_all[,2])) ## summing the number of fish in the turbine structure across simulation
}

hist(n_hits_zoi, freq=F) ## plotting the frequency of hits in the zone of influence
abline(v=schools_density*box_area_zoi, col='red') ## expected number of hits 

hist(n_hits_tur, freq=F) ## plotting the frequency of hits
abline(v=schools_density*box_area_tur, col='red') ## expected number of hits

## same results from a Poisson distribution
hist(rpois(n_sim, schools_density*box_area_zoi), breaks=max(n_hits_zoi) - 0.5)
hist(rpois(n_sim, schools_density*box_area_tur), breaks=max(n_hits_tur) - 0.5)
