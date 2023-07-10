#### Ideas for the graph JH wants ####

# Look into the literature on where tip speeds plateau
# Are tip speeds 0 m/s from when current speeds are 0-0.5 m/s?
# Do they reach a maximum tip speed and don't go beyond that?

## Variables

swimming_speeds <- seq(from = 0.2, to = 1.8, by = 0.2)
current_speeds <- seq(from = 0, to = 3.0, length.out = 9)
tip_speed_2.1 <- 2.1
tip_speed_10 <- 10
n_blades <- 3

#### Probability(Encounter > 1 body-length/second) ####

speed_probs <- vector()
for (i in 1:length(current_speeds)) {
  speed_probs[i] <- n_blades*((tip_speed_2.1)^2)/current_speeds[i]
}

speed_probs <- vector()
for (i in 1:length(current_speeds)) {
  speed_probs[i] <- n_blades*((tip_speed_10)^2)/current_speeds[i]
}

plot(swimming_speeds, speed_probs, xlab = "Swimming speed", ylab = "Strike probability", type = "b")
plot(current_speeds, speed_probs, xlab = "Current speed", ylab = "Strike probability", type = "b")

#### Probability(Encounter) ####

## Tip speed:Swimming speed ratios
ratios <- vector()
for (i in 1:length(swimming_speeds)) {
  ratios[i] <- tip_speed_2.1/swimming_speeds[i]
}

ratios <- vector()
for (i in 1:length(swimming_speeds)) {
  ratios[i] <- tip_speed_10/swimming_speeds[i]
}

## Probability of encounter
encounters <- vector()
for (i in 1:length(ratios)) {
  encounters[i] <- n_blades*ratios[i]
}

plot(current_speeds, ratios, type = "b", xlab = "Current speeds", ylab = "Tip Speed/Swimming Speed Ratio")
plot(current_speeds, encounters, type = "b", xlab = "Current speeds", ylab = "Strike probability")
plot(swimming_speeds, encounters, type = "b", xlab = "Swimming speeds", ylab = "Strike probability")
