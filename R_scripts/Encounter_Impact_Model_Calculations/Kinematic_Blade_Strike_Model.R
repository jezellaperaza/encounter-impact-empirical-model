#### Blade-strike probability from Romero-Gomez & Richmond, 2014
#### November 2022

# Variables for axial-flow

n_blades <- 3
fish_length <- 0.2
rotation_rps_axial <- 40/60
flow_speed <- seq(from = 1, to = 3.0, by = 0.2)

# Axial-flow probabilities

strike_probs <- vector()
for (i in 1:length(flow_speed)) {
  strike_probs[i] <- (n_blades*rotation_rps_axial*fish_length)/flow_speed[i]
  # strike_probs[i] <- strike_probs[i]*100
}

plot(flow_speed, strike_probs, type = "b", xlab = "Flow speed", ylab = "Blade-strike probability", 
     main = "Axial-flow tidal turbine (40 RPM)", xaxt = "n", yaxt = "n")
axis(1, at = seq(0, 3.2, by = 0.2), las = 1)
axis(2, at = seq(0, 2.2, by = 0.2), las = 1)

# Variables for cross-flow

n_blades <- 4
fish_length <- 0.2
rotation_rps_cross_avg <- 21.4/60
rotation_rps_cross_max <- 40/60
flow_speed_avg <- seq(from = 1, to = 3.0, by = 0.2)
# flow_speed_max <- seq(from = 0.6, to = 3.0, by = 0.2)

# Cross-flow probabilities

strike_probs_avg <- vector()
for (i in 1:length(flow_speed)) {
  strike_probs_avg[i] <- (n_blades*rotation_rps_cross_avg*fish_length)/flow_speed_avg[i]
  # strike_probs_avg[i] <- strike_probs_avg[i]*100
}

# strike_probs_max <- vector()
# for (i in 1:length(flow_speed)) {
#   strike_probs_max[i] <- (n_blades*rotation_rps_cross_max*fish_length)/flow_speed_max[i]
#   # strike_probs_max[i] <- strike_probs_max[i]*100
# }

plot(flow_speed, strike_probs_avg, type = "b", xlab = "Flow speed (m/s)", ylab = "Blade-strike probability", 
     main = "Cross-flow tidal turbine (21.4 RPM)", xaxt = "n", yaxt = "n")
axis(1, at = seq(0, 3.2, by = 0.2), las = 1)
axis(2, at = seq(0, 2, by = 0.2), las = 1)

# plot(flow_speed, strike_probs_max, type = "b", xlab = "Flow speed (m/s)", ylab = "Blade-strike probability", 
#      main = "Cross-flow tidal turbine (40 RPM)", xaxt = "n", yaxt = "n")
# axis(1, at = seq(0, 3.2, by = 0.2), las = 1)
# axis(2, at = seq(0, 3, by = 0.2), las = 1)
