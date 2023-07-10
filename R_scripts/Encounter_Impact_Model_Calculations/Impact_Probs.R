### November 2022

#### Day
#### Impact - Collide ####

## Axial-flow
axial_collide_impact_day <- 0.0402*the_collision_probs_axial_day
## Cross-flow (average)
cross_avg_collide_impact_day <- 0.0402*the_collision_probs_cross_avg_day
## Cross-flow (maximum)
cross_max_collide_impact_day <- 0.0402*the_collision_probs_cross_max_day

#### Impact - Strike ####
## Axial-flow
axial_strike_impact_day <- 0.0402*strike_probs
## Cross-flow (average)
cross_avg_strike_impact_day <- 0.0402*strike_probs_avg
## Cross-flow (maximum)
cross_max_strike_impact_day <- 0.0402*strike_probs_max

#### Impact - Collide & Strike ####
## Axial-flow
axial_collide_strike_impact_day <- 0.0402*axial_collide_strike_day
## Cross-flow (average)
cross_avg_collide_strike_impact_day <- 0.0402*cross_collide_strike_avg_day
## Cross-flow (maximum)
cross_max_collide_strike_impact_day <- 0.0402*cross_collide_strike_max_day

#### Impact ####
axial_impact_day <- axial_collide_impact_day + axial_strike_impact_day + axial_collide_strike_impact_day
cross_avg_impact_day <- cross_avg_collide_impact_day + cross_avg_strike_impact_day + cross_avg_collide_strike_impact_day 
cross_max_impact_day <- cross_max_collide_impact_day + cross_max_strike_impact_day + cross_max_collide_strike_impact_day

#### Avoidance - Impact ####

1 - axial_impact_day
1 - cross_avg_impact_day
1 - cross_max_impact_day

#### Night
#### Impact - Collide ####

## Axial-flow
axial_collide_impact_night <- 0.0402*the_collision_probs_axial_night
## Cross-flow (average)
cross_avg_collide_impact_night <- 0.0402*the_collision_probs_cross_avg_night
## Cross-flow (maximum)
cross_max_collide_impact_night <- 0.0402*the_collision_probs_cross_max_night

#### Impact - Strike ####
## Axial-flow
axial_strike_impact_night <- 0.0402*strike_probs
## Cross-flow (average)
cross_avg_strike_impact_night <- 0.0402*strike_probs_avg
## Cross-flow (maximum)
cross_max_strike_impact_night <- 0.0402*strike_probs_max

#### Impact - Collide & Strike ####
## Axial-flow
axial_collide_strike_impact_night <- 0.0402*axial_collide_strike_night
## Cross-flow (average)
cross_avg_collide_strike_impact_night <- 0.0402*cross_collide_strike_avg_night
## Cross-flow (maximum)
cross_max_collide_strike_impact_night <- 0.0402*cross_collide_strike_max_night

#### Impact ####
axial_impact_night <- axial_collide_impact_night + axial_strike_impact_night + axial_collide_strike_impact_night
cross_avg_impact_night <- cross_avg_collide_impact_night + cross_avg_strike_impact_night + cross_avg_collide_strike_impact_night 
cross_max_impact_night <- cross_max_collide_impact_night + cross_max_strike_impact_night + cross_max_collide_strike_impact_night

#### Avoidance - Impact ####

1 - axial_impact_night
1 - cross_avg_impact_night
1 - cross_max_impact_night
