### Collision Complement Probabilities

#### Day

avoid_factor_day <- 0.957

## Axial-flow
compliment_strikes_day_axial <- 1 - strike_probs
the_factors_day_axial <- compliment_strikes_day_axial*avoid_factor_day
the_collision_probs_axial_day <- compliment_strikes_day_axial - the_factors_day_axial

## Cross-flow (average)
compliment_strikes_day_cross_avg <- 1 - strike_probs_avg
the_factors_day_cross_avg <- compliment_strikes_day_cross_avg*avoid_factor_day
the_collision_probs_cross_avg_day <- compliment_strikes_day_cross_avg - the_factors_day_cross_avg

#### Collision & Strike ####
axial_collide_strike_day <- the_collision_probs_axial_day*strike_probs
cross_collide_strike_avg_day <- the_collision_probs_cross_avg_day*strike_probs_avg
# cross_collide_strike_max_day <- the_collision_probs_cross_max_day*strike_probs_max

#### Night

avoid_factor_night <- 0.668

## Axial-flow
compliment_strikes_night_axial <- 1 - strike_probs
the_factors_night_axial <- compliment_strikes_night_axial*avoid_factor_night
the_collision_probs_axial_night <- compliment_strikes_night_axial - the_factors_night_axial

## Cross-flow (average)
compliment_strikes_night_cross_avg <- 1 - strike_probs_avg
the_factors_night_cross_avg <- compliment_strikes_night_cross_avg*avoid_factor_night
the_collision_probs_cross_avg_night <- compliment_strikes_night_cross_avg - the_factors_night_cross_avg

#### Collision & Strike ####

### Day
axial_collide_strike_day <- the_collision_probs_axial_day*strike_probs
cross_collide_strike_avg_day <- the_collision_probs_cross_avg_day*strike_probs_avg
# cross_collide_strike_max_day <- the_collision_probs_cross_max_day*strike_probs_max

### Night
axial_collide_strike_night <- the_collision_probs_axial_night*strike_probs
cross_collide_strike_avg_night <- the_collision_probs_cross_avg_night*strike_probs_avg
# cross_collide_strike_max_night <- the_collision_probs_cross_max_night*strike_probs_max

#### Yoshida & Courtney

court_collision_compliment <- 1 - 0.13
yoshida_collision_compliment <- 1 - 0.05
yoshida_collision_compliment_night <- 1- 0.022

factors_court_day <- court_collision_compliment*avoid_factor_day
factors_court_night <- court_collision_compliment*avoid_factor_night

factors_yoshida_day <- yoshida_collision_compliment*avoid_factor_day
factors_yoshida_night <- yoshida_collision_compliment_night*avoid_factor_night

collision_prob_court_day <- court_collision_compliment - factors_court_day
collision_prob_court_night <- court_collision_compliment - factors_court_night

collision_prob_yoshida_day <- yoshida_collision_compliment - factors_yoshida_day
collision_prob_yoshida_night <- yoshida_collision_compliment_night - factors_yoshida_night

#### Yoshida & Courtney Collide + Strike

collision_prob_court_day*0.13
collision_prob_yoshida_day*0.05

collision_prob_court_night*0.13
collision_prob_yoshida_night*0.022
