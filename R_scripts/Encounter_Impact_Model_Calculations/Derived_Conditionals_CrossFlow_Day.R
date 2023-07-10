#### The conditional probabilities 

#### Cross-flow turbine
#### Day
#### Random

zoi_day_random <- 0.06360947
avoid_data <- 0.02366272 + 0.7900719
avoid_lit <- 0.372

# Entrainment
entrainment_cross_random_data <- zoi_day_random*(1 - avoid_data)
entrainment_cross_random_lit <- zoi_day_random*(1 - avoid_lit)

# Collide (data avoid)
entrainment_cross_random_data*collision_prob_court_day
entrainment_cross_random_data*collision_prob_yoshida_day
entrainment_cross_random_data*the_collision_probs_cross_avg_day

# Collide (literature avoid)
entrainment_cross_random_lit*collision_prob_court_day
entrainment_cross_random_lit*collision_prob_yoshida_day
entrainment_cross_random_lit*the_collision_probs_cross_avg_day

#### Cross-flow turbine
#### Day
#### Fixed

zoi_day_fixed <- 0.0729516

# Entrainment
entrainment_cross_fixed_data <- zoi_day_fixed*(1 - avoid_data)
entrainment_cross_fixed_lit <- zoi_day_fixed*(1 - avoid_lit)

# Collide (data avoid)
entrainment_cross_fixed_data*collision_prob_court_day
entrainment_cross_fixed_data*collision_prob_yoshida_day
entrainment_cross_fixed_data*the_collision_probs_cross_avg_day

# Collide (literature avoid)
entrainment_cross_fixed_lit*collision_prob_court_day
entrainment_cross_fixed_lit*collision_prob_yoshida_day
entrainment_cross_fixed_lit*the_collision_probs_cross_avg_day

############ Strike 

#### Cross-flow turbine
#### Day
#### Random

# Strike (data avoid)
entrainment_cross_random_data*0.13
entrainment_cross_random_data*0.05
entrainment_cross_random_data*strike_probs_avg

# Strike (literature avoid)
entrainment_cross_random_lit*0.13
entrainment_cross_random_lit*0.05
entrainment_cross_random_lit*strike_probs_avg

#### Cross-flow turbine
#### Day
#### Fixed

# Strike (data avoid)
entrainment_cross_fixed_data*0.13
entrainment_cross_fixed_data*0.05
entrainment_cross_fixed_data*strike_probs_avg

# Strike (literature avoid)
entrainment_cross_fixed_lit*0.13
entrainment_cross_fixed_lit*0.05
entrainment_cross_fixed_lit*strike_probs_avg

############ Collide and Strike 

#### Cross-flow turbine
#### Day
#### Random

# Collide and Strike  (data avoid)
entrainment_cross_random_data*collision_prob_court_day*0.13
entrainment_cross_random_data*collision_prob_yoshida_day*0.05
entrainment_cross_random_data*cross_collide_strike_avg_day

# Collide and Strike  (literature avoid)
entrainment_cross_random_lit*collision_prob_court_day*0.13
entrainment_cross_random_lit*collision_prob_yoshida_day*0.05
entrainment_cross_random_lit*cross_collide_strike_avg_day

#### Cross-flow turbine
#### Day
#### Fixed

# Collide and Strike  (data avoid)
entrainment_cross_fixed_data*collision_prob_court_day*0.13
entrainment_cross_fixed_data*collision_prob_yoshida_day*0.05
entrainment_cross_fixed_data*cross_collide_strike_avg_day

# Collide and Strike  (literature avoid)
entrainment_cross_fixed_lit*collision_prob_court_day*0.13
entrainment_cross_fixed_lit*collision_prob_yoshida_day*0.05
entrainment_cross_fixed_lit*cross_collide_strike_avg_day

