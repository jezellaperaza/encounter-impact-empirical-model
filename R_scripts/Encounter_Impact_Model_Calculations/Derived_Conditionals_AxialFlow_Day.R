#### Conditional probabilities 
#### Axial-flow turbine
#### Day

zoi_day_random <- 0.06360947
avoid_data <- 0.02366272 + 0.7900719
avoid_lit <- 0.372

############ Entrainment
entrainment_axial_random_data <- zoi_day_random*(1 - avoid_data)
entrainment_axial_random_lit <- zoi_day_random*(1 - avoid_lit)

############ Collision

# Collide (data avoid)
entrainment_axial_random_data*collision_prob_court_day
entrainment_axial_random_data*collision_prob_yoshida_day
entrainment_axial_random_data*the_collision_probs_axial_day

# Collide (literature avoid)
entrainment_axial_random_lit*collision_prob_court_day
entrainment_axial_random_lit*collision_prob_yoshida_day
entrainment_axial_random_lit*the_collision_probs_axial_day

############ Strike 

# Strike (data avoid)
entrainment_axial_random_data*0.13
entrainment_axial_random_data*0.05
entrainment_axial_random_data*strike_probs

# Strike (literature avoid)
entrainment_axial_random_lit*0.13
entrainment_axial_random_lit*0.05
entrainment_axial_random_lit*strike_probs

############ Collide and Strike 

# Collide and Strike  (data avoid)
entrainment_axial_random_data*collision_prob_court_day*0.13
entrainment_axial_random_data*collision_prob_yoshida_day*0.05
entrainment_axial_random_data*axial_collide_strike_day

# Collide and Strike  (literature avoid)
entrainment_axial_random_lit*collision_prob_court_day*0.13
entrainment_axial_random_lit*collision_prob_yoshida_day*0.05
entrainment_axial_random_lit*axial_collide_strike_day
