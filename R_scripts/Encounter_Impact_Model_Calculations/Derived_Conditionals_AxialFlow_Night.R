#### Conditional probabilities 
#### Axial-flow turbine
#### Night

zoi_night_random <- 0.06497797
avoid_data <- 0.02417181 + 0.7929039
avoid_lit <- 0.372

############ Entrainment
entrainment_axial_random_data_night <- zoi_night_random*(1 - avoid_data)
entrainment_axial_random_lit_night <- zoi_night_random*(1 - avoid_lit)

############ Collision

# Collide (data avoid)
entrainment_axial_random_data_night*collision_prob_court_night
entrainment_axial_random_data_night*collision_prob_yoshida_night
entrainment_axial_random_data_night*the_collision_probs_axial_night

# Collide (literature avoid)
entrainment_axial_random_lit_night*collision_prob_court_night
entrainment_axial_random_lit_night*collision_prob_yoshida_night
entrainment_axial_random_lit_night*the_collision_probs_axial_night

############ Strike 

# Strike (data avoid)
entrainment_axial_random_data_night*0.13
entrainment_axial_random_data_night*0.022
entrainment_axial_random_data_night*strike_probs

# Strike (literature avoid)
entrainment_axial_random_lit_night*0.13
entrainment_axial_random_lit_night*0.022
entrainment_axial_random_lit_night*strike_probs

############ Collide and Strike 

# Collide and Strike  (data avoid)
entrainment_axial_random_data_night*collision_prob_court_night*0.13
entrainment_axial_random_data_night*collision_prob_yoshida_night*0.022
entrainment_axial_random_data_night*axial_collide_strike_night

# Collide and Strike  (literature avoid)
entrainment_axial_random_lit_night*collision_prob_court_night*0.13
entrainment_axial_random_lit_night*collision_prob_yoshida_night*0.022
entrainment_axial_random_lit_night*axial_collide_strike_night
