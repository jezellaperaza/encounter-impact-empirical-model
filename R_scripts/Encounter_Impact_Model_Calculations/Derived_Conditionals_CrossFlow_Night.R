#### The conditional probabilities 

#### Cross-flow turbine
#### night
#### Random

zoi_night_random <- 0.06497797
avoid_data <- 0.02417181 + 0.7929039
avoid_lit <- 0.372

# Entrainment
entrainment_cross_random_data_night <- zoi_night_random*(1 - avoid_data)
entrainment_cross_random_lit_night <- zoi_night_random*(1 - avoid_lit)

# Collide (data avoid)
entrainment_cross_random_data_night*collision_prob_court_night
entrainment_cross_random_data_night*collision_prob_yoshida_night
entrainment_cross_random_data_night*the_collision_probs_cross_avg_night

# Collide (literature avoid)
entrainment_cross_random_lit_night*collision_prob_court_night
entrainment_cross_random_lit_night*collision_prob_yoshida_night
entrainment_cross_random_lit_night*the_collision_probs_cross_avg_night

#### Cross-flow turbine
#### night
#### Fixed

zoi_night_fixed <- 0.06346733

# Entrainment
entrainment_cross_fixed_data_night <- zoi_night_fixed*(1 - avoid_data)
entrainment_cross_fixed_lit_night <- zoi_night_fixed*(1 - avoid_lit)

# Collide (data avoid)
entrainment_cross_fixed_data_night*collision_prob_court_night
entrainment_cross_fixed_data_night*collision_prob_yoshida_night
entrainment_cross_fixed_data_night*the_collision_probs_cross_avg_night

# Collide (literature avoid)
entrainment_cross_fixed_lit_night*collision_prob_court_night
entrainment_cross_fixed_lit_night*collision_prob_yoshida_night
entrainment_cross_fixed_lit_night*the_collision_probs_cross_avg_night

############ Strike 

#### Cross-flow turbine
#### night
#### Random

# Strike (data avoid)
entrainment_cross_random_data_night*0.13
entrainment_cross_random_data_night*0.022
entrainment_cross_random_data_night*strike_probs_avg

# Strike (literature avoid)
entrainment_cross_random_lit_night*0.13
entrainment_cross_random_lit_night*0.022
entrainment_cross_random_lit_night*strike_probs_avg

#### Cross-flow turbine
#### night
#### Fixed

# Strike (data avoid)
entrainment_cross_fixed_data_night*0.13
entrainment_cross_fixed_data_night*0.022
entrainment_cross_fixed_data_night*strike_probs_avg

# Strike (literature avoid)
entrainment_cross_fixed_lit_night*0.13
entrainment_cross_fixed_lit_night*0.022
entrainment_cross_fixed_lit_night*strike_probs_avg

############ Collide and Strike 

#### Cross-flow turbine
#### night
#### Random

# Collide and Strike  (data avoid)
entrainment_cross_random_data_night*collision_prob_court_night*0.13
entrainment_cross_random_data_night*collision_prob_yoshida_night*0.022
entrainment_cross_random_data_night*cross_collide_strike_avg_night

# Collide and Strike  (literature avoid)
entrainment_cross_random_lit_night*collision_prob_court_night*0.13
entrainment_cross_random_lit_night*collision_prob_yoshida_night*0.022
entrainment_cross_random_lit_night*cross_collide_strike_avg_night

#### Cross-flow turbine
#### night
#### Fixed

# Collide and Strike  (data avoid)
entrainment_cross_fixed_data_night*collision_prob_court_night*0.13
entrainment_cross_fixed_data_night*collision_prob_yoshida_night*0.022
entrainment_cross_fixed_data_night*cross_collide_strike_avg_night

# Collide and Strike  (literature avoid)
entrainment_cross_fixed_lit_night*collision_prob_court_night*0.13
entrainment_cross_fixed_lit_night*collision_prob_yoshida_night*0.022
entrainment_cross_fixed_lit_night*cross_collide_strike_avg_night
