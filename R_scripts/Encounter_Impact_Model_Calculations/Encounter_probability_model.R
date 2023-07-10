#### Encounter Probability Model
#### October 2022

## Data Probabilities

prob.zoi <- 0.06375301
prob.zoi.day <- 0.06360947
prob.zoi.night <- 0.06497797

prob.zoi.sub <- 0.07095581
prob.zoi.day.sub <- 0.0729516
prob.zoi.night.sub <- 0.06346733

prob.zoi.active.avoidance <- 0.02370019
prob.zoi.active.avoidance.day <- 0.02366272
prob.zoi.active.avoidance.night <- 0.02417181

prob.zoi.passive.avoidance <- 0.790369
prob.zoi.passive.avoidance.day <- 0.7900719
prob.zoi.passive.avoidance.night <- 0.7929039

prob.entrainment.horizontal <- 0.01443355
prob.entrainment.horizontal.day <-  0.01440697
prob.entrainment.horizontal.night <- 0.01468027

prob.entrainment.horizontal.sub <- 0.01614189
prob.entrainment.horizontal.day.sub <- 0.01481817
prob.entrainment.horizontal.night.sub <- 0.02208727

prob.entrainment.axial <- 0.002464811
prob.entrainment.axial.day <- 0.002459134
prob.entrainment.axial.night <- 0.002505414

prob.entrainment.axial.sub <- 0.003987691
prob.entrainment.axial.day.sub <- 0.00302235
prob.entrainment.axial.night.sub <- 0.008253122

## Literature Probabilities

prob.avoidance.zoi.shen <- 0.372
prob.active.avoidance.entrainment.viehman <- 0.020
prob.passive.avoidance.entrainment.viehman <- 0.937

prob.collision
prob.strike.yoshida.bright <- 0.05
prob.strike.yoshida.dark <- 0.022
prob.strike.courtney <- 0.13
prob.strike.collision

## Probabilities

# Pr(Entrainment) = P(zone of Influence) x P(1 - avoid | zone of influence)

# Pr(Impactcollide) = P(entrainment) x P(collide | entrainment)

# Pr(Impactstrike) = P(entrainment) x P(strike | entrainment)

# Pr(Impactcollide+strike) = P(entrainment) x [P(collide) x P(strike | collide)]
