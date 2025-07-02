# Univariate models--------------------------------------------------------
# Variables to test for population scale:
### Dogs attacked in last 5 years
model_pop_1 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      NoAffectedDogs_last5y_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_1)
confint(model_pop_1)
with(summary(model_pop_1), 1 - deviance/null.deviance)

### Sheep attacked in last 5 years
model_pop_2 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      NoAffectedSheep_last5y_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_2)
confint(model_pop_2)
with(summary(model_pop_2), 1 - deviance/null.deviance)

### Wolves killed in last 5 years
model_pop_3 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfLKill_last5y_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_3)
confint(model_pop_3)
with(summary(model_pop_3), 1 - deviance/null.deviance)

### Moose density
model_pop_4 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      moose_harvest_density_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_4)
confint(model_pop_4)
with(summary(model_pop_4), 1 - deviance/null.deviance)

### Individual income
model_pop_5 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      income_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_5)
confint(model_pop_5)
with(summary(model_pop_5), 1 - deviance/null.deviance)

### Bear density
model_pop_6 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      bear_density_live_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_6)
confint(model_pop_6)
with(summary(model_pop_6), 1 - deviance/null.deviance)

### Wolf neighbour territories
model_pop_7 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      number_neighbour_terr_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_7)
confint(model_pop_7)
with(summary(model_pop_7), 1 - deviance/null.deviance)

### terrain ruggedness
model_pop_8 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      ruggedness_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_8)
confint(model_pop_8)
with(summary(model_pop_8), 1 - deviance/null.deviance)

-------------------------------------------------------------------------------
# Variables to test for municipality scale:
### Length gravel roads
model_mun_1 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      gravel_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_1)
confint(model_mun_1)
with(summary(model_mun_1), 1 - deviance/null.deviance)

### Human density
model_mun_2 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      human_density_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_2)
confint(model_mun_2)
with(summary(model_mun_2), 1 - deviance/null.deviance)

### Dogs attacked in last 5 years
model_mun_3 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      NoAffectedDogs_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_3)
confint(model_mun_3)
with(summary(model_mun_3), 1 - deviance/null.deviance)

### Sheep attacked in last 5 years
model_mun_4 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      NoAffectedSheep_last5y_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_4)
confint(model_mun_4)
with(summary(model_mun_4), 1 - deviance/null.deviance)
  
### Wolves killed in last 5 years
model_mun_5 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfLKill_last5y_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_5)
confint(model_mun_5)
with(summary(model_mun_5), 1 - deviance/null.deviance)

### Bear density
model_mun_6 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      bear_density_live_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_6)
confint(model_mun_6)
with(summary(model_mun_6), 1 - deviance/null.deviance)

### Length paved roads
model_mun_7 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      paved_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_7)
confint(model_mun_7)
with(summary(model_mun_7), 1 - deviance/null.deviance)

### snow cover
model_mun_8 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      snow_cover_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_8)
confint(model_mun_8)
with(summary(model_mun_8), 1 - deviance/null.deviance)

### night light
model_mun_9 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      night_light_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_9)
confint(model_mun_9)
with(summary(model_mun_9), 1 - deviance/null.deviance)
