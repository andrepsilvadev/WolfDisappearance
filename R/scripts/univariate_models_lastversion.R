# Univariate models--------------------------------------------------------
# Variables to test for population scale:
### Dogs attacked in last 5 years
model_pop_1 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      sum_AttackDogs_last5y_sc,
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
      sum_NoAffectedSheep_last5y_sc,
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
      sum_wolfLKill_last5y_sc,
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
      hunt_county2_sc,
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
      mean_income_prop_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_5)
confint(model_pop_5)
with(summary(model_pop_5), 1 - deviance/null.deviance)

### Bear harvest
model_pop_6 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      mean_bear_density_BZtiff_sc,
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
      mean_number_neighbour_terr_sc,
    family = binomial,
    data = modeldatascaled_year
  )
summary(model_pop_7)
confint(model_pop_7)
with(summary(model_pop_7), 1 - deviance/null.deviance)

-------------------------------------------------------------------------------
# Variables to test for municipality scale:
### Length gravel roads
model_mun_1 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      average_gravel_km_sc,
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
      pop_mean_sc,
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
      sum_AttackDogs_last5y_sc,
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
      sum_NoAffectedSheep_last5y_sc,
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
      sum_wolfLKill_last5y_sc,
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
      mean_bear_density_BZtiff_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )
summary(model_mun_6)
confint(model_mun_6)
with(summary(model_mun_6), 1 - deviance/null.deviance)


