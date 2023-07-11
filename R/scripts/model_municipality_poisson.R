## Model input data ##
## Andre P. Silva ##

# Note: add spatial correlation component here

# data -------------------------------------------------------------------------
modeldatascaled_municipality <- read_csv2('./data/modelInputData/df.municipality.csv') %>%
  # select municipalities where there was at least one disappearance
  filter(disappearance>=1)

view(modeldatascaled_municipality)

# test the effect of pop_size on the dataset ---------------------------------------
# if well supported include it in the remaining models
model1 <- glm(disappearance ~ n_territories_without_disap,
              family = poisson,
              data = modeldatascaled_municipality)
summary(model1)
confint(model1)
with(summary(model1), 1 - deviance/null.deviance)

# First-stage of analyses--------------------------------------------------------
# list of candidate models
Cand.mod <- list()
# Poaching by retaliation and socio-economic context
Cand.mod[[1]] <- glm(disappearance ~
                       sum_AttackDogs_last5y + sum_wolfLKill_last5y +
                       mean_income_prop_sc + n_territories_without_disap,
                     family = poisson,
                     data = modeldatascaled_municipality)
# poaching by retaliation and easiness of access
Cand.mod[[2]] <- glm(disappearance ~
                       sum_AttackDogs_last5y + mean_ruggedness_sc +
                       pop_mean_sc + n_territories_without_disap,
                     family = poisson,
                     data = modeldatascaled_municipality)
# global model
Cand.mod[[3]] <- glm(disappearance ~
                       sum_AttackDogs_last5y + sum_wolfLKill_last5y +
                       mean_income_prop_sc + mean_ruggedness_sc +
                       pop_mean_sc + n_territories_without_disap,
                     family = poisson,
                     data = modeldatascaled_municipality)
# null model
Cand.mod[[4]] <- glm(disappearance ~  1,
                     family = poisson,
                     data = modeldatascaled_municipality)

Modnames <- c("Poaching + socioeconomic",
              "Poaching + access",
              "Global",
              "Null")

#Model selection table based on AIC
aictab(cand.set = Cand.mod, modnames = Modnames)
summary(Cand.mod[[1]])
confint(Cand.mod[[1]])
with(summary(Cand.mod[[3]]), 1 - deviance/null.deviance)
#r2_nakagawa(Cand.mod[[3]])
#confint.merMod(Cand.mod[[1]], method = c("Wald"))

# list of candidate models
Cand.mod2 <- list()
# socioeconomic
Cand.mod2[[1]] <- glm(disappearance ~ sum_AttackDogs_last5y +
                        n_territories_without_disap,
                      family = poisson,
                      data = modeldatascaled_municipality)
# socioeconomic + inbreeding
Cand.mod2[[2]] <- glm(disappearance ~ sum_AttackDogs_last5y +
                        mean_fi_sc + n_territories_without_disap,
                      family = poisson,
                      data = modeldatascaled_municipality)

# socioeconomic + competition
Cand.mod2[[3]] <- glm(disappearance ~ sum_AttackDogs_last5y +
                        mean_number_neighbour_terr + mean_bear_density_BZtiff_sc +
                        n_territories_without_disap,
                      family = poisson,
                      data = modeldatascaled_municipality)
# global model
Cand.mod2[[4]] <- glm(disappearance ~ sum_AttackDogs_last5y +
                        mean_fi_sc + mean_number_neighbour_terr +
                        mean_bear_density_BZtiff_sc + n_territories_without_disap,
                      family = poisson,
                      data = modeldatascaled_municipality)
# null model
Cand.mod2[[5]] <- glm(disappearance ~ 1,
                      family = poisson,
                      data = modeldatascaled_municipality)

#Assign names to each model
Modnames2 <- c("socioeconomic",
               "socioeconomic + Inbreeding",
               "socioeconomic + Competition",
               "Global",
               "Null")

#Model selection table based on AIC
aictab(cand.set = Cand.mod2, modnames = Modnames2)
summary(Cand.mod2[[1]])
confint(Cand.mod2[[1]])
with(summary(Cand.mod2[[3]]), 1 - deviance/null.deviance)


