## Generalized Linear Model (GLM) ##
## Andre P. Silva ##

# Note: there is pseudo-replication aspect with territories being repeated over
# year but to few data points to run territory as random-effect - not adequate
# model structure, run only for comparasion to  survival analyses

# data -------------------------------------------------------------------------
modeldatascaled <- read_csv('./data/modelInputData/df.territory_scaled.csv')

# test the effect of time on the dataset ---------------------------------------
# if well supported include it in the remaining models
model1 <- glm(fate ~ year, family = binomial, data = df.territory_scaled)

# First-stage of analyses--------------------------------------------------------
# list of candidate models
Cand.mod <- list()
# Poaching by retaliation and socio-economic context
Cand.mod[[1]] <- glm(fate ~ AttackDogs_last5y + wolfLKill_last5y + IndividualIncome +
                       year,
                     family = binomial,
                     data = df.territory_scaled)
# poaching by retaliation and easiness of access
Cand.mod[[2]] <- glm(fate ~ AttackDogs_last5y + ruggedness_mean + pop_mean +
                       year,
                     family = binomial,
                     data = df.territory_scaled)
# global model
Cand.mod[[3]] <- glm(fate ~ AttackDogs_last5y + wolfLKill_last5y + IndividualIncome +
                       ruggedness_mean + pop_mean + year,
                     family = binomial,
                     data = df.territory_scaled)
# null model
Cand.mod[[4]] <- glm(fate ~  1,
                     family = binomial,
                     data = df.territory_scaled)

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

# second-stage of analyses -----------------------------------------------------
# takes the variables with well supported effects from the best model in the
# first stage of analyses and combines them with additional hypotheses (i.e.
# inbreeding and competition variables)

# list of candidate models
Cand.mod2 <- list()
# Poaching by retaliation and socio-economic context
Cand.mod2[[1]] <- glm(fate ~ AttackDogs_last5y + wolfLKill_last5y + year,
                      family = binomial,
                      data = df.territory_scaled)
# best of poaching + socio-economic context + inbreeding
Cand.mod2[[2]] <- glm(fate ~
                        AttackDogs_last5y + wolfLKill_last5y + mean_fi + year,
                      family = binomial,
                      data = df.territory_scaled)
# best of poaching + socio-economic context + competition
Cand.mod2[[3]] <- glm(fate ~
                        sum_AttackDogs_n + sum_wolfLKill_n +
                        mean_number_neighbour_terr + mean_bear_density_BZtiff +
                        year,
                      family = binomial,
                      data = df.territory_scaled)
# global model
Cand.mod2[[4]] <- glm(fate ~
                        sum_AttackDogs_n + sum_wolfLKill_n + mean_fi +
                        mean_number_neighbour_terr + mean_bear_density_BZtiff +
                        year,
                      family = binomial,
                      data = df.territory_scaled)
# null model
Cand.mod2[[5]] <- glm(fate ~ 1,
                      family = binomial,
                      data = df.territory_scaled)

#Assign names to each model
Modnames2 <- c("Poaching + socioeconomic",
               "Poaching + Inbreeding",
               "Poaching + Competition",
               "Global",
               "Null")

#Model selection table based on AIC
aictab(cand.set = Cand.mod2, modnames = Modnames2)
summary(Cand.mod2[[1]])
confint(Cand.mod2[[1]])
with(summary(Cand.mod2[[5]]), 1 - deviance/null.deviance)







it currently ignores that territories repaeat over time

"Fate", "SLU.ID", "territory", "Autumn", "Spring", "ruggedness_mean",
"pop_mean", 
"Fi","mean_fi","max_fi",
"hunt_county2", "number_neighbour_terr",
"AttackDogs_n","AttackDogs_last5y","AttackDogs_cumall",
"wolfLKill_n","wolfLKill_last5y","wolfLKill_cumall",
"sheepAttacks_n","NoAffectedSheep","NoAffectedSheep_last5y",
"NoAffectedSheep_cumall",
"IndividualIncome",
"bear_density_BZtiff",
"KommunerNamn", "NAME_1", "NAME_2",
"first_yeardet","years_firstdet")