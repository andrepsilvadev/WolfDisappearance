## Generalized Linear Model (GLM) ##
## Andre P. Silva ##

# Note: there is pseudo-replication aspect with territories being repeated over
# year but to few data points to run territory as random-effect - not adequate
# model structure, run only for comparasion to  survival analyses

# data -------------------------------------------------------------------------
modeldatascaled <- read_csv2('./data/modelInputData/df.territory.csv')

# test the effect of time on the dataset ---------------------------------------
# if well supported include it in the remaining models
model1 <- glm(Fate_binary ~ as.factor(years_firstdet), family = binomial, data = modeldatascaled)
summary(model1)
with(summary(model1), 1 - deviance/null.deviance)

# First-stage of analyses--------------------------------------------------------
# list of candidate models
Cand.mod <- list()
# Poaching by retaliation and socio-economic context
Cand.mod[[1]] <- glm(Fate_binary ~ AttackDogs_last5y + wolfLKill_last5y +
                       income_prop_sc, family = binomial, data = modeldatascaled)
# poaching by retaliation and easiness of access
Cand.mod[[2]] <- glm(Fate_binary ~ AttackDogs_last5y + pop_mean_sc,
                     family = binomial,
                     data = modeldatascaled)
# global model
Cand.mod[[3]] <- glm(Fate_binary ~ AttackDogs_last5y + wolfLKill_last5y +
                       income_prop_sc + pop_mean_sc,
                     family = binomial,
                     data = modeldatascaled)
# null model
Cand.mod[[4]] <- glm(Fate_binary ~  1,
                     family = binomial,
                     data = modeldatascaled)

Modnames <- c("Poaching + socioeconomic",
              "Poaching + access",
              "Global",
              "Null")

#Model selection table based on AIC
aictab(cand.set = Cand.mod, modnames = Modnames)
summary(Cand.mod[[1]])
confint(Cand.mod[[1]])
with(summary(Cand.mod[[1]]), 1 - deviance/null.deviance)
#r2_nakagawa(Cand.mod[[3]])
#confint.merMod(Cand.mod[[1]], method = c("Wald"))

# second-stage of analyses -----------------------------------------------------
# takes the variables with well supported effects from the best model in the
# first stage of analyses and combines them with additional hypotheses (i.e.
# inbreeding and competition variables)

# list of candidate models
Cand.mod2 <- list()
# Poaching by retaliation and socio-economic context
Cand.mod2[[1]] <- glm(Fate_binary ~ AttackDogs_last5y + wolfLKill_last5y ,
                      family = binomial,
                      data = modeldatascaled)
# best of poaching + socio-economic context + inbreeding
Cand.mod2[[2]] <- glm(Fate_binary ~
                        AttackDogs_last5y + wolfLKill_last5y + mean_fi ,
                      family = binomial,
                      data = modeldatascaled)
# best of poaching + socio-economic context + competition
Cand.mod2[[3]] <- glm(Fate_binary ~
                        sum_AttackDogs_n + sum_wolfLKill_n +
                        mean_number_neighbour_terr + mean_bear_density_BZtiff,
                      family = binomial,
                      data = modeldatascaled)
# global model
Cand.mod2[[4]] <- glm(Fate_binary ~
                        sum_AttackDogs_n + sum_wolfLKill_n + mean_fi +
                        mean_number_neighbour_terr + mean_bear_density_BZtiff,
                      family = binomial,
                      data = modeldatascaled)
# null model
Cand.mod2[[5]] <- glm(Fate_binary ~ 1,
                      family = binomial,
                      data = modeldatascaled)

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

"Fate_binary", "SLU.ID", "territory", "Autumn", "Spring", "ruggedness_mean",
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