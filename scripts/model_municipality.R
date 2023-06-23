## Generalized Linear Model (GLM) ##
## Andre P. Silva ##

# Note: add spatial autcorrelation component here

# data -------------------------------------------------------------------------
modeldatascaled <- read_csv('./data/modelInputData/df.municipality_scaled.csv')

# First-stage of analyses--------------------------------------------------------
# list of candidate models
Cand.mod <- list()
# Poaching by retaliation and socio-economic context
Cand.mod[[1]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                       sum_AttackDogs_n + sum_wolfLKill_n + mean_IndividualIncome,
                     family = binomial,
                     data = modeldatascaled)
# poaching by retaliation and easiness of access
Cand.mod[[2]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                       sum_AttackDogs_n + mean_ruggedness + mean_pop,
                     family = binomial,
                     data = modeldatascaled)
# global model
Cand.mod[[3]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                       sum_AttackDogs_n + sum_wolfLKill_n + mean_IndividualIncome +
                       mean_ruggedness + mean_pop,
                     family = binomial,
                     data = modeldatascaled)
# null model
Cand.mod[[4]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~  1,
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
Cand.mod2[[1]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                        sum_AttackDogs_n + sum_wolfLKill_n,
                      family = binomial,
                      data = modeldatascaled)
# best of poaching + socio-economic context + inbreeding
Cand.mod2[[2]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                        sum_AttackDogs_n + sum_wolfLKill_n + mean_fi,
                      family = binomial,
                      data = modeldatascaled)
# best of poaching + socio-economic context + competition
Cand.mod2[[3]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                        sum_AttackDogs_n + sum_wolfLKill_n +
                        mean_number_neighbour_terr + mean_bear_density_BZtiff,
                      family = binomial,
                      data = modeldatascaled)
# global model
Cand.mod2[[4]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                        sum_AttackDogs_n + sum_wolfLKill_n + mean_fi +
                        mean_number_neighbour_terr + mean_bear_density_BZtiff,
                      family = binomial,
                      data = modeldatascaled)
# null model
Cand.mod2[[5]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~ 1,
                      family = binomial,
                      data = modeldatascaled)

# Assign names to each model
Modnames2 <- c("Poaching + socioeconomic",
               "Poaching + Inbreeding",
               "Poaching + Competition",
               "Global",
               "Null")

# Model selection table based on AIC
aictab(cand.set = Cand.mod2, modnames = Modnames2)
summary(Cand.mod2[[1]])
confint(Cand.mod2[[1]])
with(summary(Cand.mod2[[5]]), 1 - deviance/null.deviance)

# references on modelling proportion data
# https://stats.stackexchange.com/questions/89734/glm-for-proportion-data-in-r

