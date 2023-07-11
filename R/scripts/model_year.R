## Model input data ##
## Andre P. Silva ##

# Note: add temporal correlation component here

territories need to use the standardized income to remove the effects of year

# data -------------------------------------------------------------------------
modeldatascaled_year <- read_csv2('./data/modelInputData/df.year.csv')

# test the effect of pop_size on the dataset ---------------------------------------
# if well supported include it in the remaining models
model1 <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                n_territories,
              family = binomial,
              data = modeldatascaled_year)
summary(model1)
with(summary(model1), 1 - deviance/null.deviance)
confint(model1)

# First-stage of analyses--------------------------------------------------------
# list of candidate models
Cand.mod <- list()
# Poaching by retaliation and socio-economic context
Cand.mod[[1]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                       sum_AttackDogs_last5y + sum_wolfLKill_last5y +
                       mean_IndividualIncome_sc + n_territories,
                     family = binomial,
                     data = modeldatascaled_year)
# poaching by retaliation and easiness of access
Cand.mod[[2]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                       sum_AttackDogs_last5y + mean_ruggedness_sc +
                       pop_mean_sc + n_territories,
                     family = binomial,
                     data = modeldatascaled_year)
# global model
Cand.mod[[3]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                       sum_AttackDogs_last5y + sum_wolfLKill_last5y +
                       mean_IndividualIncome_sc + mean_ruggedness_sc +
                       pop_mean_sc + n_territories,
                     family = binomial,
                     data = modeldatascaled_year)
# null model
Cand.mod[[4]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~  1,
                     family = binomial,
                     data = modeldatascaled_year)

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
# inbreeding
Cand.mod2[[1]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                        mean_fi_sc + n_territories,
                      family = binomial,
                      data = modeldatascaled_year)

# competition
Cand.mod2[[2]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                        mean_number_neighbour_terr + mean_bear_density_BZtiff_sc +
                        n_territories,
                      family = binomial,
                      data = modeldatascaled_year)
# global model
Cand.mod2[[3]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~
                        mean_fi_sc + mean_number_neighbour_terr +
                        mean_bear_density_BZtiff_sc + n_territories,
                      family = binomial,
                      data = modeldatascaled_year)
# null model
Cand.mod2[[4]] <- glm(cbind(sum_fate,n_territories-sum_fate) ~ 1,
                      family = binomial,
                      data = modeldatascaled_year)

#Assign names to each model
Modnames2 <- c("Inbreeding",
               "Competition",
               "Global",
               "Null")

#Model selection table based on AIC
aictab(cand.set = Cand.mod2, modnames = Modnames2)
summary(Cand.mod2[[1]])
confint(Cand.mod2[[1]])
with(summary(Cand.mod2[[3]]), 1 - deviance/null.deviance)
