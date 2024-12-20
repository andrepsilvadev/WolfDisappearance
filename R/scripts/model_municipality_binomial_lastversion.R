## Model input data ##
## Andre P. Silva ##

# Note: add spatial correlation component here

# data -------------------------------------------------------------------------
# Note: I am modelling variation of proportion of disappearance within areas
# where disappearances took place
modeldatascaled_municipality <- read.csv2('modelInputData/df.municipality.csv') #%>%
  # select municipalities where there was at least one disappearance
  #filter(disappearance>=1)
View(modeldatascaled_municipality)

municipality_mean <- modeldatascaled_municipality %>%
  group_by(NAME_2) %>%
  dplyr::summarize(Mean_disappearance = mean(disappearance, na.rm=TRUE))
summary(municipality_mean$Mean_disappearance)

# First-stage of analyses--------------------------------------------------------

# test the effect of pop_size on the dataset ---------------------------------------
# Note: I have not tested pop-size effect on the response when using proportion data
# as pop.size is already taken into account when using a proportion, which is different
# compared to using only count data and a poisson
model1 <- glm(
  cbind(disappearance, wolfPopSize - disappearance) ~
    wolfPopSize_sc,
  family = binomial,
  data = modeldatascaled_municipality
)
summary(model1)
confint(model1)
with(summary(model1), 1 - deviance/null.deviance)

# Second-stage of analyses--------------------------------------------------------

# list of candidate models
Cand.mod <- list()

# wolf population size
Cand.mod[[1]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc,
    family = binomial,
    data = modeldatascaled_municipality)

# Poaching by retaliation and socio-economic context
Cand.mod[[2]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc +
      hunt_county2_sc +
      mean_income_prop_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )

# poaching by retaliation and easiness of access
Cand.mod[[3]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc +
      mean_ruggedness_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )

# global model
Cand.mod[[4]] <- glm(cbind(disappearance, wolfPopSize-disappearance) ~
                       wolfPopSize_sc +
                       hunt_county2 +
                       mean_income_prop_sc +
                       mean_ruggedness_sc,
                     family = binomial,
                     data = modeldatascaled_municipality)
# null model
Cand.mod[[5]] <- glm(cbind(disappearance, wolfPopSize-disappearance) ~
                       1,
                     family = binomial,
                     data = modeldatascaled_municipality)

Modnames <- c("Wolf Population Size",
              "Pop Size + Poaching + socioeconomic",
              "Pop Size + Poaching + access",
              "Global",
              "Null")

#Model selection table based on AIC
aictab1 <- aictab(cand.set = Cand.mod, modnames = Modnames)
aictab1
summary(Cand.mod[[1]])
summary(Cand.mod[[3]])
confint(Cand.mod[[1]])
confint(Cand.mod[[3]])
# explanatory power
with(summary(Cand.mod[[1]]), 1 - deviance/null.deviance)
# dispersion
with(summary(Cand.mod[[3]]), deviance/df.residual)
# Note: Low dispersion (1.01) I don't see a reason to try quasi-binomial
r2_nakagawa(Cand.mod[[3]])
#confint.merMod(Cand.mod[[1]], method = c("Wald"))

# third-stage of analyses--------------------------------------------------------

# list of candidate models
Cand.mod2 <- list()

# best second stage
Cand.mod2[[1]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )

# best second stage + inbreeding
Cand.mod2[[2]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc +
      mean_fi_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )

# best second stage + competition
Cand.mod2[[3]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc +
      mean_number_neighbour_terr_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )

# global model
Cand.mod2[[4]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc +
      mean_fi_sc +
      mean_number_neighbour_terr_sc,
    family = binomial,
    data = modeldatascaled_municipality
  )

# null model
Cand.mod2[[5]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      1,
    family = binomial,
    data = modeldatascaled_municipality
  )

#Assign names to each model
Modnames2 <- c("Wolf Population Size",
               "Wolf Population Size + Inbreeding",
               "Wolf Population Size + Competition",
               "Global",
               "Null")

#Model selection table based on AIC
aictab2 <- aictab(cand.set = Cand.mod2, modnames = Modnames2)
aictab2
write_csv2(aictab2,'output/binomial_municipalityLevel_aictab_stage3.csv')

summary(Cand.mod2[[1]])
summary(Cand.mod2[[2]])
confint(Cand.mod2[[1]])
confint(Cand.mod2[[2]])
with(summary(Cand.mod2[[4]]), 1 - deviance/null.deviance)

# save output
aictab_full <- bind_rows(aictab1, aictab2)
write.csv(
  aictab_full,
  file = 'binomial_municipalityLevel_aictab_stage2_and_3.csv')
