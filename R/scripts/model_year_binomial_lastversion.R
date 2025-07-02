## Model input data ##
## Andre P. Silva ##

# Note: add temporal correlation component here

# packages
library(AICcmodavg)


# data -------------------------------------------------------------------------
modeldatascaled_year <- read.csv2('modelInputData_new/df.year.csv') #%>%
  # select years where there was at least one cbind(disappearance,wolfPopSize-disappearance), excludes year 2000
  #filter(year %in% (2000:2017)) # match year range from Liberg et al 2020

View(modeldatascaled_year)

# First-stage of analyses--------------------------------------------------------

# test the effect of pop_size on the dataset ---------------------------------------
# Note: I have not tested pop-size effect on the response when using proportion data
# as pop.size is already taken into account when using a proportion, which is different
# compared to using only count data and a poisson
# wolf population size
model1 <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc,
    family = binomial,
    data = modeldatascaled_year
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
    data = modeldatascaled_year
  )

# Poaching by retaliation and socio-economic context
# Note: Not evaluated (all variables highly correlated)
# Cand.mod[[2]] <-
#   glm(
#     cbind(disappearance, wolfPopSize - disappearance) ~
#       wolfPopSize,
#     family = binomial,
#     data = modeldatascaled_year
#  )
# poaching by retaliation and easiness of access
Cand.mod[[2]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc +
      gravel_sc +
      snow_cover_sc +
      human_density_sc,
    family = binomial,
    data = modeldatascaled_year
  )
# global model
# Cand.mod[[4]] <-
#   glm(
#     cbind(disappearance, wolfPopSize - disappearance) ~
#       wolfPopSize_sc +
#       mean_ruggedness_sc +
#       mean_artificial_area_total_sc +
#       pop_mean_sc,
#     family = binomial,
#     data = modeldatascaled_year
#   )
# null model
Cand.mod[[3]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      1,
    family = binomial,
    data = modeldatascaled_year
  )

Modnames <- c("Wolf Population Size",
              #"Wolf Pop Size + Poaching + socioeconomic",
              "Wolf Pop Size + Poaching + access",
              #"Global",
              "Null")

#Model selection table based on AIC
aictab1 <- aictab(cand.set = Cand.mod, modnames = Modnames)
aictab1
summary(Cand.mod[[1]])
summary(Cand.mod[[2]])
confint(Cand.mod[[1]], level = 0.90)
confint(Cand.mod[[2]], level = 0.90)
# Note: needs model averaging here
with(summary(Cand.mod[[3]]), 1 - deviance / null.deviance)
#r2_nakagawa(Cand.mod[[3]])
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
    data = modeldatascaled_year
  )
# best second stage + inbreeding
Cand.mod2[[2]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      wolfPopSize_sc +
      inbreeding_disapp_sc,
    family = binomial,
    data = modeldatascaled_year
  )

# socioeconomic + competition 
# Note: Not evaluated (all variables highly correlated)
# Cand.mod2[[3]] <- glm(cbind(disappearance,wolfPopSize-disappearance) ~
#                         wolfPopSize_sc,
#                         mean_number_neighbour_terr_sc,
#                         mean_bear_density_BZtiff_sc,
#                       family = binomial,
#                       data = modeldatascaled_year)
# global model
# Cand.mod2[[3]] <- glm(cbind(disappearance,wolfPopSize-disappearance) ~
#                         wolfPopSize_sc,
#                         mean_fi_sc +
#                         mean_number_neighbour_terr,
#                         mean_bear_density_BZtiff_sc,
#                       family = binomial,
#                       data = modeldatascaled_year)
# null model
Cand.mod2[[3]] <-
  glm(
    cbind(disappearance, wolfPopSize - disappearance) ~
      1,
    family = binomial,
    data = modeldatascaled_year
  )

#Assign names to each model
# Modnames2 <- c("socioeconomic",
#                "socioeconomic + Inbreeding",
#                "socioeconomic + Competition",
#                "Global",
#                "Null")

Modnames2 <- c("Wolf Population Size",
               "Wolf Population Size + Inbreeding",
               #"Wolf Population Size + Competition",
               #"Global",
               "Null")

#Model selection table based on AIC
aictab2 <- aictab(cand.set = Cand.mod2, modnames = Modnames2)
aictab2
summary(Cand.mod2[[1]])
summary(Cand.mod2[[2]])
confint(Cand.mod2[[1]])
confint(Cand.mod2[[2]])
with(summary(Cand.mod2[[1]]), 1 - deviance/null.deviance)

# save output
aictab_full <- bind_rows(aictab1, aictab2)
write.csv(
  aictab_full,
  file = 'binomial_yearLevel_aictab_stage2_and_3.csv'
)
