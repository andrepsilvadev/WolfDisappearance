# Generalized Linear Mixed-Effect Model (GLMM) ##
## Andre P. Silva ##

## libraries -------------------------------------------------------------------
source("scripts/libraries.R")
source("scripts/readData.R")

# data ------------------------------------------------------------------
modeldata <- cleandata %>%
  ungroup() %>% 
  filter(Fate %in% c("illegal","legal")) %>%
  select("Fate", "SLU-ID", "Spring", "ruggedness_mean","pop_mean", "Fi",
         "hunt_county", "number_neighbour_terr") %>%
  rename(fate = Fate,
         id = `SLU-ID`,
         Spring = Spring,
         terrRug = ruggedness_mean,
         humPop = pop_mean,
         fi = Fi,
         mooseHunt = hunt_county,
         wolfNeighbour =  number_neighbour_terr) %>%
  mutate(fate = as.factor(recode(fate, 'illegal'='1', 'legal'='0')))
head(modeldata)

# scaled data  
modeldatascaled <- modeldata %>% 
  mutate_at(c("terrRug", "humPop", "fi", "mooseHunt", "wolfNeighbour"),
            ~(scale(.) %>% as.vector)) %>%
  mutate(humPop2 = humPop^2)
head(modeldatascaled)

# list of candidate models
Cand.mod <- list()
# Poaching by retaliation and socio-economic context
Cand.mod[[1]] <- glmer(fate ~  sheepAttacks + killedWolves + IncAvg1998_2021 + (1 |Spring), data = modeldatascaled, family = binomial)
# poaching by retaliation and easiness of access
Cand.mod[[2]] <- glmer(fate ~ sheepAttacks + terrRug + humPop +  (1 | Spring), data = modeldatascaled, family = binomial)
# global model
Cand.mod[[3]] <- glmer(fate ~ sheepAttacks + killedWolves + IncAvg1998_2021 + terrRug + humPop + (1 | Spring), data = modeldatascaled, family = binomial)
# null model
Cand.mod[[4]] <- glmer(fate ~ 1 + (1 | Spring), data = modeldatascaled, family = binomial)

Modnames <- c("Poaching + socioeconomic",
              "Poaching + access",
              "Global",
              "Null")

#Model selection table based on AIC
aictab(cand.set = Cand.mod, modnames = Modnames)

# list of candidate models
Cand.mod2 <- list()
# Poaching by retaliation and socio-economic context
Cand.mod2[[1]] <- glmer(fate ~  sheepAttacks + killedWolves + (1 |Spring), data = modeldatascaled, family = binomial)
# best of poaching + inbreeding
Cand.mod2[[2]] <- glmer(fate ~ sheepAttacks + killedWolves + fi + (1 | Spring), data = modeldatascaled, family = binomial)
# best of poaching + competition
Cand.mod2[[3]] <- glmer(fate ~ sheepAttacks + killedWolves + wolfNeighbour + bearDensity + (1 | Spring), data = modeldatascaled, family = binomial)
# global model
Cand.mod2[[4]] <- glmer(fate ~ sheepAttacks + killedWolves + fi + wolfNeighbour + (1 | Spring), data = modeldatascaled, family = binomial)
# null model
Cand.mod2[[5]] <- glmer(fate ~ 1 + (1 | Spring), data = modeldatascaled, family = binomial)

# I have removed mooseHunt so far I dont understand the rationale
#Assign names to each model
Modnames2 <- c("Poaching + socioeconomic",
              "Poaching + Inbreeding",
              "Poaching + Competition",
              "Global",
              "Null")

#Model selection table based on AIC
aictab(cand.set = Cand.mod2, modnames = Modnames2)

#EVIDENCE RATIO
evidence(aictab(cand.set = Cand.mod, modnames = Modnames))

#CONFIDENCE SET
confset(cand.set = Cand.mod, modnames = Modnames, second.ord = TRUE,
        method = "raw")

#MODEL FIT
#r2_nakagawa() - marginal and conditional r-squared value for mixed effects models with complex random effects structures
library(performance)
r2_nakagawa(Cand.mod[[1]])

#confint.merMod(Cand.mod[[1]], method = c("boot"))
confint.merMod(Cand.mod[[1]], method = c("Wald"))
# see more here https://stackoverflow.com/questions/53120614/error-when-estimating-ci-for-glmm-using-confint

# observed vs predicted values plot (best model) --------------------------------------------
# Predict values
predictions <- predict(Cand.mod2[[1]]) # (not this is the best model from the second stage of model selection)

# Create a data frame with observed and predicted values
predictions.backtrans <- predictions*sd(modeldata$sheepAttacks) + mean(modeldata$sheepAttacks)
data <- data.frame(observed = modeldata$sheepAttacks, predicted = predictions.backtrans)

# Plot predicted vs observed values
ggplot(data, aes(x = observed, y = predictions.backtrans)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 45), breaks = c(seq(0,45, by = 5))) +
  scale_y_continuous(limits = c(0, 45), breaks = c(seq(0,45, by = 5))) +
  geom_smooth(method = "lm") + 
  labs(x = "Observed values", y = "Predicted") +
  title("Observed vs Predicted values - best model")
theme_classic()
# why do I have negative predicted values when backtransformed?


#univariate models for response prediction --------------------------------------------------
Cand.mod3 <- list()
Cand.mod3[[1]] <- glmer(fate ~ sheepAttacks + (1 |Spring), data = modeldatascaled, family = binomial)
Cand.mod3[[2]] <- glmer(fate ~ killedWolves + (1 |Spring), data = modeldatascaled, family = binomial)

library(ggeffects)

# model
mod1 <- Cand.mod3[[1]]

# prediction dataframe
predict.df1 <- data.frame(ggpredict(mod1, term = "sheepAttacks [all]"))

# add unscaled predictions to prediction dataframe
predict.df1$x_unscaled <- predict.df1$x*sd(modeldata$sheepAttacks) + mean(modeldata$sheepAttacks)

# plot response curve
predict_plot1 <- ggplot(data=predict.df1, aes(x=x_unscaled, y=predicted)) + 
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, by = 0.2))) + 
  xlab("Total number sheep attacks per municipality (1998-2021)") + 
  ylab("Probability of disappearance") +
  theme_classic()

# model
mod2 <- Cand.mod3[[2]]

# prediction dataframe
predict.df2 <- data.frame(ggpredict(mod2, term = "killedWolves [all]"))

# add unscaled predictions to prediction dataframe
predict.df2$x_unscaled <- predict.df2$x*sd(modeldata$killedWolves) + mean(modeldata$killedWolves)

# plot response curve
predict_plot2 <- ggplot(data=predict.df2, aes(x=x_unscaled, y=predicted)) + 
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), linetype=2, alpha=0.1) +
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0,1, by = 0.2))) + 
  xlab("Total number of legally killed wolves per municipality (1998-2021)") + 
  ylab("Probability of disappearance") +
  theme_classic()

# response plots
grid.arrange(predict_plot1,
             predict_plot2,
             nrow = 1,
             top = "Predicted probability (with 95% CI) for univariate models with best predictors")


# Residual diagnostics (DHARMa) ------------------------------------------------------------
fittedModel <- Cand.mod[[1]]
simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 250)
simulationOutput$scaledResiduals
# We would expect: 
#a uniform (flat) distribution of the overall residuals; 
#uniformity in y direction if we plot against any predictor.

#Plotting the scaled residuals
plot(simulationOutput)
plot(simulationOutput, asFactor = T) #Should we plot as factor for a Binomial response?

#Formal goodness-of-fit tests on the scaled residuals
testResiduals(simulationOutput)
#ZeroInflation test
testZeroInflation(simulationOutput)
testDispersion(simulationOutput)
testSpatialAutocorrelation(simulationOutput) 

confint(modGlobal)

r2_nakagawa(modGlobal)

see interpretation of glmer models, calculation of confidence intervals and  interpretations
https://data.library.virginia.edu/getting-started-with-binomial-generalized-linear-mixed-models/
  
  model assumptions
chrome-extension://nlaealbpbmpioeidemdfedkfmglobidl/https://www.st-andrews.ac.uk/media/ceed/students/mathssupport/mixedeffectsknir.pdf


















# based on IGK script -> doublecheck
sheepAttacks <- Cand.mod3[[1]]
pred <- data.frame(disappearance  = c("no", "yes"))

mu.sheepAttacks <- attr(scale(modeldata$sheepAttacks),
                     which = "scaled:center")
sd.sheepAttacks <- attr(scale(modeldata$sheepAttacks),
                     which = "scaled:scale")

pred <- data.frame(sheepAttacks  = seq(min(modeldatascaled$sheepAttacks),
                              max(modeldatascaled$sheepAttacks),
                              length=100),
                   real.sheepAttacks = (seq(min(modeldatascaled$sheepAttacks),
                                        max(modeldatascaled$sheepAttacks),length=100)*
                     sd.sheepAttacks) + mu.sheepAttacks)


pred$pred <- plogis(predict(Cand.mod3[[1]], newdata=pred,re.form=NA))

# response curve for killed wolves
killedwolvesM <- Cand.mod[[9]]

mu.killedwolves <- attr(scale(modeldata$killedWolves),
                     which = "scaled:center")
sd.killedwolves <- attr(scale(modeldata$killedWolves),
                     which = "scaled:scale")

pred2 <- data.frame(killedWolves  = seq(min(modeldatascaled$killedWolves),
                                    max(modeldatascaled$killedWolves),
                                    length=100),
                   real.killedWolves = (seq(min(modeldatascaled$killedWolves),
                                         max(modeldatascaled$killedWolves),length=100)*
                                       sd.killedwolves) + mu.killedwolves)

pred2$pred <- plogis(predict(Cand.mod3[[2]], newdata=pred2,re.form=NA, se.fit=TRUE))

par(mfrow=c(1,2))
plot(pred$real.sheepAttacks,pred$pred, type="l", las=1, 
     ylab="Pr(disap. | sheepAttacks)",
     xlab="Total number of dog attacks per municipality", ylim=c(0,1))
lines(pred$real.sheepAttacks,pred$pred, lwd=2,col=4)
# lines(pred$real.sheepAttacks,pred$pred/max(pred$pred), lwd=2,col=4) why was it divided by max ?
plot(pred2$real.killedWolves,pred2$pred, type="l", las=1, 
     ylab="Pr(disap. | killedwolves)",
     xlab="Total number of legally killed Wolves per municipality", ylim=c(0,1))
lines(pred2$real.killedWolves,pred2$pred, lwd=2,col=4)

# dont forget that variables need to be re-done to match the year of the variable with the year of disappearance


#see potential options for confidence intervals
PI <- predictInterval(merMod = Cand.mod[[9]], newdata = pred,
                      level = 0.95, n.sims = 1000,
                      stat = "mean", type="linear.prediction",
                      include.resid.var = TRUE)


pred <- data.frame(fit = apply(modeldatascaled$killedWolves, 2, function(x) as.numeric(quantile(x, probs=.5, na.rm=TRUE))),
                   lwr = apply(modeldatascaled$killedWolves, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE))),
                   upr = apply(modeldatascaled$killedWolves, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
boot3 <- lme4::bootMer(Cand.mod[[9]], predict, nsim=250, use.u=TRUE, type="semiparametric")

se also example of confidence intervals with bootMer
https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html







# individual, sex, year as random effects (how to account for multiple random effects?)

#Data input
mlpe.dat <- data.frame(int = int, #IGK matrix from Google Images
                       int.lit = intLit, #IGK matrix from Literature
                       killer = spp[as.numeric(as.character(id[,1]))],
                       killed = spp[as.numeric(as.character(id[,2]))], 
                       real.bm = vMass[,2][as.numeric(as.character(id[,1]))],
                       bm = scale(log(vMass[,2]))[as.numeric(as.character(id[,1]))], #Log body mass (scaled)
                       real.bmr = bmr,
                       bmr = scale(bmr), #Body mass ratio (scaled)
                       bmr2 = scale(bmr)^2, #Body mass quadratic term
                       fam = Fam[as.numeric(as.character(id[,1]))], #Same/different family binary indicator (edge)
                       phab = mPhab[,2][as.numeric(as.character(id[,1]))], #Predatory habits (preys on medium or large mammals) binary indicator (node)
                       diet = Diet #Diet overlap (edge) 
)

mlpe.dat <- mlpe.dat[complete.cases(mlpe.dat),]



#######################################################################################################
#MODEL FITTING AND SELECTION (Generalized Linear Mixed-Effects Models with "Killer" as random effect)
#######################################################################################################

#Set up candidate models in a list
Cand.mod <- list()
#A. Univariate
Cand.mod[[1]]  <- glmer(int ~ bm + (1 | killer), data = mlpe.dat, family = binomial) #Body mass (node)
Cand.mod[[2]]  <- glmer(int ~ bmr + bmr2 + (1 | killer), data = mlpe.dat, family = binomial) #Body mass ratio (edge) (always quadratic)
Cand.mod[[3]]  <- glmer(int ~ fam + (1 | killer), data = mlpe.dat, family = binomial) #Family-level taxonomy (edge) 
Cand.mod[[4]] <- glmer(int ~ phab + (1 | killer), data = mlpe.dat, family = binomial) #Predatory habits (node) 
Cand.mod[[5]] <- glmer(int ~ diet + (1 | killer), data = mlpe.dat, family = binomial) #Diet overlap (edge) 
#B. With interactions
Cand.mod[[6]]  <- glmer(int ~ (bmr+bmr2)*bm + (1 | killer), data = mlpe.dat, family = binomial) #Effect of body mass ratio DEPENDS on body mass of killer
Cand.mod[[7]]  <- glmer(int ~ (bmr+bmr2)*phab + (1 | killer), data = mlpe.dat, family = binomial) #Effect of body mass ratio DEPENDS on predatory habits of killer
Cand.mod[[8]]  <- glmer(int ~ (bmr+bmr2)*fam + (1 | killer), data = mlpe.dat, family = binomial) #Effect of body mass ratio DEPENDS on family-level taxonomy of killer/killed
Cand.mod[[9]]  <- glmer(int ~ bm*phab + (1 | killer), data = mlpe.dat, family = binomial) #Effect of body mass DEPENDS on predatory habits of killer
Cand.mod[[10]]  <- glmer(int ~ bm*fam + (1 | killer), data = mlpe.dat, family = binomial) #Effect of body mass DEPENDS on family-level taxonomy of killer/killed
#C. Global
Cand.mod[[11]] <- glmer(int ~ bm + bmr + bmr2 + fam + phab + diet + (1 | killer), data = mlpe.dat, family = binomial) #Global model
#D. Null
Cand.mod[[12]] <- glmer(int ~ 1 + (1 | killer), data = mlpe.dat, family = binomial) 

#Assign names to each model
Modnames <- c("Body-mass", "Body-mass ratio", "Family taxonomy", "Predatory habits", "Diet overlap", 
              "Body-mass ratio * Bm", "Body-mass ratio * Phab", "Body-mass ratio * fam", 
              "Body-mass * Phab", "Body-mass * fam",
              "Global", "Null")

#Model selection table based on AIC
aictab(cand.set = Cand.mod, modnames = Modnames)

#EVIDENCE RATIO
evidence(aictab(cand.set = Cand.mod, modnames = Modnames))

#CONFIDENCE SET
confset(cand.set = Cand.mod, modnames = Modnames, second.ord = TRUE,
        method = "raw")

#MODEL FIT
#r2_nakagawa() - marginal and conditional r-squared value for mixed effects models with complex random effects structures
library(performance)
r2_nakagawa(Cand.mod[[6]])

# Residual diagnostics (DHARMa)
library(DHARMa)
#citation("DHARMa")

fittedModel <- Cand.mod[[6]]

simulationOutput <- simulateResiduals(fittedModel = fittedModel, n = 250)
#For example, a scaled residual value of 0.5 means that half of the simulated data 
#are higher than the observed value, and half of them lower. 
#A value of 0.99 would mean that nearly all simulated data are lower than the observed value.
#The minimum/maximum values for the residuals are 0 and 1.

simulationOutput$scaledResiduals
# We would expect: 
#a uniform (flat) distribution of the overall residuals; 
#uniformity in y direction if we plot against any predictor.

#Plotting the scaled residuals
plot(simulationOutput)
plot(simulationOutput, asFactor = T) #Should we plot as factor for a Binomial response?

#Formal goodness-of-fit tests on the scaled residuals
testResiduals(simulationOutput)

#$uniformity
#One-sample Kolmogorov-Smirnov test
#data:  simulationOutput$scaledResiduals
#D = 0.05193, p-value = 0.07411
#alternative hypothesis: two-sided

#$dispersion
#DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
#data:  simulationOutput
#ratioObsSim = 0.99293, p-value = 0.896
#alternative hypothesis: two.sided

#$outliers
#DHARMa outlier test based on exact binomial test
#data:  simulationOutput
#outLow = 4.0000e+00, outHigh = 2.0000e+00, nobs = 6.1100e+02, freqH0 = 3.9841e-03, p-value = 1
#alternative hypothesis: two.sided

#$uniformity
#One-sample Kolmogorov-Smirnov test
#data:  simulationOutput$scaledResiduals
#D = 0.05193, p-value = 0.07411
#alternative hypothesis: two-sided

#$dispersion
#DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated
#data:  simulationOutput
#ratioObsSim = 0.99293, p-value = 0.896
#alternative hypothesis: two.sided

#$outliers
#DHARMa outlier test based on exact binomial test
#data:  simulationOutput
#outLow = 4.0000e+00, outHigh = 2.0000e+00, nobs = 6.1100e+02, freqH0 = 3.9841e-03, p-value = 1
#alternative hypothesis: two.sided

#ZeroInflation test
testZeroInflation(simulationOutput)

#Calculating residuals per group
simulationOutputGroup = recalculateResiduals(simulationOutput, group = mlpe.dat$killer)
#Repeat previous tests


#######################################################################################################
#MODEL PREDICTIONS
#######################################################################################################

#FROM BEST MODEL (Body-mass ratio * Bm)
Mbmrbm <- Cand.mod[[6]]

#get scaled bodymass of killer species
scaled_killer_bm <- unique(cbind(spp[as.numeric(as.character(id[,1]))], scale(log(vMass[,2]))[as.numeric(as.character(id[,1]))]))

#Create prediction values
mu.bmr <- attr(scale(bmr),which = "scaled:center")
sd.bmr <- attr(scale(bmr),which = "scaled:scale")

pred <- data.frame(bmr  = rep(seq(min(mlpe.dat$bmr),max(mlpe.dat$bmr),length=100),5),
                   bmr2 = rep(seq(min(mlpe.dat$bmr),max(mlpe.dat$bmr),length=100)^2,5),
                   real.bmr = rep(seq(min(mlpe.dat$bmr),max(mlpe.dat$bmr),length=100) * sd.bmr + mu.bmr,5),
                   bm = rep(c(2.307,1.550,1.457,1.651,1.098),each=100), #Set body mass value for important killer species (e.g. Lion, Leopard)
                   spp = rep(c("Lion","Leopard", "Cheetah", "Spotted hyena", "Wild dog"),each=100))

pred$pred <- plogis(predict(Mbmrbm, newdata=pred,re.form=NA))

#PLOT
ggplot(pred,aes(x=real.bmr, y=pred,group=spp,color=spp)) +
  geom_line(size=0.5) +
  geom_vline(xintercept = c((1/2),(1/5.4)), col="grey", size=1) + #suggested 2-5.4 body-mass ratio
  theme_classic()
  