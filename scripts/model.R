# Generalized Linear Mixed-Effect Model (GLMM) ##
## Andre P. Silva ##

## libraries -------------------------------------------------------------------
source("scripts/libraries.R")

# data
modeldata <- cleandata %>%
  ungroup() %>% 
  filter(Fate %in% c("illegal","legal")) %>%
  select("Fate", "ruggedness_mean","pop_mean", "Fi",
         "hunt_county", "number_neighbour_terr") %>%
  rename(fate = Fate,
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
            ~(scale(.) %>% as.vector))
head(modeldatascaled)




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