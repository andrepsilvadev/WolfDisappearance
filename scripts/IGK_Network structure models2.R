########################################################################################################
## ANALYSIS FOR:
## INTRAGUILD KILLING NETWORKS AMONG SOUTHERN AFRICA CARNIVORES
########################################################################################################

#######################################################################################################
#PART II. MODEL NETWORK STRUCTURE
#######################################################################################################

library(devtools)
#devtools::install_github("wpeterman/ResistanceGA", build_vignettes = TRUE) # Download package
#devtools::install_git("https://github.com/wpeterman/ResistanceGA") # Download package
library(lme4)
library(ecodist)
library(ResistanceGA)
library(ggplot2)
library(MuMIn)
library(igraph)
library(dplyr)
library(Matrix)
library(tibble)
library(AICcmodavg)

#######################################################################################################
#IMPORT DATA
#######################################################################################################

#IGK pairs: killer > killed ONLY (lower half of matrix)
#Compiled data Google images searches)
mInt <- read.csv("data/Adjacency matrix_GoogleImagesBINARY_byGeoRange_FINAL.csv")[,-1] #final dataset with revised species' IDs

#Node and Edge variables
#Body size (NODE) H: larger species kill more?
vMass <- read.csv("data/bodyMass.csv")
#Body size ratio (EDGE) H: at small and large differences attacks are less likely to occur, at intermediate differences killing interactions are frequent?
mbmRatio <- outer(vMass$mass_kg,vMass$mass_kg,FUN=function(X,Y){(X/Y)})
#Family-level taxonomy (EDGE) H: carnivores tend to interact more with species in the same family than with species in different families?
mFam <- read.csv("data/family_mtx.csv")[,-1]
# Predatory habits (NODE) H: carnivores highly adapted to kill vertebrate prey are more prone to killing interactions?
mPhab <- read.csv("data/PredHab.csv")
# Diet overlap (EDGE) H: dietary overlap among carnivores is correlated with high levels of interspecific aggression? 
mDiet <- read.csv("data/dietOverlap_mtx.csv")[,-1]


#Data from matrix:
spp <- colnames(mInt)
id <- ResistanceGA::To.From.ID(nrow(mInt))
int <- ResistanceGA:::lower(mInt)
intLit <- ResistanceGA:::lower(mLit)
bmr <- ResistanceGA:::lower(mbmRatio)
Fam <- ResistanceGA:::lower(mFam)
Diet <- ResistanceGA:::lower(mDiet)
bm <- log(vMass[,2])[as.numeric(as.character(id[,1]))]

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

#######################################################################################################
#RESULTS & MODEL FIT EVALUATION
#######################################################################################################

#Model selection based on AICc:
#  K   AICc Delta_AICc AICcWt Cum.Wt      LL
#Body-mass ratio * Bm   7 288.93       0.00   0.55   0.55 -137.37
#Body-mass ratio * Phab 7 289.32       0.39   0.45   1.00 -137.57
#Global                 8 306.81      17.89   0.00   1.00 -145.29
#Predatory habits       3 317.01      28.08   0.00   1.00 -155.48
#Body-mass * Phab       5 317.20      28.27   0.00   1.00 -153.55
#Body-mass ratio        4 320.17      31.25   0.00   1.00 -156.05
#Body-mass * fam        5 320.80      31.87   0.00   1.00 -155.35
#Body-mass              3 322.45      33.52   0.00   1.00 -158.20
#Body-mass ratio * fam  7 324.03      35.10   0.00   1.00 -154.92
#Diet overlap           3 330.63      41.71   0.00   1.00 -162.30
#Null                   2 332.25      43.33   0.00   1.00 -164.12
#Family taxonomy        3 333.71      44.78   0.00   1.00 -163.83

#EVIDENCE RATIO
evidence(aictab(cand.set = Cand.mod, modnames = Modnames))
#Evidence ratio between models 'Body-mass ratio * Bm' and 'Body-mass ratio * Phab':
# 1.22  

#CONFIDENCE SET
confset(cand.set = Cand.mod, modnames = Modnames, second.ord = TRUE,
        method = "raw")
#Confidence set for the best model
#Method:	 raw sum of model probabilities
#95% confidence set:
#                       K   AICc Delta_AICc AICcWt
#Body-mass ratio * Bm   7 288.93       0.00   0.55
#Body-mass ratio * Phab 7 289.32       0.39   0.45
#Model probabilities sum to 1 


#MODEL FIT

#r2_nakagawa() - marginal and conditional r-squared value for mixed effects models with complex random effects structures
library(performance)
r2_nakagawa(Cand.mod[[6]])
# R2 for Mixed Models
#  Conditional R2: 0.663
#  Marginal R2: 0.463

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





##FROM UNIVARIATE MODELS (Predatory habits, Body-mass ratio, Body-mass, Diet overlap, Family taxonomy)

#Predatory habits (node) 
Mphab <- Cand.mod[[4]]

pred <- data.frame(phab  = c("no", "yes"))

pred$pred <- plogis(predict(Mphab, newdata=pred,re.form=NA))
par(mfrow=c(1,1))
plot(pred$phab,pred$pred, type="n", las=1, 
     ylab="Pr(kills | PHab)", xlab="Predatory habits",ylim=c(0,1))


#Body mass ratio (edge) (always quadratic)
Mbmr <- Cand.mod[[2]]

mu.bmr <- attr(scale(bmr),which = "scaled:center")
sd.bmr <- attr(scale(bmr),which = "scaled:scale")

pred <- data.frame(bmr  = seq(min(mlpe.dat$bmr),max(mlpe.dat$bmr),length=100),
                   bmr2 = seq(min(mlpe.dat$bmr),max(mlpe.dat$bmr),length=100)^2,
                   real.bmr = seq(min(mlpe.dat$bmr),max(mlpe.dat$bmr),length=100)*
                   sd.bmr + mu.bmr)

pred$pred <- plogis(predict(Mbmr, newdata=pred,re.form=NA))

par(mfrow=c(1,1))
plot(pred$real.bmr,pred$pred, type="n", las=1, 
     ylab="Pr(kills | BMR)", xlab="Body Mass Ratio",ylim=c(0,1))
abline(v=c((1/2),(1/5.4)), col="grey", lty=2)
lines(pred$real.bmr,pred$pred/max(pred$pred), lwd=2,col=4)

#Body mass (node)
Mbm <- Cand.mod[[1]]

mu.bm <- attr(scale(bm),which = "scaled:center")
sd.bm <- attr(scale(bm),which = "scaled:scale")

pred <- data.frame(bm  = seq(min(mlpe.dat$bm),max(mlpe.dat$bm),length=100),
                   real.bm = seq(min(mlpe.dat$bm),max(mlpe.dat$bm),length=100)*
                     sd.bm + mu.bm)

pred$pred <- plogis(predict(Mbm, newdata=pred,re.form=NA))

par(mfrow=c(1,1))
plot(pred$real.bm,pred$pred, type="n", las=1, 
     ylab="Pr(kills | BM)", xlab="log(Body Mass)",ylim=c(0,1))
lines(pred$real.bm,pred$pred/max(pred$pred), lwd=2,col=4)

#Diet overlap (edge) 
Mdiet <- Cand.mod[[5]]

pred <- data.frame(diet  = seq(min(mlpe.dat$diet),max(mlpe.dat$diet),length=100))

pred$pred <- plogis(predict(Mdiet, newdata=pred,re.form=NA)) #very low probs

par(mfrow=c(1,1))
plot(pred$diet,pred$pred, type="n", las=1, 
     ylab="Pr(kills | Diet)", xlab="Dietary overlap",ylim=c(0,1))
lines(pred$diet,pred$pred, lwd=2,col=4)

#Family-level taxonomy (edge) 
Mfam <- Cand.mod[[3]]

pred <- data.frame(fam  = c("diff", "same"))

pred$pred <- plogis(predict(Mfam, newdata=pred,re.form=NA))

par(mfrow=c(1,1))
plot(pred$fam,pred$pred, type="n", las=1, 
     ylab="Pr(kills | Fam)", xlab="Family",ylim=c(0,1))