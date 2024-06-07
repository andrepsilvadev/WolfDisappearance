## Model input data ##
## Andre P. Silva ##

# Notes:
# save previous file/code (github version to replicate legal vs illegal before
# modifications why is the correlation between Fi and max fi not =1?
# since I select the territory by mean Fi it is expected that I do not have
# a correlation of 1 with the Fi of the individual it was selected still 0.6
# seems a bit low - indication of wide variation in the data?
  
# raw data ---------------------------------------------------------------------
data <-
  readr::read_csv2(
    "data/rawData/FateWolvesTerritory1998_2020_Scandinavia_Final_CDB.csv",
    locale = locale(encoding = "ISO-8859-1")
  )

# data clean -------------------------------------------------------------------
idvars <- c(colnames(data)[1:13], "KommunerNamn", "SLU.ID")
explanatoryvars <- c("ruggedness_mean",
                     "average_gravel_km",
                     "average_paved_km",
                     "artificial_area_total",
                     "pop_mean",
                     "mean_snow",
                     "hunt_afo",
                     "hunt_county2",
                     "Fi",
                     "number_neighbour_terr",
                     "bear_density_BZtiff")

varstoselect <- c(idvars, explanatoryvars)

cleandata <- data %>%
  dplyr::select(all_of(varstoselect)) %>%
  mutate(mod_id = 1:nrow(data)) %>% 
  relocate(mod_id, .before = id) %>% 
  mutate(Fate = str_replace(Fate, "Legal", "legal")) %>%
  mutate(Fate = str_replace(Fate, "Illegal", "illegal")) %>%
  mutate(Fate = str_replace(Fate, "censor", "censored")) %>%
  mutate(Fate = str_replace(Fate, "cernsored", "censored")) %>%
  mutate(Fate = str_replace(Fate, "censoreded", "censored")) %>%
  mutate(KommunerNamn = tolower(KommunerNamn)) %>%
  ungroup()

# add new explanatory variables ------------------------------------------------
source("R/scripts/AdditionalExplanatoryData.R")
length(unique(dataToModel8$territory)) # 435 all territories
nrow(dataToModel8) # 1920 all territories
unique(dataToModel8$Fate)

# territory selection ----------------------------------------------------------
# Note: function tested by Cecilia and Giorgia
# territorySelection_testFunctionsCeciliaGiorgia.R
# inconsistencies detected for instance when there is combination of fates such
# as "illegal", "other" - 6 territories and "illegal", "legal" - 12 territories 
territoriesToModel <- dataToModel8 %>%
  group_by(territory) %>%
  # only keep territories that have "NA","illegal" or,"censored" in their fate
  # by excluding other fates
  filter(!any(Fate %in% c("other","legal","natural","traffic"))) %>% # 291
  # keep the row with the maximum inbreeding, if tied keep the first row
  group_by(territory, Autumn) %>%
  slice_max(mean_fi, with_ties=FALSE) %>% 
  # add column to double-check potential duplicate rows within territories 
  #mutate(Autumn_dup = duplicated(Autumn)) %>%
  ungroup() %>%
  mutate(Fate_binary = ifelse(is.na(Fate), 0,1),.after=Fate) %>%
  arrange(territory, Autumn)

length(unique(territoriesToModel$territory)) # 291
nrow(territoriesToModel) # 642 only one individual per territory
unique(territoriesToModel$Fate)

# count disappearances and ongoing territories per year ------------------------
df <- territoriesToModel %>% 
  group_by(Spring) %>%
  summarise(
    ongoing = sum(is.na(Fate)==TRUE),
    disappearance = sum(Fate %in% c("illegal" ,"censored")),
    sum = ongoing + disappearance,
    prop_disappearance = round(disappearance/sum, 2),
    n()
  )

sum(df$disappearance) # 193 # double checking
sum(df$ongoing) # 449 # double checking
print(df, n=30)

# plot disappearance rate - it does not match the plot from Liberg et al. 2020
# I cannot calculate prop. disappearance and total number of territories
# like this because I am excluding territories with other fates (it influences the sum)
# what are the implications for the territory analyses?
# the plot
ggplot(data=df, aes(x=Spring, y=prop_disappearance, group=1)) +
  geom_line()+
  geom_point()

# covariate correlation --------------------------------------------------------
colnames(territoriesToModel) # output AdditionalExplanatoryData.R
territoriesToModel2 <- territoriesToModel[19:44]

cormat <- round(
  cor(
    territoriesToModel2,
    use = "pairwise.complete.obs",
    method = "spearman"),
  2)

cormat
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p2 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4.5)
p2  

# highlyCorrelatedVars <- melted_cormat %>%
#   filter(value >0.5 | value < -0.5)
#highlyCorrelatedVars

# correlation between metrics for the same variable
VarsToTest <- c("AttackDogs_last5y",
                "AttackDogs_n",
                "AttackDogs_cumall") # keep AttackDogs_last5y

VarsToTest <- c("NoAffectedSheep_cumall",
                "NoAffectedSheep_last5y",
                "NoAffectedSheep",
                "sheepAttacks_n") # keep NoAffectedSheep_last5y

VarsToTest <- c("wolfLKill_cumall",
                "wolfLKill_last5y",
                "wolfLKill_n") # keep wolfLKill_last5y

VarsToTest <- c("hunt_afo",
                "hunt_county2") # not correlated but keep hunt_county2 (density)

VarsToTest <- c("average_gravel_km",
                "average_paved_km") # keep average_paved_km

VarsToTest <- c("IndividualIncome",
                "income_prop") # keep income_prop

VarsToTest <- c("mean_fi",
                "max_fi") # keep mean_fi

# correlation between covariates representing the same mechanism
VarsToTest <- c("ruggedness_mean",
                "average_paved_km",
                "artificial_area_total",
                "pop_mean",
                "mean_snow") # keep ruggedness_mean, artificial_area_total, pop_mean

VarsToTest <- c("AttackDogs_last5y",
                "NoAffectedSheep_last5y",
                "wolfLKill_last5y",
                "hunt_county2",
                "income_prop") # keep AttackDogs_last5y, wolfLKill_last5y, hunt_county2, income_prop  

VarsToTest <- c("bear_density_BZtiff",
                "number_neighbour_terr") # keep both

subset <- territoriesToModel %>%
   select(all_of(VarsToTest))

sub_cormat <- round(
  cor(
    subset,
    use = "pairwise.complete.obs",
    method = "spearman"),2
)

upper_tri <- get_upper_tri(sub_cormat)
sub_melted_cormat <-
  reshape2::melt(upper_tri, na.rm = TRUE)

p2_sub <- ggplot(sub_melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1,
    size = 12
  )) +
  theme(axis.text.y = element_text(size = 12)) +
  geom_text(aes(Var2, Var1, label = value),
            color = "white",
            size = 4)
p2_sub

# check correlation between selected covariates representing different mechanisms
VarsToKeep <- c("wolfPopSize",
                "AttackDogs_last5y",
                "wolfLKill_last5y",
                "income_prop",
                #"hunt_county2", removed due to high correlation with wolfPopSize
                "ruggedness_mean",
                "artificial_area_total",
                "pop_mean",
                "mean_fi",
                #"bear_density_BZtiff", # removed due to correlation with 
                #ruggedness_mean and pop_mean
                "number_neighbour_terr") # not standardized by the mean

# double check that no highly correlated variables are included   
data_withoutCorrVars <- territoriesToModel %>%
  select(all_of(VarsToKeep))

View(data_withoutCorrVars)

# change column names
colnames(data_withoutCorrVars) <- c("WolfPopSize","DogAttacks_last5y","LegWolfHarv_last5y",
                                    "IncomeProp","TerrRug","ArtificialArea","HumanDens",
                                    "Mean_InbreedCoef","WolfNeighbourTerr")

sub_cormat <- round(
  cor(
    data_withoutCorrVars, #subset in case running VarsToTest
    use = "pairwise.complete.obs",
    method = "spearman"),2
)

upper_tri <- get_upper_tri(sub_cormat)
sub_melted_cormat <-
  reshape2::melt(upper_tri, na.rm = TRUE)

p2_sub <- ggplot(sub_melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1,
    size = 18
  )) +
  theme(axis.text.y = element_text(size = 18)) +
  geom_text(aes(Var2, Var1, label = value),
            color = "white",
            size = 8)
p2_sub

# Calculate min, max and mean of covariates------------------------------------
min_cov <- format(sapply(df.territory,min, na.rm = TRUE), scientific = FALSE)
View(min_cov)
min_cov <- as.data.frame(min_cov)

max_cov <- format(sapply(df.territory,max, na.rm = TRUE), scientific = FALSE)
View(max_cov)
max_cov <- as.data.frame(max_cov)

mean_cov <- format(sapply(df.territory,mean, na.rm = TRUE), scientific = FALSE)
View(mean_cov)
mean_cov <- as.data.frame(mean_cov)

var <- data.frame(min_cov, max_cov, mean_cov)
View(var)

write.csv(var, "info_variables_territory.csv")

# currently highest correlation among selected variables is 0.51

# model data scaled ------------------------------------------------------------
# for some reason scale with dplyr and mutate was producing nan
# count data not scaled
modeldatascaled <- territoriesToModel
modeldatascaled$ruggedness_mean_sc <- scale(modeldatascaled$ruggedness_mean)[,1] 
modeldatascaled$average_gravel_km_sc <- scale(modeldatascaled$average_gravel_km)[,1] 
modeldatascaled$average_paved_km_sc <- scale(modeldatascaled$average_paved_km)[,1]       
modeldatascaled$artificial_area_total_sc <- scale(modeldatascaled$artificial_area_total)[,1] 
modeldatascaled$pop_mean_sc <- scale(modeldatascaled$pop_mean)[,1]               
modeldatascaled$mean_snow_sc <- scale(modeldatascaled$mean_snow)[,1] 
modeldatascaled$hunt_afo_sc <- scale(modeldatascaled$hunt_afo)[,1]              
modeldatascaled$hunt_county2_sc <- scale(modeldatascaled$hunt_county2)[,1] 
# modeldatascaled$number_neighbour_terr_sc <- scale(modeldatascaled$number_neighbour_terr)[,1]  # count data - not scaled 
modeldatascaled$bear_density_BZtiff_sc <- scale(modeldatascaled$bear_density_BZtiff)[,1] 
modeldatascaled$Fi_sc <- scale(modeldatascaled$Fi)[,1] 
modeldatascaled$mean_fi_sc <- scale(modeldatascaled$mean_fi)[,1] 
modeldatascaled$max_fi_sc <- scale(modeldatascaled$max_fi)[,1]               
# modeldatascaled$wolfLKill_cumall_sc <- scale(modeldatascaled$wolfLKill_cumall)[,1]  # count data - not scaled 
# modeldatascaled$wolfLKill_last5y_sc <- scale(modeldatascaled$wolfLKill_last5y)[,1]  # count data - not scaled     
# modeldatascaled$wolfLKill_n_sc <- scale(modeldatascaled$wolfLKill_n)[,1]  # count data - not scaled 
# modeldatascaled$NoAffectedSheep_cumall_sc <- scale(modeldatascaled$NoAffectedSheep_cumall)[,1]  # count data - not scaled 
# modeldatascaled$NoAffectedSheep_last5y_sc <- scale(modeldatascaled$NoAffectedSheep_last5y)[,1]  # count data - not scaled 
# modeldatascaled$NoAffectedSheep_sc <- scale(modeldatascaled$NoAffectedSheep)[,1]  # count data - not scaled       
# modeldatascaled$sheepAttacks_n_sc <- scale(modeldatascaled$sheepAttacks_n)[,1]  # count data - not scaled 
# modeldatascaled$AttackDogs_cumall_sc <- scale(modeldatascaled$AttackDogs_cumall)[,1]  # count data - not scaled      
# modeldatascaled$AttackDogs_last5y_sc <- scale(modeldatascaled$AttackDogs_last5y)[,1]  # count data - not scaled 
# modeldatascaled$AttackDogs_n_sc <- scale(modeldatascaled$AttackDogs_n)[,1]  # count data - not scaled         
modeldatascaled$IndividualIncome_sc <- scale(modeldatascaled$IndividualIncome)[,1] 
modeldatascaled$income_prop_sc <- scale(modeldatascaled$income_prop)[,1]
# modeldatascaled$wolfPopSize <- scale(modeldatascaled$wolfPopSize)[,1]  # count data - not scaled         
modeldatascaled <- as_tibble(modeldatascaled)

# territory data
write_csv2(modeldatascaled,'data/modelInputData/df.territory.csv')

# summarise territory data per year --------------------------------------------
# adopted from territory selection but includes all fates to calculate pop.size
yearlyTerritoryStats <- 
  dataToModel8 %>%
  group_by(territory, Spring) %>%
  slice_max(mean_fi, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(territory, Spring) %>%
  mutate(year = Spring, .before = mod_id) %>%
  group_by(year) %>%
  summarise(
    wolfPopSize = length(unique(territory)),
    disappearance = sum(Fate == "illegal", na.rm = TRUE),
    n_territories_without_disap = wolfPopSize - disappearance,
    prop_disappearance = disappearance / wolfPopSize,
  )
#head(yearlyTerritoryStats, n=30)

yearlyTerritoryFeatures <- territoriesToModel %>%
  mutate(year = Spring, .before = mod_id) %>%
  filter(Fate_binary == 1) %>% # filter only disappearances
  group_by(year) %>%
  summarise(
    mean_ruggedness = mean(ruggedness_mean, na.rm = TRUE),
    average_gravel_km = mean(average_gravel_km, na.rm = TRUE),
    average_paved_km = mean(average_paved_km, na.rm = TRUE),
    pop_mean = mean(pop_mean, na.rm = TRUE),
    mean_snow = mean(mean_snow, na.rm = TRUE),
    mean_artificial_area_total = mean(artificial_area_total, na.rm = TRUE),
    hunt_county2 = mean(hunt_county2, na.rm = TRUE),
    mean_fi = mean(mean_fi, na.rm = TRUE),
    mean_maxfi = mean(max_fi, na.rm = TRUE),
    mean_number_neighbour_terr = mean(number_neighbour_terr, na.rm = TRUE),
    mean_bear_density_BZtiff = mean(bear_density_BZtiff, na.rm = TRUE),
    sum_wolfLKill_n = sum(wolfLKill_n, na.rm = TRUE),
    sum_wolfLKill_last5y = sum(wolfLKill_last5y, na.rm = TRUE),
    sum_sheepAttacks_n = sum(sheepAttacks_n, na.rm = TRUE),
    sum_NoAffectedSheep_last5y = sum(NoAffectedSheep_last5y, na.rm = TRUE),
    sum_AttackDogs_n = sum(AttackDogs_n, na.rm = TRUE),
    sum_AttackDogs_last5y = sum(AttackDogs_last5y, na.rm = TRUE),
    mean_IndividualIncome = mean(IndividualIncome, na.rm = TRUE),
    mean_income_prop = mean(income_prop, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(year)

df.year <- left_join(yearlyTerritoryStats,
                     yearlyTerritoryFeatures,
                     by = c('year' = 'year')) 

# scale covariates
df.year$mean_ruggedness_sc = scale(df.year$mean_ruggedness)[,1]      
df.year$average_gravel_km_sc = scale(df.year$average_gravel_km)[,1] 
df.year$average_paved_km_sc = scale(df.year$average_paved_km)[,1] 
df.year$pop_mean_sc = scale(df.year$pop_mean)[,1]            
df.year$mean_snow_sc = scale(df.year$mean_snow)[,1] 
df.year$mean_artificial_area_total_sc = scale(df.year$mean_artificial_area_total)[,1]
df.year$hunt_county2_sc = scale(df.year$hunt_county2)[,1]
df.year$mean_fi_sc = scale(df.year$mean_fi)[,1] 
df.year$mean_maxfi_sc = scale(df.year$mean_maxfi)[,1]
#df.year$mean_number_neighbour_terr_sc = scale(df.year$mean_number_neighbour_terr)[,1] # count data - not scaled 
df.year$mean_bear_density_BZtiff_sc = scale(df.year$mean_bear_density_BZtiff)[,1]
#sum_wolfLKill_n = wolfLKill_n # count data - not scaled 
#sum_wolfLKill_last5y = wolfLKill_last5y # count data - not scaled 
#sum_sheepAttacks_n = sheepAttacks_n # count data - not scaled 
#sum_NoAffectedSheep_last5y = NoAffectedSheep_last5y # count data - not scaled 
#sum_AttackDogs_n = AttackDogs_n # count data - not scaled 
#sum_AttackDogs_last5y = AttackDogs_last5y # count data - not scaled 
df.year$mean_IndividualIncome_sc = scale(df.year$mean_IndividualIncome)[,1] # count data - not scaled 
df.year$mean_income_prop_sc = scale(df.year$mean_income_prop)[,1] # count data - not scaled 
# df.year$wolfPopSize <- scale(df.year$wolfPopSize)[,1]  # count data - not scaled         

write_csv2(df.year,
           'data/modelInputData/df.year.csv')

# covariate correlation
# Note: covariate correlation had to be calculated for the different scales as 
# big discrepancies between scales were detected
colnames(df.year)
df.year2 <- df.year[2:24]

covToCorrelate <- df.year2 %>%
  select(-c("disappearance", "n_territories_without_disap"))

cormat <- round(
  cor(
    covToCorrelate,
    use = "pairwise.complete.obs",
    method = "spearman"),
  2)

cormat
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p3 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 5)
p3  


# covariate selection correlation between metrics for the same variable
VarsToTest <- c("sum_AttackDogs_last5y",
                "sum_AttackDogs_n") # keep sum_AttackDogs_last5y

VarsToTest <- c("sum_sheepAttacks_n",
                "sum_NoAffectedSheep_last5y") # keep sum_NoAffectedSheep_last5

VarsToTest <- c("sum_wolfLKill_n",
                "sum_wolfLKill_last5y") # keep sum_wolfLKill_last5y

VarsToTest <- c("average_gravel_km",
                "average_paved_km") # keep both

VarsToTest <- c("mean_IndividualIncome",
                "mean_income_prop") # keep mean_income_prop

VarsToTest <- c("mean_fi",
                "mean_maxfi") # keep mean_fi

# correlation between covariates representing the same mechanism
VarsToTest <- c("mean_ruggedness",
                "average_gravel_km",
                "average_paved_km",
                "mean_artificial_area_total",
                "pop_mean",
                "mean_snow") # keep mean_ruggedness, average_gravel_km, pop_mean, mean_snow  

VarsToTest <- c("sum_AttackDogs_last5y",
                "sum_NoAffectedSheep_last5y",
                "sum_wolfLKill_last5y",
                "hunt_county2",
                "mean_income_prop") # keep sum_AttackDogs_last5y, mean_income_prop  

VarsToTest <- c("mean_bear_density_BZtiff",
                "mean_number_neighbour_terr") # keep mean_number_neighbour_terr

subset <- covToCorrelate %>%
  select(all_of(VarsToTest))

sub_cormat <- round(
  cor(
    subset,
    use = "pairwise.complete.obs",
    method = "spearman"),2
)

upper_tri <- get_upper_tri(sub_cormat)
sub_melted_cormat <-
  reshape2::melt(upper_tri, na.rm = TRUE)

p3_sub <- ggplot(sub_melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1,
    size = 12
  )) +
  theme(axis.text.y = element_text(size = 12)) +
  geom_text(aes(Var2, Var1, label = value),
            color = "white",
            size = 4)
p3_sub

# check correlation between covariates representing different mechanisms
VarsToKeep <- c(
  "wolfPopSize",
  #"sum_AttackDogs_last5y", # remove due to high correlation with
  # wolfPopSize
  "mean_income_prop",
  "mean_ruggedness",
  "average_gravel_km",
  "pop_mean",
  "mean_snow",
  "mean_fi"
  #"mean_number_neighbour_terr", # remove due to high correlation with 
  # wolfPopSize
) 

# double check that no highly correlated variables are included   
data_withoutCorrVars <- covToCorrelate %>%
  select(all_of(VarsToKeep))

View(data_withoutCorrVars)

# change column names
colnames(data_withoutCorrVars) <- c("WolfPopSize","IncomeProp","TerrRug",
                                    "GravelRoad","HumanDens","SnowCover",
                                    "Mean_InbreedCoef")

sub_cormat <- round(
  cor(
    data_withoutCorrVars,
    use = "pairwise.complete.obs",
    method = "spearman"),2
)

upper_tri <- get_upper_tri(sub_cormat)
sub_melted_cormat <-
  reshape2::melt(upper_tri, na.rm = TRUE)

p3_sub <- ggplot(sub_melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1,
    size = 16
  )) +
  theme(axis.text.y = element_text(size = 16)) +
  geom_text(aes(Var2, Var1, label = value),
            color = "white",
            size = 8)
p3_sub

# Calculate min, max and mean of covariates------------------------------------

min_cov <- format(sapply(df.year,min, na.rm = TRUE), scientific = FALSE)
View(min_cov)
min_cov <- as.data.frame(min_cov)

max_cov <- format(sapply(df.year,max, na.rm = TRUE), scientific = FALSE)
View(max_cov)
max_cov <- as.data.frame(max_cov)

mean_cov <- format(sapply(df.year,mean, na.rm = TRUE), scientific = FALSE)
View(mean_cov)
mean_cov <- as.data.frame(mean_cov)

var <- data.frame(min_cov, max_cov, mean_cov)
View(var)

write.csv(var, "info_variables_population.csv")

# Plots for covariates--------------------------------------------------------
# packages necessary
library(ggplot2)
library(cowplot)

##########################################################
# calculate coefficients for each variable

# format
max_cov_t <- as.data.frame(t(max_cov))
str(max_cov_t)
max_cov_t[c(1:37)] <- as.numeric(max_cov_t[c(1:37)])

# coefficients
coeff_income <- max_cov_t$wolfPopSize/max_cov_t$mean_IndividualIncome

coeff_artificial <- max_cov_t$wolfPopSize/max_cov_t$mean_artificial_area_total

coeff_bear <- max_cov_t$wolfPopSize/max_cov_t$mean_bear_density_BZtiff

coeff_roads <- max_cov_t$wolfPopSize/max_cov_t$average_paved_km

coeff_moose <- max_cov_t$wolfPopSize/max_cov_t$hunt_county2

coeff_wolvesKilled <- max_cov_t$wolfPopSize/max_cov_t$sum_wolfLKill_last5y

coeff_attacksSheep <- max_cov_t$wolfPopSize/max_cov_t$sum_NoAffectedSheep_last5y

coeff_attacksDogs <- max_cov_t$wolfPopSize/max_cov_t$sum_AttackDogs_last5y

##########################################################
# plots
# DISCRETE
plot_wolvesKilled <- 
  ggplot(modeldatascaled_year)  + 
  scale_linetype_manual("", values = 1) +
  geom_col(aes(x=year, y=sum_wolfLKill_last5y, group = 1, fill = "#660000"))+
  geom_line(aes(x=year, y=wolfPopSize / coeff_wolvesKilled, linetype = "Wolf population size"), colour = "black")+
  scale_y_continuous(name = "Number of wolves killed",
                     sec.axis = sec_axis(~.*coeff_wolvesKilled, name = "Wolf population size")) +
  scale_fill_manual('', values = '#660000', label = 'Number of wolves killed (5 years)') +
  theme_light() +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.y.right = element_text(size=7),
    legend.position = "bottom"
  ) +
  xlab("Year")

###
plot_dogs <- 
  ggplot(modeldatascaled_year)  + 
  scale_linetype_manual("", values = 1) +
  geom_col(aes(x=year, y=sum_AttackDogs_last5y, group = 1, fill = "tan2"))+
  geom_line(aes(x=year, y=wolfPopSize / coeff_attacksDogs, linetype = "Wolf population size"), colour = "black")+
  scale_y_continuous(name = "Number of dogs attacked",
                     sec.axis = sec_axis(~.*coeff_attacksDogs, name = "Wolf population size")) +
  scale_fill_manual('', values = 'tan2', label = 'Number of dogs attacked (5 years)') +
  theme_light() +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.y.right = element_text(size=7),
    legend.position = "bottom"
  ) +
  xlab("Year")

###
plot_sheep <- 
  ggplot(modeldatascaled_year)  + 
  scale_linetype_manual("", values = 1) +
  geom_col(aes(x=year, y=sum_NoAffectedSheep_last5y, group = 1, fill = "sienna3"))+
  geom_line(aes(x=year, y=wolfPopSize / coeff_attacksSheep, linetype = "Wolf population size"), colour = "black")+
  scale_y_continuous(name = "Number of sheep attacked",
                     sec.axis = sec_axis(~.*coeff_attacksSheep, name = "Wolf population size")) +
  scale_fill_manual('', values = 'sienna3', label = 'Number of sheep attacked (5 years)') +
  theme_light() +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.y.right = element_text(size=7),
    legend.position = "bottom"
  ) +
  xlab("Year")

plot_grid(plot_wolvesKilled,plot_sheep,plot_dogs)

###
# CONTINUOUS

plot_income <- ggplot(modeldatascaled_year, aes(x = year)) +
  geom_line(aes(y = mean_IndividualIncome, colour = "Individual income"), linewidth = 0.8) +
  geom_line(aes(y = wolfPopSize / coeff_income, colour = "Wolf population size"), linewidth = 0.8) +
  scale_color_manual(values = c("#006600", "black")) +
  scale_y_continuous(name = "Individual income",
                     sec.axis = sec_axis(~.*coeff_income, name = "Wolf population size")) +
  theme_light() +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.y.right = element_text(size=7),
    legend.position = "bottom"
  ) +
  labs(x = "Year",
       colour = "")

###
plot_artificial <- ggplot(modeldatascaled_year, aes(x = year)) +
  geom_line(aes(y = mean_artificial_area_total, colour = "Artifical area"), linewidth = 0.8) +
  geom_line(aes(y = wolfPopSize / coeff_artificial, colour = "Wolf population size"), linewidth = 0.8) +
  scale_color_manual(values = c("#9966CC", "black")) +
  scale_y_continuous(name = "Artificial area",
                     sec.axis = sec_axis(~.*coeff_artificial, name = "Wolf population size")) +
  theme_light() +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.y.right = element_text(size=7),
    legend.position = "bottom"
  ) +
  labs(x = "Year",
       colour = "")

###
plot_bear <- ggplot(modeldatascaled_year, aes(x = year)) +
  geom_line(aes(y = mean_bear_density_BZtiff, colour = "Bear density"), linewidth = 0.8) +
  geom_line(aes(y = wolfPopSize / coeff_bear, colour = "Wolf population size"), linewidth = 0.8) +
  scale_color_manual(values = c("#0066CC", "black")) +
  scale_y_continuous(name = "Bear density",
                     sec.axis = sec_axis(~.*coeff_bear, name = "Wolf population size")) +
  theme_light() +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.y.right = element_text(size=7),
    legend.position = "bottom"
  ) +
  labs(x = "Year",
       colour = "")

###
plot_roads <- ggplot(modeldatascaled_year, aes(x = year)) +
  geom_line(aes(y = average_paved_km, colour = "Length of paved roads"), linewidth = 0.8) +
  geom_line(aes(y = wolfPopSize / coeff_roads, colour = "Wolf population size"), linewidth = 0.8) +
  scale_color_manual(values = c("#3399CC", "black")) +
  scale_y_continuous(name = "Length of paved roads",
                     sec.axis = sec_axis(~.*coeff_roads, name = "Wolf population size")) +
  theme_light() +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.y.right = element_text(size=7),
    legend.position = "bottom"
  ) +
  labs(x = "Year",
       colour = "")

###
plot_moose <- ggplot(modeldatascaled_year, aes(x = year)) +
  geom_line(aes(y = hunt_county2, colour = "Moose density"), linewidth = 0.8) +
  geom_line(aes(y = wolfPopSize / coeff_moose, colour = "Wolf population size"), linewidth = 0.8) +
  scale_color_manual(values = c("#FF9999", "black")) +
  scale_y_continuous(name = "Moose density",
                     sec.axis = sec_axis(~.*coeff_moose, name = "Wolf population size")) +
  theme_light() +
  theme(
    axis.title.y = element_text(size=10),
    axis.title.y.right = element_text(size=7),
    legend.position = "bottom"
  ) +
  labs(x = "Year",
       colour = "")

###
plot_grid(plot_wolvesKilled,plot_sheep,plot_dogs,plot_income, plot_artificial, plot_moose, nrow=3)



# summarise territory data per municipality ------------------------------------
municipalityTerritoryStats <-
  dataToModel8 %>%
  group_by(territory, Spring) %>%
  slice_max(mean_fi, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(territory, Spring) %>%
  mutate(year = Spring, .before = mod_id) %>%
  group_by(NAME_2) %>%
  summarise(
    wolfPopSize = length(unique(territory)),
    disappearance = sum(Fate == "illegal", na.rm = TRUE),
    n_territories_without_disap = wolfPopSize - disappearance,
    prop_disappearance = disappearance / wolfPopSize,
  ) 

#head(municipalityTerritoryStats, n=30)
# proportion of disappearance per municipality
p4.1 <-
  ggplot(municipalityTerritoryStats,
         aes(x = reorder(NAME_2,+prop_disappearance),
             y = prop_disappearance)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))
p4.1

municipalityTerritoryFeatures <-
  territoriesToModel %>%
  mutate(year = Spring, .before = mod_id) %>%
  filter(Fate_binary == 1) %>% # filter only disappearances
  group_by(NAME_2) %>%
  summarise(
    mean_ruggedness = mean(ruggedness_mean, na.rm = TRUE),
    average_gravel_km = mean(average_gravel_km, na.rm = TRUE),
    average_paved_km = mean(average_paved_km, na.rm = TRUE),
    pop_mean = mean(pop_mean, na.rm = TRUE),
    mean_snow = mean(mean_snow, na.rm = TRUE),
    mean_artificial_area_total = mean(artificial_area_total, na.rm = TRUE),
    hunt_county2 = mean(hunt_county2, na.rm = TRUE),
    mean_fi = mean(mean_fi, na.rm = TRUE),
    mean_maxfi = mean(max_fi, na.rm = TRUE),
    mean_number_neighbour_terr = mean(number_neighbour_terr, na.rm = TRUE),
    mean_bear_density_BZtiff = mean(bear_density_BZtiff, na.rm = TRUE),
    sum_wolfLKill_n = sum(wolfLKill_n, na.rm = TRUE),
    sum_wolfLKill_last5y = sum(wolfLKill_last5y, na.rm = TRUE),
    sum_sheepAttacks_n = sum(sheepAttacks_n, na.rm = TRUE),
    sum_NoAffectedSheep_last5y = sum(NoAffectedSheep_last5y, na.rm = TRUE),
    sum_AttackDogs_n = sum(AttackDogs_n, na.rm = TRUE),
    sum_AttackDogs_last5y = sum(AttackDogs_last5y, na.rm = TRUE),
    mean_IndividualIncome = mean(IndividualIncome, na.rm = TRUE),
    mean_income_prop = mean(income_prop, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(NAME_2)

municipalityTerritoryStats <-
  municipalityTerritoryStats %>%
  filter(
    NAME_2 %in% unique(municipalityTerritoryFeatures$NAME_2)
  )

df.municipality <-
  left_join(
    municipalityTerritoryStats,
    municipalityTerritoryFeatures,
    by = c('NAME_2' = 'NAME_2')
  )

view(df.municipality)
# double checking 
# head(df.municipality)
# table(df.municipality$sum_fate)
# sum((df.municipality$sum_fate))
# sum((df.municipality$n_territories))

# scale covariates
df.municipality$mean_ruggedness_sc = scale(df.municipality$mean_ruggedness)[,1]      
df.municipality$average_gravel_km_sc = scale(df.municipality$average_gravel_km)[,1] 
df.municipality$average_paved_km_sc = scale(df.municipality$average_paved_km)[,1] 
df.municipality$pop_mean_sc = scale(df.municipality$pop_mean)[,1]            
df.municipality$mean_snow_sc = scale(df.municipality$mean_snow)[,1] 
df.municipality$mean_artificial_area_total_sc = scale(df.municipality$mean_artificial_area_total)[,1]
df.municipality$hunt_county2_sc = scale(df.municipality$hunt_county2)[,1]
df.municipality$mean_fi_sc = scale(df.municipality$mean_fi)[,1] 
df.municipality$mean_maxfi_sc = scale(df.municipality$mean_maxfi)[,1]
#df.municipality$mean_number_neighbour_terr_sc = scale(df.municipality$mean_number_neighbour_terr)[,1] # count data - not scaled 
df.municipality$mean_bear_density_BZtiff_sc = scale(df.municipality$mean_bear_density_BZtiff)[,1]
#sum_wolfLKill_n = wolfLKill_n # count data - not scaled 
#sum_wolfLKill_last5y = wolfLKill_last5y # count data - not scaled 
#sum_sheepAttacks_n = sheepAttacks_n # count data - not scaled 
#sum_NoAffectedSheep_last5y = NoAffectedSheep_last5y # count data - not scaled 
#sum_AttackDogs_n = AttackDogs_n # count data - not scaled 
#sum_AttackDogs_last5y = AttackDogs_last5y # count data - not scaled 
df.municipality$mean_IndividualIncome_sc = scale(df.municipality$mean_IndividualIncome)[,1] # count data - not scaled 
df.municipality$mean_income_prop_sc = scale(df.municipality$mean_income_prop)[,1] # count data - not scaled 

write_csv2(df.municipality,
           'data/modelInputData/df.municipality.csv')

# covariate correlation
# Note: covariate correlation had to be calculated for the different scales as 
# big discrepancies between scales were detected
colnames(df.municipality)
df.municipality2 <- df.municipality[2:24]

covToCorrelate <- df.municipality2 %>%
  select(-c("disappearance", "n_territories_without_disap"))

cormat <- round(
  cor(
    covToCorrelate,
    use = "pairwise.complete.obs",
    method = "spearman"),
  2)

cormat
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p4.2 <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, size = 13)) +
  theme(axis.text.y = element_text(size = 13)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 5)
p4.2  

# highlyCorrelatedVars <- melted_cormat %>%
#   filter(value >0.5 | value < -0.5)
# #highlyCorrelatedVars

# covariate selection correlation between metrics for the same variable
VarsToTest <- c("sum_AttackDogs_last5y",
                "sum_AttackDogs_n") # keep sum_AttackDogs_last5y

VarsToTest <- c("sum_sheepAttacks_n",
                "sum_NoAffectedSheep_last5y") # keep sum_NoAffectedSheep_last5y

VarsToTest <- c("sum_wolfLKill_n",
                "sum_wolfLKill_last5y") # keep sum_wolfLKill_last5y

VarsToTest <- c("average_gravel_km",
                "average_paved_km") # keep average_gravel_km

VarsToTest <- c("mean_IndividualIncome",
                "mean_income_prop") # keep mean_income_prop

VarsToTest <- c("mean_fi",
                "mean_maxfi") # keep mean_fi

# correlation between covariates representing the same mechanism
VarsToTest <- c("mean_ruggedness",
                "average_gravel_km",
                "mean_artificial_area_total",
                "pop_mean",
                "mean_snow") # keep pop_mean, mean_ruggedness

VarsToTest <- c("sum_AttackDogs_last5y",
                "sum_NoAffectedSheep_last5y",
                "sum_wolfLKill_last5y",
                "hunt_county2",
                "mean_income_prop") # keep sum_AttackDogs_last5y, sum_wolfLKill_last5y, hunt_county2, mean_income_prop

VarsToTest <- c("mean_bear_density_BZtiff",
                "mean_number_neighbour_terr") # keep both

subset <- covToCorrelate %>%
  select(all_of(VarsToTest))

sub_cormat <- round(
  cor(
    subset,
    use = "pairwise.complete.obs",
    method = "spearman"),2
)

upper_tri <- get_upper_tri(sub_cormat)
sub_melted_cormat <-
  reshape2::melt(upper_tri, na.rm = TRUE)

p3_sub <- ggplot(sub_melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1,
    size = 12
  )) +
  theme(axis.text.y = element_text(size = 12)) +
  geom_text(aes(Var2, Var1, label = value),
            color = "white",
            size = 4)
p3_sub

# check correlation between covariates representing different mechanisms
VarsToKeep <- c(
  "wolfPopSize",
  "pop_mean",
  #"mean_snow", # remove mean_snow due to correlation with bear_density and pop_mean
  #"sum_AttackDogs_last5y", # highly correlated with wolfPopSize
  #"sum_wolfLKill_last5y", # highly correlated with wolfPopSize
  "hunt_county2",
  "mean_income_prop",
  "mean_fi",
  #"mean_bear_density_BZtiff", # highly correlated with wolfPopSize
  "mean_number_neighbour_terr"
) 

# double check that no highly correlated variables are included   
data_withoutCorrVars <- covToCorrelate %>%
  select(all_of(VarsToKeep))

View(data_withoutCorrVars)

# change column names
colnames(data_withoutCorrVars) <- c("WolfPopSize","HumanDens","MooseHarv_county",
                                    "IncomeProp","Mean_InbreedCoef","WolfNeighbourTerr")

sub_cormat <- round(
  cor(
    data_withoutCorrVars,
    use = "pairwise.complete.obs",
    method = "spearman"),2
)

upper_tri <- get_upper_tri(sub_cormat)
sub_melted_cormat <-
  reshape2::melt(upper_tri, na.rm = TRUE)

p3_sub <- ggplot(sub_melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1,
    size = 16
  )) +
  theme(axis.text.y = element_text(size = 16)) +
  geom_text(aes(Var2, Var1, label = value),
            color = "white",
            size = 8)
p3_sub

# Calculate min, max and mean of covariates------------------------------------
min_cov <- format(sapply(df.municipality,min, na.rm = TRUE), scientific = FALSE)
View(min_cov)
min_cov <- as.data.frame(min_cov)

max_cov <- format(sapply(df.municipality,max, na.rm = TRUE), scientific = FALSE)
View(max_cov)
max_cov <- as.data.frame(max_cov)

mean_cov <- format(sapply(df.municipality,mean, na.rm = TRUE), scientific = FALSE)
View(mean_cov)
mean_cov <- as.data.frame(mean_cov)

var <- data.frame(min_cov, max_cov, mean_cov)
View(var)

write.csv(var, "info_variables_municipality.csv")

# summarise per territory - illegal (disappearance) vs legal fate --------------
modeldata <- cleandata.join1 %>%
  ungroup() %>% 
  filter(Fate %in% c("illegal","legal")) %>%
  select("Fate", "SLU.ID", "Spring", "ruggedness_mean","pop_mean", "Fi",
         "hunt_county2", "number_neighbour_terr", "dogattack",
         "artificial_area_total",
         "killedWolves", "killedsheep","IncAvg1998_2021",
         "bear_density_BZtiff") %>%
  rename(fate = Fate,
         id = `SLU.ID`,
         Spring = Spring,
         terrRug = ruggedness_mean,
         humPop = pop_mean,
         fi = Fi,
         mooseHunt = hunt_county2,
         wolfNeighbour =  number_neighbour_terr,
         sheepAttacks = killedsheep,
         bearDensity = bear_density_BZtiff) %>%
  mutate(fate = as.factor(recode(fate, 'illegal'='1', 'legal'='0')))
head(modeldata)

# scaled data  
modeldatascaled <- modeldata %>% 
  mutate_at(c("terrRug", "humPop", "fi", "mooseHunt", "wolfNeighbour",
              "artificial_area_total", "dogattack", "killedWolves",
              "IncAvg1998_2021", "sheepAttacks", "bearDensity"),
            ~(scale(.) %>% as.vector)) %>%
  mutate(humPop2 = humPop^2)
head(modeldatascaled)

write.csv(
  modeldata,
  'data/modelInputData/modelInputDataLegalvsIllegal_unscaled.csv',
  row.names = F
)

write.csv(
  modeldatascaled,
  'data/modelInputData/modelInputDataLegalvsIllegal_scaled.csv',
  row.names = F
)

# correlation graphs with only the chosen variables ----------------------------
## territory level -------------------------------------------------------------
territoriesToModel3 <- territoriesToModel[c(19:24,26,28:30,33,36,40,43:44)]
colnames(territoriesToModel3)
     
# change column names
colnames(territoriesToModel3) <- c("TerrRug","GravelRoad","PavedRoad",
                                   "ArtificialArea","HumanDens","SnowCover",
                                   "MooseHarv","WolfNeighbourTerr","BearHarv",
                                   "InbreedCoef","LegWolfHarv",
                                   "SheepAttacks","DogAttacks",
                                   "IncomeProp","WolfPopSize")

cormat <- round(
     cor(
         territoriesToModel3,
         use = "pairwise.complete.obs",
         method = "spearman"),
     2)
cormat
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p_terr <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
     geom_tile(color = "white") +
     scale_fill_viridis_c() +
     ggtitle(label = "Spearman correlation - continuous variables") + 
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                      hjust = 1, size = 16)) +
     theme(axis.text.y = element_text(size = 16)) +
     geom_text(aes(Var2, Var1, label = value), color = "white", size = 6)
p_terr  

## population level ------------------------------------------------------------
df.year3 <- df.year[c(2,6:13,15:16,18,20,22,24)]
colnames(df.year3)

# change column names
colnames(df.year3) <- c("WolfPopSize","TerrRug","GravelRoad","PavedRoad",
                        "HumanDens","SnowCover","ArtificialArea","MooseHarv",
                        "InbreedCoef","WolfNeighbourTerr","BearHarv",
                        "LegWolfHarv","SheepAttacks","DogAttacks","IncomeProp")
cormat <- round(
     cor(
         df.year3,
         use = "pairwise.complete.obs",
         method = "spearman"),
     2)
cormat
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p_pop <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
     geom_tile(color = "white") +
     scale_fill_viridis_c() +
     ggtitle(label = "Spearman correlation - continuous variables") + 
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                      hjust = 1, size = 16)) +
     theme(axis.text.y = element_text(size = 16)) +
     geom_text(aes(Var2, Var1, label = value), color = "white", size = 6)
p_pop

## municipality level ----------------------------------------------------------
df.municipality3 <- df.municipality[c(2,6:13,15:16,18,20,22,24)]
colnames(df.municipality3)

# change column names
colnames(df.municipality3) <- c("WolfPopSize","TerrRug","GravelRoad","PavedRoad",
                        "HumanDens","SnowCover","ArtificialArea","MooseHarv",
                        "InbreedCoef","WolfNeighbourTerr","BearHarv",
                        "LegWolfHarv","SheepAttacks","DogAttacks","IncomeProp")
cormat <- round(
  cor(
    df.municipality3,
    use = "pairwise.complete.obs",
    method = "spearman"),
  2)
cormat
upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)

p_mun <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, size = 16)) +
  theme(axis.text.y = element_text(size = 16)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 6)
p_mun
