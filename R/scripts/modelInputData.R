## Model input data ##
## Andre P. Silva ##

# Notes:
# save previous file/code (github version to replicate legal vs illegal before modifications
# why is the correlation betwen Fi and max fi not =1?
# since I select the territory by mean Fi it is expected that I do not have
# a correlation of 1 with the Fi of the individual it was selected still 0.6
# seems a bit low - indication of wide variation in the data?
  
# raw data ---------------------------------------------------------------------
data <- readr::read_csv2("data/rawData/FateWolvesTerritory1998_2020_Scandinavia_Final_CDB.csv",
                         locale = locale(encoding = "ISO-8859-1"))

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
source("scripts/AdditionalExplanatoryData.R")
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

cormat <- round(
  cor(
    territoriesToModel[19:44],
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
                                   hjust = 1, size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)
p2  

highlyCorrelatedVars <- melted_cormat %>%
  filter(value >0.5 | value < -0.5)
#highlyCorrelatedVars

# variables to exclude
VarsToExclude <- c("ruggedness_mean",
                   "average_gravel_km",
                   "average_paved_km",
                   "artificial_area_total",
                   "mean_snow",
                   "Fi",
                   "max_fi",
                   "hunt_afo",
                   "wolfLKill_n",
                   "wolfLKill_cumall",
                   "NoAffectedSheep_last5y",
                   "NoAffectedSheep_cumall",
                   "sheepAttacks_n",
                   "AttackDogs_n",
                   "AttackDogs_cumall",
                   "IndividualIncome") # not standardized by the mean

# double check that no highly correlated variables are included   
data_withoutCorrVars <- territoriesToModel %>%
  select(-VarsToExclude)

sub_cormat <- round(
  cor(
    data_withoutCorrVars[19:28],
    use = "pairwise.complete.obs",
    method = "spearman"),2
  )

upper_tri <- get_upper_tri(sub_cormat)
sub_melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)         

p2_sub <- ggplot(sub_melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  ggtitle(label = "Spearman correlation - continuous variables") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust = 1, size = 12)) +
  theme(axis.text.y = element_text(size = 12)) +
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)
p2_sub

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
yearlyTerritoryStats <- dataToModel8 %>%
  group_by(territory, Spring) %>%
  slice_max(mean_fi, with_ties=FALSE) %>%
  ungroup() %>%
  arrange(territory, Spring) %>%
  mutate(year = Spring,.before=mod_id) %>%
  group_by(year) %>%
  summarise(disappearance = sum(Fate == "illegal", na.rm=TRUE),
            n_territories_without_disap = n()-disappearance,
            n_territories = n_territories_without_disap+disappearance,
            prop_disappearance = disappearance/n_territories)

head(yearlyTerritoryStats, n=30)

ggplot(data=yearlyTerritoryStats, aes(x=year, y=prop_disappearance, group=1)) +
  geom_line()+
  geom_point()

ggplot(data=yearlyTerritoryStats, aes(x=year, y= n_territories_without_disap, group=1)) +
  geom_line()+
  geom_point()

it still does not extactly match the plot form liberg et al 2020

yearlyTerritoryFeatures <- territoriesToModel %>%
  mutate(year = Spring,.before=mod_id) %>%
  filter(Fate_binary == 1) %>% # filter only disappearances
  group_by(year) %>%
  summarise(mean_ruggedness = mean(ruggedness_mean, na.rm = TRUE),      
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
            n=n()) %>%
  arrange(year)

df.year <- left_join(yearlyTerritoryStats,
                     yearlyTerritoryFeatures,
             by=c('year'='year')) 

# view(df.year)


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

write_csv2(df.year,
          'data/modelInputData/df.year.csv')

# summarise territory data per municipality ------------------------------------
municipalityTerritoryStats <- dataToModel8 %>%
  group_by(territory, Spring) %>%
  slice_max(mean_fi, with_ties=FALSE) %>%
  ungroup() %>%
  arrange(territory, Spring) %>%
  mutate(year = Spring,.before=mod_id) %>%
  group_by(NAME_2) %>%
  summarise(disappearance = sum(Fate == "illegal", na.rm=TRUE),
            n_territories_without_disap = n()-disappearance,
            n_territories = n_territories_without_disap+disappearance,
            prop_disappearance = disappearance/n_territories)

head(municipalityTerritoryStats, n=30)

p3 <- ggplot(municipalityTerritoryStats, aes(x=reorder(NAME_2,+prop_disappearance), y=prop_disappearance)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

municipalityTerritoryFeatures <- territoriesToModel %>%
  mutate(year = Spring,.before=mod_id) %>%
  filter(Fate_binary == 1) %>% # filter only disappearances
  group_by(NAME_2) %>%
  summarise(mean_ruggedness = mean(ruggedness_mean, na.rm = TRUE),      
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
            n=n()) %>%
  arrange(NAME_2)

df.municipality <- left_join(municipalityTerritoryStats,
                     municipalityTerritoryFeatures,
                     by=c('NAME_2'='NAME_2')) 

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

write.csv(modeldata,
          'data/modelInputData/modelInputDataLegalvsIllegal_unscaled.csv',
          row.names = F)
write.csv(modeldatascaled,
          'data/modelInputData/modelInputDataLegalvsIllegal_scaled.csv',
          row.names = F)
