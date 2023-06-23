## Model input data ##
## Andre P. Silva ##

to delete later
#Notes: save previosu file (github version to see legal vs illegal before modifications
#### to delete later
why is the coorrelation betwen Fi and max fi =1?
  cor(dataToModel$Fi,dataToModel$max_fi, use="complete.obs")

in the new data there seems to not exist a colum "x" at the end as in the
previous dats - not sure if has ny impact

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
  mutate(KommunerNamn = tolower(KommunerNamn))

# add new explanatory variables ------------------------------------------------
clean data should run with this one instead of previous data to Model
source("scripts/AdditionalExplanatoryData.R")

# covariate correlation --------------------------------------------------------

this now should use the output of AdditionalExplanatoryData.R

cormat <- round(cor(df.municipality[4:20], use = "complete.obs",
                    method = "spearman"),2)
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


# variables to exclude
VarsToExclude <- c("average_gravel_km",
                   "average_paved_km",
                   "ruggedness_mean",
                   "mean_artificial_area_total",
                   "mean_snow",
                   "mean_maxfi",
                   "sum_sheepAttacks_n",
                   "mean_IndividualIncome")

df_municipality.pruned <- df.municipality %>%
  select(-VarsToExclude)
# double check that no hilgly correlated variables are included
cormat <- round(cor(df_municipality.pruned[4:12], use = "complete.obs",
                    method = "spearman"),2)
cormat


# summarise per territory illegal (disappearance) vs ongoing -------------------

new function 


# dataToModel will be read in AdditionalExplanatoryData.R
dataToModel <- bind_rows(allyeardata) 
# count number of disappearances and ongoing territories per year
mastercountdf <- bind_rows(counts) %>%
  rowwise() %>%
  mutate(totalNoTerritories = sum(OngoingT, Illegals))
print(mastercountdf, n=Inf)

#

modeldata <- dataToModel6 %>%
  ungroup() %>% 
  select("Fate", "SLU.ID", "territory", "Autumn", "Spring", "ruggedness_mean",
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
         "first_yeardet","years_firstdet") %>%
  rename(fate = Fate,
         id = `SLU.ID`,
         Spring = Spring,
         terrRug = ruggedness_mean,
         humPop = pop_mean,
         fi = Fi,
         mooseHunt = hunt_county2,
         wolfNeighbour =  number_neighbour_terr,
         bearDensity = bear_density_BZtiff) %>%
  mutate(fate = recode(fate, 'illegal'=1)) %>%
  mutate(fate = replace_na(fate, 0))
head(modeldata)

table(modeldata$fate)

# scale data  
modeldatascaled <- modeldata %>% 
  mutate_at(c("terrRug", "humPop",
              "fi","mean_fi", "max_fi",
              "mooseHunt", "wolfNeighbour",
              "AttackDogs_n",
              "wolfLKill_n", "wolfLKill_last5y","wolfLKill_cumall",
              "sheepAttacks_n","NoAffectedSheep","NoAffectedSheep_last5y",
              "NoAffectedSheep_cumall",
              "IndividualIncome",
              "bearDensity"),
            ~(scale(.) %>% as.vector))
head(modeldatascaled)

write.csv(modeldatascaled,'data/modelInputData/survivalAnalysis_testdata.csv',
          row.names = F)

# summarise territory data per year --------------------------------------------
df.year <- - dataToModel6 %>%
  mutate(Fate_binary = ifelse(is.na(Fate), 0,1)) %>%
  mutate(year = Autumn,.before=mod_id) %>%
  group_by(year) %>%
  summarise(sum_fate = sum(Fate_binary),
            n_territories = n(),
            prop_disappearance = sum_fate/n_territories,
            mean_ruggedness = mean(ruggedness_mean),      
            average_gravel_km = mean(average_gravel_km),
            average_paved_km = mean(average_paved_km),
            pop_mean = mean(pop_mean),            
            mean_snow = mean(mean_snow),
            mean_artificial_area_total = mean(artificial_area_total),
            hunt_county2 = mean(hunt_county2),
            mean_fi = mean(Fi), 
            mean_maxfi = mean(max_fi),
            mean_number_neighbour_terr = mean(number_neighbour_terr),
            mean_bear_density_BZtiff = mean(bear_density_BZtiff),
            sum_wolfLKill_n = sum(wolfLKill_n),
            sum_wolfLKill_last5y = sum(wolfLKill_last5y),
            sum_sheepAttacks_n = sum(sheepAttacks_n),
            sum_NoAffectedSheep_last5y = sum(NoAffectedSheep_last5y),
            sum_AttackDogs_n = sum(AttackDogs_n),
            sum_AttackDogs_last5y = sum(AttackDogs_last5y),
            mean_IndividualIncome = mean(IndividualIncome)) 

# scale data  
VarsToScale <- c("pop_mean",
                 "hunt_county2", 
                 "mean_fi",
                 "mean_bear_density_BZtiff",
                 "wolfLKill_n",
                 "sheepAttacks_n",
                 "AttackDogs_n")

df.year_scaled <- df.year %>%
  mutate_at(VarsToScale,
            ~(scale(.) %>% as.vector))

write.csv(df.year_scaled,
          'data/modelInputData/df.year_scaled.csv', row.names = F)


# summarise territory data per municipality per year ---------------------------
df.municipality_year <- dataToModel6 %>%
  relocate(NAME_2, .before=mod_id) %>%
  mutate(Fate_binary = ifelse(is.na(Fate), 0,1)) %>%
  group_by(NAME_2, Autumn) %>%
  summarise(sum_fate = sum(Fate_binary),
            n_territories = n(),
            prop_disappearance = sum_fate/n_territories,
            NAME_1 = first(NAME_1),
            ruggedness_mean = mean(ruggedness_mean),      
            average_gravel_km = mean(average_gravel_km),
            average_paved_km = mean(average_paved_km),
            pop_mean = mean(pop_mean),            
            mean_snow = mean(mean_snow),
            mean_artificial_area_total = mean(artificial_area_total),
            hunt_county2 = mean(hunt_county2),
            mean_fi = mean(Fi), 
            mean_maxfi = mean(max_fi),
            mean_number_neighbour_terr = mean(number_neighbour_terr),
            mean_bear_density_BZtiff = mean(bear_density_BZtiff),
            # below metrics collected at the municipality level per year so only
            # the first element of the vector was used
            wolfLKill_n = first(wolfLKill_n),
            wolfLKill_last5y = first(wolfLKill_last5y),
            sheepAttacks_n = first(sheepAttacks_n),
            NoAffectedSheep_last5y = first(NoAffectedSheep_last5y),
            AttackDogs_n = first(AttackDogs_n),
            AttackDogs_last5y = first(AttackDogs_last5y),
            IndividualIncome = first(IndividualIncome)) %>%
  relocate(NAME_1,.after="NAME_2")

# double checking 
# head(df.municipality_year)
# table(df.municipality_year$sum_fate)
# sum((df.municipality_year$sum_fate))

# scale data  
VarsToScale <- c("pop_mean",
                 "hunt_county2", 
                 "mean_fi",
                 "mean_bear_density_BZtiff",
                 "wolfLKill_n",
                 "sheepAttacks_n",
                 "AttackDogs_n")

df.municipality_year_scaled <- df.municipality_year %>%
  mutate_at(VarsToScale,
            ~(scale(.) %>% as.vector))

write.csv(df.municipality_year_scaled,
          'data/modelInputData/df.municipality_year_scaled.csv', row.names = F)

# summarise territory data per municipality ---------------------------------------------------
df.municipality <- dataToModel6 %>%
  relocate(NAME_2, .before=mod_id) %>%
  mutate(Fate_binary = ifelse(is.na(Fate), 0,1)) %>%
  group_by(NAME_2) %>%
  summarise(NAME_1 = first(NAME_1),
            sum_fate = sum(Fate_binary),
            n_territories = n(),
            prop_disappearance = sum_fate/n_territories,
            ruggedness_mean = mean(ruggedness_mean),      
            average_gravel_km = mean(average_gravel_km),
            average_paved_km = mean(average_paved_km),
            pop_mean =  mean(pop_mean),            
            mean_snow = mean(mean_snow),
            mean_artificial_area_total = mean(artificial_area_total),
            mean_hunt_county2 = mean(hunt_county2),
            mean_fi  = mean(Fi), 
            mean_maxfi = mean(max_fi),
            mean_number_neighbour_terr = mean(number_neighbour_terr),
            mean_bear_density_BZtiff = mean(bear_density_BZtiff),
            sum_wolfLKill_n = sum(wolfLKill_n),
            sum_sheepAttacks_n = sum(sheepAttacks_n),
            sum_AttackDogs_n = sum(AttackDogs_n),
            mean_IndividualIncome = mean(IndividualIncome))

# double checking 
# head(df.municipality)
# table(df.municipality$sum_fate)
# sum((df.municipality$sum_fate))
# sum((df.municipality$n_territories))

# scale data  
VarsToScale <- c("pop_mean",
                 "hunt_county2", 
                 "mean_fi",
                 "mean_bear_density_BZtiff",
                 "wolfLKill_n",
                 "sheepAttacks_n",
                 "AttackDogs_n")

df.municipality_scaled <- df.municipality %>%
  mutate_at(VarsToScale,
            ~(scale(.) %>% as.vector))

write.csv(df.municipality,
          'data/modelInputData/df.municipality_scaled.csv', row.names = F)


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
