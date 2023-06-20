## Model input data ##
## Andre P. Silva ##

# using distinct in the ongoing territories means that I am selecting the first row
# what is the impact on inbreeding?

# dataold <- readr::read_csv("data/rawData/FateWolvesTerritory1998_2020_Scandinavia_Final.csv")
# moose and bear data update by cecillia
# I have double checked the count of territories per year with both datasets and it is the same
# in the new data there seems to not exist a colum "x" at the end as in the previous dats - not sure if has ny impact
data <- readr::read_csv2("data/rawData/FateWolvesTerritory1998_2020_Scandinavia_Final_CDB.csv",
                         locale = locale(encoding = "ISO-8859-1"))
idvars <- c(colnames(data)[1:13], "KommunerNamn", "SLU.ID")
explanatoryvars <- c("ruggedness_mean", "average_gravel_km", "average_paved_km",
                     "pop_mean", "mean_snow", "hunt_afo", "hunt_county2",
                     "Fi", "number_neighbour_terr",
                     "bear_density_BZtiff","artificial_area_total")
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

# calculate average and maximum inbreeding per territory -----------------------
fi.df <- cleandata %>%
  select(territory, Fi) %>% 
  group_by(territory) %>%
  summarize(mean_fi = mean(Fi),
            max_fi = max(Fi))

# head(fi.df)
# nrow(fi.df)

cleandata <- left_join(cleandata,
                        fi.df,
                        by = c('territory'))


why is the coorrelation betwen Fi and max fi =1?
  cor(dataToModel$Fi,dataToModel$max_fi, use="complete.obs")
  
# model input data to compare illegal (disappearance) vs ongoing territories ---

#inbreeding needs to be changed for average inbreeding

years <- seq(1999,2020,1) # keep location only for previous year before last year monitoring to be sure we are working with pairs that are alive

allyeardata <- list()
counts <- list()
for (i in 1:length(years)) {
  ongoing.territories <- cleandata %>%
    ungroup() %>%
    filter(Fate %in% c(NA)) %>% # meaning fate in 2020 (last year monitoring) was NA (alive)
    filter(Spring == years[i]) %>% # keep location only for previous year before last year monitoring to be sure we are working with pairs that are alive
    group_by(territory) %>%
    slice_max(Fi, with_ties=FALSE) # keeps the row with the maximum inbreeding
  
  illegals <- cleandata %>%
    ungroup() %>%
    group_by(territory) %>%
    arrange(desc(Spring)) %>%
    filter(Spring == years[i]) %>%
    filter(Fate %in% c("illegal")) %>%
    # keep the row with the maximum inbreeding, do not keep ties 
    slice_max(Fi, with_ties=FALSE) 
  
  yeardata <- bind_rows(ongoing.territories, illegals)
  yearname <- paste(years[i])
  allyeardata[[yearname]] <- yeardata 
  
  # double check counts per year
  yearcount <- data.frame(Year = yearname,
                           OngoingT = nrow(ongoing.territories),
                           Illegals = nrow(illegals))
  counts[[yearname]] <- yearcount
}

dataToModel <- bind_rows(allyeardata)  
mastercountdf <- bind_rows(counts) %>%
  rowwise() %>%
  mutate(totalNoTerritories = sum(OngoingT, Illegals))
print(mastercountdf, n=Inf)

teste <- dataToModel %>% 
  #select(territory,Autumn, Spring) %>%
  arrange(territory, Autumn) %>%
  mutate(Autumn_dup = duplicated(Autumn))

view(teste)

why do i have repeated territories for these same years
for instance
Årjangkongsvinger           1999   2000          1998         2001              1
Årjangkongsvinger           1999   2000


i shoukd have 390 unique territories
sort(unique(teste$territory))

# add additional explanatory variables

modeldata <- dataToModel6 %>%
  ungroup() %>% 
  select("Fate", "SLU.ID", "territory", "Autumn", "Spring", "ruggedness_mean","pop_mean", 
         "Fi","mean_fi","max_fi",
         "hunt_county2", "number_neighbour_terr",
         "AttackDogs_n","AttackDogs_last5y","AttackDogs_cumall",
         "wolfLKill_n","wolfLKill_last5y","wolfLKill_cumall",
         "sheepAttacks_n","NoAffectedSheep","NoAffectedSheep_last5y","NoAffectedSheep_cumall",
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
              "sheepAttacks_n","NoAffectedSheep","NoAffectedSheep_last5y","NoAffectedSheep_cumall",
              "IndividualIncome",
              "bearDensity"),
            ~(scale(.) %>% as.vector))
head(modeldatascaled)

write.csv(modeldatascaled,'data/modelInputData/survivalAnalysis_testdata.csv', row.names = F)


# summarise per municipality ---------------------------------------------------
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

# variable correlation
cormat <- round(cor(df.municipality[4:20], use = "complete.obs", method = "spearman"),2)
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
cormat <- round(cor(df_municipality.pruned[4:12], use = "complete.obs", method = "spearman"),2)
cormat

# run correlation plot again

# scale data  
VarsToScale <- c("pop_mean",
              "mean_hunt_county2", 
              "mean_fi",
              #"mean_number_neighbour_terr",
              "mean_bear_density_BZtiff",
              "sum_wolfLKill_n",
              "sum_AttackDogs_n")

df.municipality_scaled <- df.municipality %>%
  mutate_at(VarsToScale,
            ~(scale(.) %>% as.vector))
head(df.municipality_scaled)

write.csv(df.municipality_scaled,'data/modelInputData/df.municipality_scaled.csv', row.names = F)

# summarise per municipality and year ---------------------------------------------------
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
            # below metrics collected at the municipality level per year so only the first element of the vector was used
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

# variable correlation
cormat <- round(cor(df.municipality_year[3:24], use = "complete.obs", method = "spearman"),2)
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
                   "wolfLKill_last5y",
                   "NoAffectedSheep_last5y",
                   "IndividualIncome")

df_municipality_year.pruned <- df.municipality_year %>%
  select(-VarsToExclude)
cormat <- round(cor(df_municipality_year.pruned[3:14], use = "complete.obs", method = "spearman"),2)
cormat

# run correlation plot again

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
head(df.municipality_year_scaled)


write.csv(df.municipality_year_scaled,'data/modelInputData/df.municipality_year_scaled.csv', row.names = F)


# model input data to compare illegal (disappearance) vs legal fate ----------------------------
modeldata <- cleandata.join1 %>%
  ungroup() %>% 
  filter(Fate %in% c("illegal","legal")) %>%
  select("Fate", "SLU.ID", "Spring", "ruggedness_mean","pop_mean", "Fi",
         "hunt_county2", "number_neighbour_terr", "dogattack", "artificial_area_total",
         "killedWolves", "killedsheep","IncAvg1998_2021","bear_density_BZtiff") %>%
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

write.csv(modeldata,'data/modelInputData/modelInputDataLegalvsIllegal_unscaled.csv', row.names = F)
write.csv(modeldatascaled,'data/modelInputData/modelInputDataLegalvsIllegal_scaled.csv', row.names = F)
