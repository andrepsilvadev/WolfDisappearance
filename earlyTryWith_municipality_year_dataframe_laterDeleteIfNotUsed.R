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