## Read Wolf Territory Dataset ##
## Andre P. Silva ##

data <- readr::read_csv("data/rawData/FateWolvesTerritory1998_2020_Scandinavia_Final.csv")
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
  mutate(KommunerNamn = tolower(KommunerNamn)) %>% # to minimize mismatches due to case sensitivity
  # keep only the latest record for each individual/territory
  group_by(territory) %>%
  arrange(desc(Spring)) %>% # distinct keeps the first row
  distinct(territory, .keep_all=TRUE)

# check only one row per individual
#unique(cleandata$id)
# check missing values
#sapply(dataClean, function(y) sum(length(which(is.na(y)))))




  

