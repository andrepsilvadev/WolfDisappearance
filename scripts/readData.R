## Read Wolf Territory Dataset ##
## Andre P. Silva ##

data <- readr::read_csv("data/rawData/rawData.csv")
idvars <- c(colnames(data)[1:13], "KommunerNamn", "SLU-ID")
explanatoryvars <- c("ruggedness_mean", "average_gravel_km", "average_paved_km",
                     "pop_mean", "mean_snow", "hunt_afo", "hunt_county",
                     "Fi", "number_neighbour_terr")
varstoselect <- c(idvars, explanatoryvars)

cleandata <- data %>%
  select(all_of(varstoselect)) %>%
  mutate(mod_id = 1:nrow(data)) %>% #
  relocate(mod_id, .before = id) %>% 
  mutate(Fate = str_replace(Fate, "Illegal", "illegal")) %>%
  # keep only the latest record for each individual/territory
  group_by(territory) %>%
  arrange(desc(Spring)) %>% # distinct keeps the first row
  distinct(territory, .keep_all=TRUE)

# check only one row per individual
#unique(dataClean$id)
# check missing values
#sapply(dataClean, function(y) sum(length(which(is.na(y)))))











  

