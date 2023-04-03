## Read Wolf Territory Dataset ##
## Andre P. Silva ##

data <- readr::read_csv("data/rawData/rawData.csv")
#data <- readr::read_csv("data/rawData/wolfFate_corrected_OLI_20230206.csv")
idvars <- c(colnames(data)[1:14], "KommunerNamn", "SLU-ID")
explanatoryvars <- c("ruggedness_mean", "average_gravel_km", "average_paved_km",
                     "pop_mean", "mean_snow", "hunt_afo", "hunt_county",
                     "Fi", "number_neighbour_terr")

#idvars <- c(colnames(data)[1:14], "KommunerNamn")
#explanatoryvars <- c("ruggedness_mean", "average_gravel_km", "average_paved_km",
#                     "pop_mean", "mean_snow", "hunt_afo", "hunt_county",
#                     "number_neighbour_terr")


varstoselect <- c(idvars, explanatoryvars)

cleandata <- data %>%
  select(all_of(varstoselect)) %>%
  mutate(mod_id = 1:nrow(data)) %>% 
  relocate(mod_id, .before = id) %>% 
  mutate(Fate = str_replace(Fate, "Legal", "legal")) %>%
  mutate(Fate = str_replace(Fate, "Illegal", "illegal")) %>%
  mutate(Fate = str_replace(Fate, "censor", "censored")) %>%
  mutate(Fate = str_replace(Fate, "cernsored", "censored")) %>%
  mutate(Fate = str_replace(Fate, "censoreded", "censored")) %>%
  # keep only the latest record for each individual/territory
  group_by(territory) %>%
  arrange(desc(Spring)) %>% # distinct keeps the first row
  distinct(territory, .keep_all=TRUE)

# check only one row per individual
#unique(cleandata$id)
# check missing values
#sapply(dataClean, function(y) sum(length(which(is.na(y)))))




many illegals
"Most of the fates “illegal” are, as you suspected,
animals that we lost contact with and have reasons to believe the cause
is poaching (we describe criteria for that judgement in our 2012 and 2020
poaching papers" replace to unknown?
traffic category - dissapeared
still many NAs
what is the explanation to NA
corrected fate -> fate
slu-id and fid disappeared in corrected file
length(unique(cleandata$territory))
[1] 436
> length(unique(cleandata$territory))
[1] 461
25 new territories?




  

