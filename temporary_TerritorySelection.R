years <- seq(2015,2016,1)

for (i in 1:length(years)) {
ongoing.territories <- cleandata %>%
  ungroup() %>%
  filter(Fate %in% c(NA)) %>% # meaning fate in 2020 (last year monitoring) was NA (alive)
  filter(Spring == years[i]) %>% # keep location only for previous year before last year monitoring to be sure we are working with pairs that are alive
  group_by(territory) %>%
  slice_max(Fi, with_ties=FALSE) %>% # keeps the row with the maximum inbreeding
  arrange(territory)
  }

ongoing.territories
nrow(ongoing.territories)
view(ongoing.territories)

for (i in 1:length(years)) {
  ongoing.territories2 <- cleandata %>%
    ungroup() %>%
    filter(Spring == years[i]) %>% 
    group_by(territory) %>%
    
    
    filter(Fate %in% c(NA)) %>% # meaning fate in 2020 (last year monitoring) was NA (alive)
    group_by(territory) %>%
    slice_max(Fi, with_ties=FALSE) %>% # keeps the row with the maximum inbreeding
    arrange(territory)
}


