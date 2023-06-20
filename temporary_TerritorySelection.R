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
    filter(Spring == years[i]) %>% 
    group_by(territory) %>%
    
    
    filter(Fate %in% c(NA)) %>% # meaning fate in 2020 (last year monitoring) was NA (alive)
    group_by(territory) %>%
    slice_max(Fi, with_ties=FALSE) %>% # keeps the row with the maximum inbreeding
    arrange(territory)
}

teste2 <- cleandata %>%
  group_by(Autumn,territory) %>%
  # filter(!any(Fate %in% c("other","legal","natural","traffic"))) %>%
  # keep the row with the maximum inbreeding, if tied keep the first row
  #slice_max(Fi, with_ties=FALSE) %>% 
  # add column to double-check potential duplicate rows within territories 
  #mutate(Autumn_dup = duplicated(Autumn)) %>%
  arrange(territory)

sort(unique(teste2$territory))
length(unique(teste2$territory)) #435
table(unique(teste2$Autumn_dup))
unique(teste2$Fate)
view(teste2)

teste3 <- cleandata %>%
  group_by(territory) %>%
  #filter(!any(Fate=="other" | Fate=="legal" | Fate=="natural" | Fate=="traffic")) %>%
  #filter(!any(Fate = "other|legal|natural|traffic")) %>%
  #filter(any(Fate != "other|legal|natural|traffic")) %>% # I get 388 
  #filter(any(!Fate %in% c("other","legal","natural","traffic"))) %>% # I get 398, it is tking only the rows that do not match the condition
  filter(!any(Fate %in% c("other","legal","natural","traffic"))) %>% # i get 291
  # keep the row with the maximum inbreeding, if tied keep the first row
  #slice_max(Fi, with_ties=FALSE) %>% 
  # add column to double-check potential duplicate rows within territories 
  #mutate(Autumn_dup = duplicated(Autumn)) %>%
  arrange(territory, Autumn)

double check this approach with several territories I thinkt it might be doing the trick

view(teste3)
length(unique(teste3$territory))
