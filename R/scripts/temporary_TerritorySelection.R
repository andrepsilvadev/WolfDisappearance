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

unique(teste3$Fate)

df <- teste3 %>% 
  group_by(Spring) %>%
  summarise(
    ongoing = sum(is.na(Fate)==TRUE),
    disappearance = sum(Fate %in% c("illegal" ,"censored")),
    sum = ongoing + disappearance,
    prop_disappearance = round(disappearance/sum, 2),
    n()
    )

print(df, n=30)

# plot disappearance rate - it does not match the plot from Liberg et al. 2020
# I cannot calculate prop. disappearance and total number of territories
# like this because I am excluding territories with other fates (it influences the sum)
# what are the implications for the territory analyses?
# the plot
ggplot(data=df, aes(x=Spring, y=prop_disappearance, group=1)) +
  geom_line()+
  geom_point()


  

go ahead with building the dataframe for the survival analyses with this dataset
so that giorgia can proceed?

how to calculate 

does it make a differnce? to model the proportion per municipality

view(teste3)
length(unique(teste3$territory))
