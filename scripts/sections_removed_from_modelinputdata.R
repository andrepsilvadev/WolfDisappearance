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
I think the previous is equal to this


# summarise per territory illegal (disappearance) vs ongoing territories -------

# keep location only for previous year before last year monitoring to be sure
# we are working with pairs that are alive
years <- seq(1999,2020,1) 

allyeardata <- list()
counts <- list()
for (i in 1:length(years)) {
  ongoing.territories <- cleandata %>%
    ungroup() %>%
    # meaning fate in 2020 (last year monitoring) was NA (alive)
    filter(Fate %in% c(NA)) %>% 
    # keep location only for previous year before last year monitoring to be sure
    # we are working with pairs that are alive
    filter(Spring == years[i]) %>% 
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
