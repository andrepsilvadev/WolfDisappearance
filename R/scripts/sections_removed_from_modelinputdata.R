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

# section removed from Additional explanatory data ------------------------------

#####################################
# variable distribution and correlations ---------------------------------------
# run histogram p6 and correlation with other variables (descriptive stats.R)
explanatoryvars <- c("territory","ruggedness_mean", "average_gravel_km", "average_paved_km",
                     "pop_mean", "mean_snow", "hunt_afo", "hunt_county2",
                     "bear_density_BZtiff", "artificial_area_total",
                     "Fi","mean_fi","max_fi",
                     "number_neighbour_terr",
                     "AttackDogs_n", "AttackDogs_last5y","AttackDogs_cumall",
                     "wolfLKill_n", "wolfLKill_last5y" , "wolfLKill_cumall",
                     "sheepAttacks_n","NoAffectedSheep","NoAffectedSheep_last5y", "NoAffectedSheep_cumall", 
                     "IndividualIncome")

cordata <- dataToModel6 %>% select(all_of(explanatoryvars))

cordatalong <- cordata %>% 
  pivot_longer(
    cols = !territory,
    names_to = "variable", 
    values_to = "value"
  )
head(cordatalong)

# variable distribution
p1 <- ggplot(data = cordatalong,
             aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = "free_x") +
  ylab("Count") +
  ggtitle(label = "Territory features") + 
  theme_minimal() +
  theme(text = element_text(size = 20))
p1

# variable correlation
cormat <- round(cor(cordata[2:24], use = "complete.obs", method = "spearman"),2)
cormat

# Get upper triangle of the correlation matrix
# from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
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






# plot explanatory data

# number of legally killed wolves ----------------------------------------------
p3 <- ggplot() +
  geom_sf(data = scandinavia) +
  geom_sf(data = killedWolvesSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Legally killed wolves (2000-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p3

# number of wolf attacks on sheep ----------------------------------------------
p4 <- ggplot() +
  geom_sf(data = scandinavia) +
  geom_sf(data = killedsheepSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Sheep attacks by wolves (1998-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p4

# number of wolf attacks on domestic dogs --------------------------------------
p5 <- ggplot() +
  geom_sf(data = scandinavia) +
  geom_sf(data = dogdataSpatial, size = 0.7) +
  coord_sf(xlim = c(3, 23), ylim = c(57, 64), expand = FALSE) +
  #facet_wrap(~Fate) +
  viridis::scale_fill_viridis(discrete=TRUE) +
  ggtitle(label = "Wolf attacks on domestic dogs (1997-2022)") + 
  geom_text(size = 30) +
  theme_minimal()
p5

# section removed from model input data ---------------------------------
conflict between scripts done in flight to Lisbon

<<<<<<< HEAD
why is the coorrelation betwen Fi and max fi =1?
  cor(dataToModel$Fi,dataToModel$max_fi, use="complete.obs")

# model input data to compare illegal (disappearance) vs ongoing territories ---

#inbreeding needs to be changed for average inbreeding

years <- seq(1999,2020,1) # keep location only for previous year before last 
# year monitoring to be sure we are working with pairs that are alive

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
=======
  upper_tri <- get_upper_tri(cormat)
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
>>>>>>> 483bbcd0c60e86f23d929c713a0171b775421127








