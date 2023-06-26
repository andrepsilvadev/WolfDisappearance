## Additional explanatory data ##
## Andre P. Silva ##

# to double check

# number of wolf killed 
# the dataset only has observations from 2000 to 2022. Before 2000 values I have
# coded as NA from 2000 municipalities without wolf kills are coded as zero
# I have now considered as date to match the records the autumn year.

# notes:
# most updated script for extracting explanatory variables
# scandinavian counties added
# calculation of wolves, sheep attacks and dog attacks per year per municipality

# clean data does not have clean kommune names also seems to have NA data

# bear density
 # am I using the corrected bear denity by cecilia? shall I include bear density due to
 # the correlation with human density

# average income per municipality 
 # prevasive number of NAs
# so far it seems I was using the individual income availble for Sweden but missing for nowray 
# for norway and sweden household income seem to be available but only since 2011
# give it another ago
# if I want to try household income check the following
# household income data (2011-2021, Sweden; 2005-2021 Norway) per municipality -------------------------------------------------
# Disposable income for households. Mean value, SEK thousands by
# region, type of household, age and year
# from https://www.statistikdatabasen.scb.se/pxweb/en/ssd/START__HE__HE0110__HE0110G/TabVX4bDispInkN/table/tableViewLayout1/

# attacks on sheep
think autumn or spring date
  # does rovbaseSample.csv represents all the wolf attacks on sheep?
  # confirm if the rovbasesample data is complete
  # i have only taken into account the number of events not how
  # many sheep are killed in each event - discuss with Camilla.
confirm with camilla what variable to use to count number of killed sheep
several options
14] "Antal döda under 1 år"            
[15] "Antal avlivade under 1 år"        
[16] "Antal skadade under 1 år"         
[17] "Antal saknade under 1 år"         
[18] "Antal döda 1 till 2 år"           
[19] "Antal avlivade 1 till 2 år"       
[20] "Antal skadade 1 till 2 år"        
[21] "Antal saknade 1 till 2 år"        
[22] "Antal döda över 2 år"             
[23] "Antal avlivade över 2 år"         
[24] "Antal skadade över 2 år"          
[25] "Antal saknade över 2 år"          
[26] "Antal döda körren"    
  # to do it by year I first need to separate the datasets by year, then run spatail intersection by year,
  # and then join the data from each year or otherwise fixed the kommune name in the original file
  # and do it in the by dataframe manipulation
  # a lof of NAs
  # warning in killedsheepclean
 # records were considered to be available since 1995 (first year of the record)
#therefore for lack of records after 1995 per municipality was considered zero not NA
# I have now considered as date to match the records the autumn year.
# This considers for instance that the number of killed sheep in 1998 influences the monitoring of 1998-1999  

# same as dog data - verify if norway attacks are recorded it seeems that a lot of NAs exist for Norway kommunes
# with no data for some kommunes att all. It is different for the wolf kill data because the wolf kill data
# starts in 2000 so before that it is NA while for both dog and sheep rovbase data they start (1995 and 1993) before the monitoring period
# so NAs are in fact 0s

# attacks on domestic dogs
 # double check the intersection method 
 # dog attacks seem to be from 1995. why sheep is from 1998?
 # no affected dogs is mostly one (586 cases - 1s, 2 cases - 2s). Also Norway have
 # not recorded the number of affected so it could not be calculated. I have
 # therefore decided to keep only the number of attacks for the dog variable 
 # double check if dog data includes norway, looking at data joins it does not seem so a lot of NAs for Norwegian kommunes 

# calculate average and maximum inbreeding per territory -----------------------
cleandata_withFi <- cleandata %>%
  group_by(territory) %>%
  mutate(mean_fi = mean(Fi),
         max_fi = max(Fi))

# spatial data -----------------------------------------------------------------
norway <- sf::st_read("data/spatialData/gadm36_NOR_shp/gadm36_NOR_2.shp")
sweden <- sf::st_read("data/spatialData/gadm36_SWE_shp/gadm36_SWE_2.shp")
scandinavia <- rbind(norway, sweden) %>% 
  select(NAME_1,NAME_2,geometry)

# add municipality (gadm classification) ---------------------------------------
# convert to spatial data and add gadm names
dataToModel1 <- st_as_sf(x = cleandata_withFi, #convert to spatial features (sf)
                         coords = c("X_coordinate_fatefile",
                                    "Y_coordinate_fatefile"),
                                crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>%
  st_intersection(scandinavia) %>% # add gadm names for municipalities
  st_drop_geometry() # back to data frame

# number of legally killed wolves ----------------------------------------------
killedWolvesData <- readr::read_csv("data/rawData/Rovbase230311_CW_killed wolves_final.csv") %>%
  separate(Dødsdato, c("DødsYear", "DødsMonth","DødsDay")) %>%
  mutate(DødsYear = as.numeric(DødsYear),
         DødsMonth = as.numeric(DødsMonth),
         DødsDay = as.numeric(DødsDay)) %>%
  select("RovbaseID", "DødsYear",
         "Nord (RT90)", "Øst (RT90)") %>% # columns of interest 
  filter(DødsYear > 1998) # remove records outside of the monitoring period

# convert to spatial data
killedWolvesSpatial <- st_as_sf(x = killedWolvesData,
                                coords = c("Øst (RT90)", "Nord (RT90)"),
                                crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) #%>% 
  #select(RovbaseID,geometry)

# intersect legally killed wolves with scandinavia admn areas
wolfLKill <- st_intersection(killedWolvesSpatial, scandinavia)

# count legal kills per municipality per year
wolfLKill_municipality <- wolfLKill %>% count(DødsYear, NAME_2) %>%
  rename(wolfLKill_n = n)

# build base dataframe 
kommun <- rep(levels(as.factor(dataToModel1$NAME_2)), # uses the kommunes with wolf territories
              each=length(c(2000:2022)))
# data starts in 2000 - see wolfLKill_municipality
year <- rep(c(2000:2022), length(levels(as.factor(dataToModel1$NAME_2))))
df <- data.frame(kommun = kommun,year = year)


# cumulative sum in the last five years include the target year and the previous 4
# cumulative slide was calculated based on https://stackoverflow.com/questions/72587840/how-to-get-the-cummean-of-the-last-n-rows-before-the-last-row-using-dplyr
wolfLKillAllMetrics <- left_join(df,
                                 wolfLKill_municipality,
                  by=c('kommun'='NAME_2', 'year'='DødsYear')) %>%
  group_by(kommun) %>%
  # replaces NAs by zeros. This considers that records of wolf kilss were recorded since 2000 (first year with records)
  mutate_at(c('wolfLKill_n'), ~replace_na(.,0)) %>%
  mutate(wolfLKill_last5y = slider::slide_dbl(wolfLKill_n, sum, .before = 4, .after = 0),
         .after=year) %>%
  mutate(wolfLKill_cumall = cumsum(wolfLKill_n),.after=year)

# join to data to model  
dataToModel2 <- left_join(dataToModel1,
                          wolfLKillAllMetrics,
                          by=c('NAME_2'='kommun', 'Autumn'='year')) %>%
  # replaces zeros to NAs before 1999 because there was no wolf kill data before 2000
  mutate(wolfLKill_n = ifelse(Autumn <= 1999, NA, wolfLKill_n)) 

# tests
# table(is.na(dataToModel2$wolfLKill_n))

# wolf attacks on sheep --------------------------------------------------------
rovbasedata <- readr::read_csv("data/rawData/rovbaseSample.csv") %>%
  # sums killed, injured, or missing individuals across the different time spans measured
  mutate(NoAffectedSheep = rowSums(across("Antal döda under 1 år":"Antal saknade över 2 år"), na.rm = F))
  
# select attack id, date and coordinates (check if coordinate system is the same as in scandinavia)
killedsheepclean <- rovbasedata %>% select("HändelseID", "Skada på", "Fynddatum",
                                           "NoAffectedSheep", "Kommune",
                                           "Nord (RT90)", "Öst (RT90)") %>%
  na.omit() %>%
  filter(`Skada på` %in% c("Får")) %>%
  separate(Fynddatum, c("year","month","day)")) %>% 
  mutate(year = as.numeric(year)) #%>%
  #filter(year>=1998) 

# read as spatial points and change coordinate to RT90
killedsheepSpatial <- st_as_sf(x = killedsheepclean,
                               coords = c("Öst (RT90)", "Nord (RT90)"),
                               crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>% 
  select(HändelseID, year, "NoAffectedSheep", geometry) 

# intersect sheep attacks with scandinavia admn areas
sheepAttacks <- st_intersection(killedsheepSpatial, scandinavia)

# count number of sheep killed  and number of attacks per municipality
sheepAffected_municipality <- sheepAttacks %>%
  group_by(year, NAME_2) %>%
  summarise(NoAffectedSheep = sum(NoAffectedSheep),
            n = n(),.groups = "drop") %>%
  rename(sheepAttacks_n = n)

# build base dataframe 
kommun <- rep(levels(as.factor(dataToModel2$NAME_2)), # uses the kommunes with wolf territories
              each=length(c(1995:2021)))
# data starts in 1995 - see sheepAffected_municipality
year <- rep(c(1995:2021), length(levels(as.factor(dataToModel2$NAME_2))))
df <- data.frame(kommun = kommun,year = year)

# cumulative metrics
# cumulative sum in the last five years include the target year and the previous 4
# cumulative slide was calculated based on https://stackoverflow.com/questions/72587840/how-to-get-the-cummean-of-the-last-n-rows-before-the-last-row-using-dplyr
sheepAllMetrics <- left_join(df,
                             sheepAffected_municipality,
                  by=c('kommun'='NAME_2', 'year'='year')) %>%
  dplyr::group_by(kommun) %>%
  # replaces NAs by zeros. This considers that records of sheep events were recorded since 1995 (first year with records)
  mutate_at(c('NoAffectedSheep','sheepAttacks_n'), ~replace_na(.,0)) %>%
  mutate(NoAffectedSheep_last5y = slider::slide_dbl(NoAffectedSheep, sum, .before = 4, .after = 0),
         .after=year) %>%
  mutate(NoAffectedSheep_cumall = cumsum(NoAffectedSheep),.after=year)
 
# join to data to model  
dataToModel3 <- left_join(dataToModel2,
                      sheepAllMetrics,
                      by=c('NAME_2'='kommun', 'Autumn'='year'))

# tests
# table(is.na(dataToModel3$NoAffectedSheep_cumall))
# cor(dataToModel3$NoAffectedSheep, dataToModel3$NoAffectedSheep_last5y, use="complete.obs")
# cor(dataToModel3$NoAffectedSheep, dataToModel3$NoAffectedSheep_cumall, use="complete.obs")
# cor(dataToModel3$NoAffectedSheep_last5y, dataToModel3$NoAffectedSheep_cumall, use="complete.obs")

# wolf attacks on domestic dogs ------------------------------------------------
dogData <- readr::read_csv("data/rawData/Wolf attacks on dogs Sweden and Norway_Rovbase 230131_230210_CW.csv") %>%
  # sums killed, injured, or missing individuals across the different time spans measured
  mutate(NoAffectedDogs = rowSums(across("Antal döda under 1 år":"Antal saknade över 2 år"), na.rm = F))

# select attack id, date and coordinates (check if coordinate system is the same as in scandinavia)
AttacksDogclean <- dogData %>% select("HändelseID", "Country", "Fynddatum", "NoAffectedDogs", "Kommune",
                                   "Nord (RT90)", "Öst (RT90)") %>%
  na.omit() %>%
  separate(Fynddatum, c("year","month","day)")) %>%
  filter(year >= 1995) %>% # original span is 1995-2022
  mutate(year = as.numeric(year))

# read as spatial points and change coordinate to RT90
AttacksDogSpatial <- st_as_sf(x = AttacksDogclean,
                           coords = c("Öst (RT90)", "Nord (RT90)"),
                           crs = "EPSG:3021") %>% #RT90
  st_transform(crs = st_crs(scandinavia)) %>% 
  select(HändelseID, NoAffectedDogs, year, geometry)

# intersect attacks on dogs with scandinavia admn areas
AttackDogs <- st_intersection(AttacksDogSpatial, scandinavia)

# count number of attacks on dogs per year and per municipality
AttackDogs_municipality <- AttackDogs %>%
  group_by(year, NAME_2) %>%
  summarise(n = n(),.groups = "drop") %>%
  rename(AttackDogs_n = n)

# build base dataframe 
kommun <- rep(levels(as.factor(dataToModel3$NAME_2)),
              each=length(c(1995:2021)))
# data starts in 1995 - see AttackDogs_municipality
year <- rep(c(1995:2021), length(levels(as.factor(dataToModel3$NAME_2))))
df <- data.frame(kommun = kommun,year = year)

# cumulative metrics
# cumulative sum in the last five years include the target year and the previous 4
# cumulative slide was calculated based on https://stackoverflow.com/questions/72587840/how-to-get-the-cummean-of-the-last-n-rows-before-the-last-row-using-dplyr
dogAllMetrics <- left_join(df,
                           AttackDogs_municipality,
                           by=c('kommun'='NAME_2', 'year'='year')) %>%
  dplyr::group_by(kommun) %>%
  # replaces NAs by zeros. This considers that records of sheep events were recorded since 1995
  # (first year with records)
  mutate_at(c('AttackDogs_n'), ~replace_na(.,0)) %>%
  mutate(AttackDogs_last5y = slider::slide_dbl(AttackDogs_n, sum, .before = 4, .after = 0),
         .after=year) %>%
  mutate(AttackDogs_cumall = cumsum(AttackDogs_n),.after=year)

# join to data to model  
dataToModel4 <- left_join(dataToModel3,
                          dogAllMetrics,
                          by=c('NAME_2'='kommun', 'Autumn'='year'))
# tests
# table(is.na(dataToModel4$AttackDogs_n))
# cor(dataToModel4$AttackDogs_n, dataToModel4$AttackDogs_last5y, use="complete.obs")
# cor(dataToModel4$AttackDogs_n, dataToModel4$AttackDogs_cumall, use="complete.obs")
# cor(dataToModel4$AttackDogs_last5y, dataToModel4$AttackDogs_cumall, use="complete.obs")

# individual income data (1991-2021, only Sweden) per municipality -------------

# individual income was extracted for Sweden and Norway separately 
# is my understanding that both refer to gross income. Norway data is NOK, Sweden data is in SEK - I have ignored the difference 
# the links used for extraction are
# Sweden https://www.scb.se/en/finding-statistics/statistics-by-subject-area/household-finances/income-and-income-distribution/income-and-tax-statistics/
# Norway https://www.ssb.no/en/statbank/table/05854/

individualIncomeDataSweden <- readr::read_csv("data/rawData/avg_income_municipalities_1991_2021.csv",
                              locale = locale(encoding = "ISO-8859-1")) %>%
  tidyr::gather(key="Year", value ="IndividualIncome", -Kommun) %>%
  separate_wider_delim(Year, "_", names = c("x", "Year")) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Kommun = str_replace(Kommun, ' LÄN', '')) %>%
  mutate(Kommun = str_to_title(Kommun)) %>% # turns first letter of each word in caps
  mutate(Kommun = str_replace(Kommun, "Upplands Väsby", "Upplands-Väsby")) %>%
  mutate(Kommun = str_replace(Kommun, "Heby2", "Heby")) %>%
  mutate(Kommun = str_replace(Kommun, " ", "")) %>% # remove possible empty spaces %>%
  select(-x) %>%
  # replace zeros from kommunes with no data in early years
  mutate(IndividualIncome=ifelse(IndividualIncome==0,NA,IndividualIncome))


# data from Norway
# add to manually remove contact details and comments from the
# bottom of the original excel file
# found the unique kommunes in the monitoring dataset for Norway
# selected manually in the norway income datafile
# data for Romskog fo2020 and 2021 seems to be missing
# valer there were two. Had to manually go to the wolf dataset and see where the geographic location matched.
# It is identified in the income dataset as Våler while the other Våler found in the income
# dataset I have identified as Våler2

individualIncomeDataNorway <- readr::read_csv("data/rawData/Norway_averageIndividualGrossIncome1991_2021_municipalities1965_manuallyEdited.csv") %>%
  group_by(kommun) %>%
  select(-c("kommun original","age","metric")) %>%
  summarise(across(everything(), sum)) %>%
  mutate(across(where(is.numeric), ~na_if(., 0))) %>% # replaces NAs across all dataframe
  pivot_longer(cols=c(2:24),
               names_to='Year',
               values_to='IndividualIncome') %>%
  rename(Kommun = kommun) %>%
  mutate(Year = as.numeric(Year))

individualIncomeData <- bind_rows(individualIncomeDataSweden,individualIncomeDataNorway) %>%
  # difference to year average to remove correlation with year (income increases over year)
  group_by(Year) %>%
  mutate(year_average = mean(IndividualIncome,na.rm = TRUE),
         income_prop = IndividualIncome/year_average)

# head(individualIncomeData)
# view(individualIncomeData)

# the following observations seem to have the wrong country attributed based on the kommune assigned. The kommune 
# was double-checked with the overlap with gadmn kommunes
kommunesWithWrongCountry <- c("Aurskog-Høland","Enebakk","Aremark","Dals-Ed",
                              "Halden","Rømskog","Åsnes","Eidskog","Grue",
                              "Kongsvinger","Trysil","Våler")  

dataToModel4 <-  dataToModel4 %>% 
  mutate(Country_fatefile = ifelse(NAME_2 %in% kommunesWithWrongCountry, str_replace(Country_fatefile, "S", "N"), Country_fatefile))

dataToModel5 <- left_join(dataToModel4,
                          individualIncomeData,
                          by=c('NAME_2'='Kommun','Autumn'='Year'))

# add years since first detection for test with survival analyses --------------
dataToModel6 <- dataToModel5 %>% 
  group_by(territory) %>%
  mutate(first_yeardet = min(Autumn),
         last_yeardet = max(Autumn),
         years_firstdet = Autumn-first_yeardet,
         time_start = Autumn-first_yeardet,
         time_end = Spring-first_yeardet) 

# prepare for correlation analyses ---------------------------------------------
dataToModel7 <- dataToModel6 %>%
  relocate(c(NAME_1,NAME_2), .after=Comment) %>%
  select(-c(geometry.x,geometry.y)) %>%
  relocate(geometry, .after=Comment) %>%
  relocate(income_prop, .after=IndividualIncome)
  #mutate_at(c("wolfLKill_cumall", "wolfLKill_n", "sheepAttacks_n",
  #            "AttackDogs_cumall", "AttackDogs_n"),
  #          ~as.numeric(.))
  




