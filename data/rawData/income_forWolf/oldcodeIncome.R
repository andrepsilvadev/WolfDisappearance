# old code (before 20250301) from below - delete at the end

# individual income data (1991-2021, only Sweden) per municipality

# individual income was extracted for Sweden and Norway separately 
# is my understanding that both refer to gross income. Norway data is NOK, Sweden data is in SEK - I have ignored the difference 
# the links used for extraction are
# Sweden https://www.scb.se/en/finding-statistics/statistics-by-subject-area/household-finances/income-and-income-distribution/income-and-tax-statistics/
# Norway https://www.ssb.no/en/statbank/table/05854/

individualIncomeDataSweden <- readr::read_csv("rawData/avg_income_municipalities_1991_2021.csv",
                              locale = locale(encoding = "ISO-8859-1")) %>%
  tidyr::gather(key="Year", value ="IndividualIncome", -Kommun) %>%
  separate_wider_delim(Year, "_", names = c("x", "Year")) %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  dplyr::mutate(Kommun = str_replace(Kommun, ' LÄN', '')) %>%
  dplyr::mutate(Kommun = str_to_title(Kommun)) %>% # turns first letter of each word in caps
  dplyr::mutate(Kommun = str_replace(Kommun, "Upplands Väsby", "Upplands-Väsby")) %>%
  dplyr::mutate(Kommun = str_replace(Kommun, "Heby2", "Heby")) %>%
  dplyr::mutate(Kommun = str_replace(Kommun, " ", "")) %>% # remove possible empty spaces %>%
  select(-x) %>%
  # replace zeros from kommunes with no data in early years
  dplyr::mutate(IndividualIncome=ifelse(IndividualIncome==0,NA,IndividualIncome))


# data from Norway
# add to manually remove contact details and comments from the
# bottom of the original excel file
# found the unique kommunes in the monitoring dataset for Norway
# selected manually in the norway income datafile
# data for Romskog fo2020 and 2021 seems to be missing
# valer there were two. Had to manually go to the wolf dataset and see where the geographic location matched.
# It is identified in the income dataset as Våler while the other Våler found in the income
# dataset I have identified as Våler2

individualIncomeDataNorway <- readr::read_csv("rawData/Norway_averageIndividualGrossIncome1991_2021_municipalities1965_manuallyEdited.csv") %>%
  group_by(kommun) %>%
  select(-c("kommun original","age","metric")) %>%
  dplyr::summarise(across(everything(), sum)) %>%
  dplyr::mutate(across(where(is.numeric), ~na_if(., 0))) %>% # replaces NAs across all dataframe
  pivot_longer(cols=c(2:24),
               names_to='Year',
               values_to='IndividualIncome') %>%
  dplyr::rename(Kommun = kommun) %>%
  dplyr::mutate(Year = as.numeric(Year))

individualIncomeData <- bind_rows(individualIncomeDataSweden,individualIncomeDataNorway) %>%
  # difference to year average to remove correlation with year (income increases over year)
  group_by(Year) %>%
  dplyr::mutate(year_average = mean(IndividualIncome,na.rm = TRUE),
         income_prop = IndividualIncome/year_average)

# head(individualIncomeData)
# view(individualIncomeData)

# the following observations seem to have the wrong country attributed based on the kommune assigned. The kommune 
# was double-checked with the overlap with gadmn kommunes
kommunesWithWrongCountry <- c("Aurskog-Høland","Enebakk","Aremark","Dals-Ed",
                              "Halden","Rømskog","Åsnes","Eidskog","Grue",
                              "Kongsvinger","Trysil","Våler")  
