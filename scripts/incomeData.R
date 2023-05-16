incomeData <- readr::read_csv("data/rawData/avg_income_municipalities_1991_2021.csv", locale = locale(encoding = "ISO-8859-1")) %>%
  rowwise() %>%
  mutate(IncAvg1998_2021 = mean(c_across(c('year_1998': 'year_2021')), na.rm=TRUE)) %>%
  mutate(Kommun = tolower(Kommun)) %>% #to minimize mismatches due to case sensitivity
  select("Kommun","IncAvg1998_2021") %>%
  rename(KommunerNamn = Kommun)

#incomeData

#cleanDataWithIncome <- left_join(cleandata, incomeData, by="KommunerNamn")

colSums(is.na(cleanDataWithIncome))



var1 <- incomeData %>% ungroup() %>% select(KommunerNamn)
var2 <- cleandata %>% ungroup() %>% select(KommunerNamn) 
compare <- full_join(var1,var2)