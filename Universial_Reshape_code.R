library (dplyr)
library(tidyverse)


###Importing Vote data from MIT Election Data and Science Lab

IN_Data1 <- filter(X2016_precinct_house,X2016_precinct_house$state_postal=="KY") %>%
  unite("xyz", jurisdiction:precinct,remove = FALSE)
  
IN_Data=transform(IN_Data1,ID=paste0(xyz,district))

IN_Mini=select(IN_Data, ID, party, votes)

IN_Data_Rep=IN_Mini %>% filter(party=="republican") %>% 
  rename(votes_rep = votes )

IN_Data_Dem=IN_Mini %>% filter(party=="democratic") %>% 
  rename(votes_dem = votes )

IN_Data_Lib=IN_Mini %>% filter(party=="libertarian") %>% 
  rename(votes_lib = votes )


IN_Data_Dem_Sum=IN_Data_Dem %>%
  group_by(ID) %>%
  summarise(votes_dem_sum = sum(votes_dem))

IN_Data_Rep_Sum=IN_Data_Rep %>%
  group_by(ID) %>%
  summarise(votes_rep_sum = sum(votes_rep))

IN_Data_Lib_Sum=IN_Data_Lib %>%
  group_by(ID) %>%
  summarise(votes_lib_sum = sum(votes_lib))

IN_Data_Col=IN_Data %>% select(year, stage, special, state, state_postal, state_fips, state_icpsr, 
                               county_name, county_fips, county_ansi, county_lat, county_long, 
                               jurisdiction, district, precinct, ID) %>%
  distinct() 


IN_Data_RD1=left_join(IN_Data_Col,IN_Data_Rep_Sum,by = c("ID"))
IN_Data_RD2=left_join(IN_Data_RD1,IN_Data_Dem_Sum,by = c("ID"))


IN_Data_Final_Fix=IN_Data_RD2 %>%
  filter(precinct!="KOSCIUSKO") %>%
  filter(!is.na(district)) %>%
  replace(is.na(.), 0) %>%
  filter(votes_dem_sum >1 | votes_rep_sum >1 )

boxplot(IN_Data_Final_Fix$votes_dem_sum-IN_Data_Final_Fix$votes_rep_sum~IN_Data_Final_Fix$district)
abline(h=0)

boxplot(IN_Data_Final_Fix$votes_dem_sum-IN_Data_Final_Fix$votes_rep_sum~IN_Data_Final_Fix$jurisdiction)
abline(h=0)

#boxplot(IN_Data_Final_Fix$votes_dem_sum-IN_Data_Final_Fix$votes_rep_sum~IN_Data_Final_Fix$precinct)
#abline(h=0)

House_Seats=IN_Data_Final_Fix %>%
  group_by(district) %>%
  summarise(votes_tot = sum(votes_dem_sum-votes_rep_sum))

IN_Permute_Temp=select(IN_Data_Final_Fix , ID, votes_rep_sum, votes_dem_sum)

IN_Perm_trial=sample_frac(IN_Permute_Temp,n=1,replace = FALSE)

N_rep <- 1000
TDist <- 1

for (i in 1:N_rep) {
  IN_Perm_trial=sample_frac(IN_Permute_Temp,n=1,replace = FALSE)
  TempTemp=bind_cols(IN_Data_Final_Fix,IN_Perm_trial)
  Temp_nums=TempTemp %>%
    group_by(district) %>%
    summarise(votes_tot = sum(votes_dem_sum...21-votes_rep_sum...20))
  succ=Temp_nums$votes_tot
  TDist[i]= sum(succ > 0)
}

TDist

hist(TDist)

IN_Data_Final_Fix %>%
  group_by(district) %>%
  summarise(votes_tot = sum(votes_dem_sum-votes_rep_sum))

sum(House_Seats$votes_tot > 0)
median(TDist)




TempTemp=bind_cols(IN_Data_Final_Fix,IN_Perm_trial)
TempTemp %>%
  group_by(district) %>%
  summarise(votes_tot = sum(votes_dem_sum...21-votes_rep_sum...20))

IN_Perm_trial=sample_frac(IN_Permute_Temp,n=1,replace = FALSE)
TempTemp=bind_cols(IN_Data_Final_Fix,IN_Perm_trial)
TempTemp %>%
  group_by(jurisdiction) %>%
  summarise(votes_tot = sum(votes_dem_sum...21-votes_rep_sum...20))


