library (dplyr)
library(reshape2)

IN_Data <- filter(X2016_precinct_house,X2016_precinct_house$state_postal=="IN")

levels(as.factor(IN_Data$party))
levels(as.factor(IN_Data$precinct))
levels(as.factor(IN_Data$district))
levels(as.factor(IN_Data$jurisdiction))

nlevels(as.factor(IN_Data$precinct))/nlevels(as.factor(IN_Data$district))

IN_Mini=select(IN_Data, precinct, party, votes)

IN_Data_Rep=IN_Mini %>% filter(party=="republican") 
IN_Data_Rep=IN_Data_Rep %>% rename(votes_rep = votes )
  
IN_Data_Dem=IN_Mini %>% filter(party=="democratic")
IN_Data_Dem=IN_Data_Dem %>% rename(votes_dem = votes )

IN_Data_Lib=IN_Mini %>% filter(party=="libertarian")
IN_Data_Lib=IN_Data_Lib %>% rename(votes_lib = votes )

IN_Data_Precinct_Master=IN_Data %>% select(precinct) %>%
  distinct(precinct)

IN_Data_Dem_Sum=IN_Data_Dem %>%
  group_by(precinct) %>%
  summarise(votes_dem_sum = sum(votes_dem))

IN_Data_Rep_Sum=IN_Data_Rep %>%
  group_by(precinct) %>%
  summarise(votes_rep_sum = sum(votes_rep))

IN_Data_Lib_Sum=IN_Data_Lib %>%
  group_by(precinct) %>%
  summarise(votes_lib_sum = sum(votes_lib))

IN_Data_Col=IN_Data %>% select(year, stage, special, state, state_postal, state_fips, state_icpsr, 
    county_name, county_fips, county_ansi, county_lat, county_long, jurisdiction, district, precinct) %>%
    distinct() 
    
    
IN_Data_RD1=left_join(IN_Data_Col,IN_Data_Rep_Sum,by = c("precinct"))
IN_Data_RD2=left_join(IN_Data_RD1,IN_Data_Dem_Sum,by = c("precinct"))

dim(IN_Data_Col)
dim(IN_Data_Precinct_Master)
dim(IN_Data_RD1)
dim(IN_Data_RD2)
dim(IN_Data_Rep_Sum)


IN_Data_Final_Fix=IN_Data_RD2 %>%
      filter(precinct!="KOSCIUSKO") %>%
      filter(!is.na(district)) %>%
      replace(is.na(.), 0) 

dim(IN_Data_RD2)-dim(IN_Data_Final_Fix)

boxplot(IN_Data_Final_Fix$votes_dem_sum-IN_Data_Final_Fix$votes_rep_sum~IN_Data_Final_Fix$district)
abline(h=0)

boxplot(IN_Data_Final_Fix$votes_dem_sum-IN_Data_Final_Fix$votes_rep_sum~IN_Data_Final_Fix$jurisdiction)
abline(h=0)

boxplot(IN_Data_Final_Fix$votes_dem_sum-IN_Data_Final_Fix$votes_rep_sum~IN_Data_Final_Fix$precinct)
abline(h=0)

IN_Data_Final_Fix %>%
    group_by(district) %>%
    summarise(votes_tot = sum(votes_dem_sum-votes_rep_sum))





