rm(list=ls(all=TRUE))
setwd("PASTE WORKING DIRECTORY HERE")
library(tidycensus)
library(tidyverse)
library(rio)

#census_api_key("aed7bfa15ecfb5bdac5fc798f4bd0aa63d56bab4", install = TRUE)
#acs18 <- load_variables(2018, "acs5", cache = TRUE)
#data(fips_codes)
#years <- lst(2014, 2015, 2016, 2017, 2018)

counties <- c("dallas", "rockwall", "collin county", "denton", "tarrant", "kaufman", "ellis")

#CRI ACS Health Variables no transformation
hel_variables <- c(
  hel_totalpop = "S2701_C01_001", 
  hel_popunins = "S2701_C04_001",
  hel_popins = "S2701_C02_001",
  hel_pripct = "S2703_C03_001",
  hel_pubpct = "S2704_C03_001",
  hel_poppri = "S2703_C01_001",
  hel_poppub = "S2704_C01_001" 
  )

#City of Dallas 5 Year ACS Family Variable
hel_cityofdall <- get_acs(geography = "place", variables = hel_variables,
                           state = "TX", year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 4819000) %>%
  mutate(
    GEOTYPE = "PLACE")

#Dallas County 5 Year ACS Family Variables
hel_dallcounty <- get_acs(geography = "tract", variables = hel_variables,
                           state = "TX", county = "dallas", year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")


#North Texas Counties 5 Year ACS Family Variables
hel_ntxcountiesTs <- get_acs(geography = "tract", variables = hel_variables,
                             state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

hel_ntxcountiesCs <- get_acs(geography = "county", variables = hel_variables,
                             state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "COUNTY")

hel_ntxcounties <- full_join(hel_ntxcountiesTs, hel_ntxcountiesCs)

rm(hel_ntxcountiesCs)
rm(hel_ntxcountiesTs)

#Dallas/Ft. Worth MSA 5 Year ACS Family Variables
hel_dfw <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", variables = hel_variables,
                    year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

#merge datasets
hel_geo <- full_join(full_join(full_join(hel_cityofdall, hel_dallcounty), hel_dfw), hel_ntxcounties)
colnames(hel_geo)

rm(hel_cityofdall)
rm(hel_dallcounty)
rm(hel_dfw)
rm(hel_ntxcounties)

                                  #####I don't even know what's going on with these public and private tables any longer.
hel_geoFIN <- mutate(hel_geo,
                     privateper = round(hel_totalpopE*(hel_pripctE/100)),
                     publicper = round(hel_totalpopE*(hel_pubpctE/100)),
                     difper = hel_popinsE-(privateper+publicper),
                     difpop = hel_popinsE-(hel_poppriE+hel_poppubE),
                     diftot = hel_totalpopE-(hel_poppriE+hel_poppubE),
                     difidk = hel_totalpopE-(privateper+publicper+hel_popuninsE))
   
#Export to .csv
export(hel_geo, "PASTE EXPORT DIRECTORY HERE")
