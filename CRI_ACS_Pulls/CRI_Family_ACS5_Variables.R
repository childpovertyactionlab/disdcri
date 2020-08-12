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

#CRI ACS Family Variables no transformation
fam_variables <- c(
  fam_hhc = "S1101_C01_005", 
  fam_hhcmh = "S1101_C03_005", 
  fam_hhcfh = "S1101_C04_005"
)

fam_mghh <- c(fam_mghh = "B11017_001")

#City of Dallas 5 Year ACS Family Variable
fam_cityofdallS <- get_acs(geography = "place", variables = fam_variables,
                                      state = "TX", year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 4819000) %>%
  mutate(
    GEOTYPE = "PLACE")

fam_cityofdallB <- get_acs(geography = "place", variables = fam_mghh,
                          state = "TX", year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 4819000) %>%
  mutate(
    GEOTYPE = "PLACE")

fam_cityofdall <- full_join(fam_cityofdallS, fam_cityofdallB)

rm(fam_cityofdallB)
rm(fam_cityofdallS)

#Dallas County 5 Year ACS Family Variables
fam_dallcountyS <- get_acs(geography = "tract", variables = fam_variables,
                                            state = "TX", county = "dallas", year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

fam_dallcountyB <- get_acs(geography = "tract", variables = fam_mghh,
                          state = "TX", county = "dallas", year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

fam_dallcounty <- full_join(fam_dallcountyS, fam_dallcountyB)

rm(fam_dallcountyB)
rm(fam_dallcountyS)

#North Texas Counties 5 Year ACS Family Variables
fam_ntxcountiesTs <- get_acs(geography = "tract", variables = fam_variables,
                             state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

fam_ntxcountiesCs <- get_acs(geography = "county", variables = fam_variables,
                             state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "COUNTY")

fam_Sntxcounties <- full_join(fam_ntxcountiesTs, fam_ntxcountiesCs)

rm(fam_ntxcountiesCs)
rm(fam_ntxcountiesTs)

fam_ntxcountiesTb <- get_acs(geography = "tract", variables = fam_mghh,
                             state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

fam_ntxcountiesCb <- get_acs(geography = "county", variables = fam_mghh,
                             state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "COUNTY")

fam_Bntxcounties <- full_join(fam_ntxcountiesTb, fam_ntxcountiesCb,)

rm(fam_ntxcountiesCb)
rm(fam_ntxcountiesTb)

fam_ntxcounties <- full_join(fam_Sntxcounties, fam_Bntxcounties)

rm(fam_Sntxcounties)
rm(fam_Bntxcounties)

#Dallas/Ft. Worth MSA 5 Year ACS Family Variables
fam_dfwS <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", variables = fam_variables,
                                        year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

fam_dfwB <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", variables = fam_mghh,
                   year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

fam_dfw <- full_join(fam_dfwS, fam_dfwB)

rm(fam_dfwB)
rm(fam_dfwS)

#merge datasets
fam_geo <- full_join(full_join(full_join(fam_cityofdall, fam_dallcounty), fam_dfw), fam_ntxcounties)

#combining and estimating margins of estimate(M) and coefficient of variation(CV)
fam_geo <- mutate(fam_geo, 
                   fam_tphhE = fam_hhcE - (fam_hhcmhE + fam_hhcfhE),
                   fam_tphh_pctE = (fam_tphhE/fam_hhcE)*100)
                   
#Export to .csv
export(fam_geo, "PASTE EXPORT DIRECTORY HERE")
