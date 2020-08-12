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

#CRI ACS Education Variables no transformation
edu_Svariables <- c(
  edu_popadu = "S1501_C01_006", 
  edu_bach = "S1501_C01_012", 
  edu_prof = "S1501_C01_013")

edu_Bvariables <- c(
  edu_k34 = "B09001_004", 
  edu_k34mpu = "B14003_004", 
  edu_k34mpr = "B14003_013",
  edu_k34fpu = "B14003_032",
  edu_k34fpr = "B14003_041"
  )
        ##B tables and S tables unable to load with same get_acs pull, have to be done seperately.

#City of Dallas 5 Year ACS Education Variables
edu_cityofdallS <- get_acs(geography = "place", variables = edu_Svariables,
                            state = "TX", year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 4819000) %>%
  mutate(
    GEOTYPE = "PLACE")

edu_cityofdallB <- get_acs(geography = "place", variables = edu_Bvariables,
                            state = "TX", year = 2018, survey = "acs5", output = "wide") %>%  
  filter(GEOID == 4819000) %>%
  mutate(
    GEOTYPE = "PLACE")

edu_cityofdall <- full_join(edu_cityofdallS, edu_cityofdallB)

rm(edu_cityofdallB)
rm(edu_cityofdallS)

#Dallas County 5 Year ACS Education Variables
edu_dallcountyS <- get_acs(geography = "tract", variables = edu_Svariables,
                            state = "TX", county = "dallas", year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

edu_dallcountyB <- get_acs(geography = "tract", variables = edu_Bvariables,
                            state = "TX", county = "dallas", year = 2018, survey = "acs5", output = "wide") %>%  
  mutate(
    GEOTYPE = "TRACT")

edu_dallcounty <- full_join(edu_dallcountyS, edu_dallcountyB)

rm(edu_dallcountyB)
rm(edu_dallcountyS)

#North Texas Counties 5 Year ACS Education Variables
edu_ntxcountiesTs <- get_acs(geography = "tract", variables = edu_Svariables,
                              state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

edu_ntxcountiesCs <- get_acs(geography = "county", variables = edu_Svariables,
                              state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "COUNTY")

edu_Sntxcounties <- full_join(edu_ntxcountiesTs, edu_ntxcountiesCs)

rm(edu_ntxcountiesCs)
rm(edu_ntxcountiesTs)

edu_ntxcountiesTb <- get_acs(geography = "tract", variables = edu_Bvariables,
                              state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

edu_ntxcountiesCb <- get_acs(geography = "county", variables = edu_Bvariables,
                              state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "COUNTY")

edu_Bntxcounties <- full_join(edu_ntxcountiesTb, edu_ntxcountiesCb,)

rm(edu_ntxcountiesCb)
rm(edu_ntxcountiesTb)

edu_ntxcounties <- full_join(edu_Sntxcounties, edu_Bntxcounties)

rm(edu_Sntxcounties)
rm(edu_Bntxcounties)

#Dallas/Ft. Worth MSA 5 Year ACS Education Variables
edu_dfwS <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", variables = edu_Svariables,
                     year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

edu_dfwB <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", variables = edu_Bvariables,
                     year = 2018, survey = "acs5", output = "wide") %>%  
  filter(GEOID == 19100) %>%
  mutate(
    GEOTYPE = "MSA")

edu_dfw <- full_join(edu_dfwS, edu_dfwB)

rm(edu_dfwB)
rm(edu_dfwS)

#merge datasets
edu_geo <- full_join(full_join(full_join(edu_cityofdall, edu_dallcounty), edu_dfw), edu_ntxcounties)

#combining and estimating margins of estimate(M) and coefficient of variation(CV)
edu_geo <- mutate(edu_geo, 
                   edu_multdegE = edu_bachE + edu_profE,
                   edu_multdegM = sqrt((edu_bachM^2)+(edu_profM^2)),
                   edu_multdegCV = (edu_multdegM/1.645)/edu_multdegE*100,
                   edu_k34esE = edu_k34mpuE + edu_k34mprE + edu_k34fpuE + edu_k34fprE,
                   edu_k34esM = sqrt((edu_k34mpuM^2)+(edu_k34mprM^2)+(edu_k34fpuM^2)+(edu_k34fprM^2)),
                   edu_k34esCV = (edu_k34esM/1.645)/edu_k34esE*100)

#creating missing population estimates
edu_geo <- mutate(edu_geo,
                   edu_perbach = (edu_multdegE/100)*edu_popaduE,
                   edu_perearlyed = (edu_k34esE/100)*edu_k34E)

#dropping superfluous variables
edu_geoFINAL <- select(edu_geo, -(edu_k34mpuE:edu_k34fprM))


#Export to .csv
export(edu_geoFINAL, "PASTE EXPORT DIRECTORY HERE")
