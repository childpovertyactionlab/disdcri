rm(list=ls(all=TRUE))
setwd("PASTE WORKING DIRECTORY HERE")
library(tidycensus)
library(tidyverse)
library(rio)

#census_api_key("aed7bfa15ecfb5bdac5fc798f4bd0aa63d56bab4", install = TRUE)
acs18 <- load_variables(2018, "acs5", cache = TRUE)
#data(fips_codes)
#years <- lst(2014, 2015, 2016, 2017, 2018)

counties <- c("dallas", "rockwall", "collin county", "denton", "tarrant", "kaufman", "ellis")

#CRI ACS Community Variables no transformation
comm_variables <- c(
  comm_thh = "B25106_001", 
  comm_oohh = "B25106_002", 
  comm_rohh = "B25106_024", 
  comm_ocb1 = "B25106_006", 
  comm_ocb2 = "B25106_010", 
  comm_ocb3 = "B25106_014", 
  comm_ocb4 = "B25106_018", 
  comm_ocb5 = "B25106_022", 
  comm_rcb1 = "B25106_028",
  comm_rcb2 = "B25106_032", 
  comm_rcb3 = "B25106_036", 
  comm_rcb4 = "B25106_040", 
  comm_rcb5 = "B25106_044",
  comm_bb1 = "B28005_005",
  comm_bb2 = "B28005_011",
  comm_bb3 = "B28005_017",
  comm_bb4 = "B28005_001",
  comm_bb5 = "B28005_002"
  )

#City of Dallas 5 Year ACS Community Variable
comm_cityofdall <- get_acs(geography = "place", variables = comm_variables,
                                      state = "TX", year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 4819000) %>%
  mutate(
    GEOTYPE = "PLACE")


#Dallas County 5 Year ACS Community Variables
comm_dallcounty <- get_acs(geography = "tract", variables = comm_variables,
                                            state = "TX", county = "dallas", year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

#North Texas Counties 5 Year ACS Community Variables
comm_ntxcountiesT <- get_acs(geography = "tract", variables = comm_variables,
                                            state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "TRACT")

comm_ntxcountiesC <- get_acs(geography = "county", variables = comm_variables,
                            state = "TX", county = counties, year = 2018, survey = "acs5", output = "wide") %>%
  mutate(GEOTYPE = "COUNTY")

comm_ntxcounties <- full_join(comm_ntxcountiesT, comm_ntxcountiesC)

rm(comm_ntxcountiesC)
rm(comm_ntxcountiesT)

#Dallas/Ft. Worth MSA 5 Year ACS Community Variables
comm_dfw <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", variables = comm_variables,
                                        year = 2018, survey = "acs5", output = "wide") %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

#merge datasets
comm_geo <- full_join(full_join(full_join(comm_cityofdall, comm_dallcounty), comm_dfw), comm_ntxcounties)

#combining and estimating margins of estimate(M) and coefficient of variation(CV)
comm_geo <- mutate(comm_geo, 
                   comm_u18bbpE = comm_bb1E/comm_bb5E,
                   comm_bbpE = (comm_bb1E+comm_bb2E+comm_bb3E)/comm_bb4E,
                   comm_u18bbpM = comm_bb1M/comm_bb5M,
                   comm_bbpM = sqrt((comm_bb1M^2)+(comm_bb2M^2)+(comm_bb3M^2)/comm_bb4M)
)

#Export to .csv
export(comm_geo, "PASTE EXPORT DIRECTORY HERE")
