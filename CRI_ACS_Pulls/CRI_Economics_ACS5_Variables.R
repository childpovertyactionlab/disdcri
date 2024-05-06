library(tidycensus)
library(tidyverse)
library(rio)

vars_subj_acs18 <- load_variables(2018, "acs5/subject", cache = TRUE)

vars_dp_acs18 <- load_variables(year = 2018, "acs5/profile")

counties <- c("dallas",
              "rockwall",
              "collin county",
              "denton",
              "tarrant",
              "kaufman",
              "ellis")

# CRI ACS Economics Variables no transformation ----
econ_Svariables <- c(
  econ_adupop = "B23001_001",
  econ_juvpop1 = "B23001_003",
  econ_juvpop2 = "DP05_0009",
  econ_pop = "S1701_C01_001",
  econ_belpov = "S1701_C02_001",
  econ_kids = "S1701_C01_002",
  econ_cbp = "S1701_C02_002",
  econ_u5 = "S1701_C01_003",
  econ_u5bp = "S1701_C02_003",
  econ_5t17 = "S1701_C01_004",
  econ_5t17bp = "S1701_C02_004",
  econ_popov16 = "S0101_C01_025"
)

# econ_juvunem: currently unable to find in ACS census,
# will calculate pop estimate later.

econ_medinc <- c(
  econ_medinc = "B19013_001",
  econ_unem = "B23025_005",
  econ_juvunemM1 = "B23001_008",
  econ_juvunemM2 = "B23001_015",
  econ_juvunemM3 = "B23001_022",
  econ_juvunemF1 = "B23001_094",
  econ_juvunemF2 = "B23001_101",
  econ_juvunemF3 = "B23001_108"
)

# B tables and S tables unable to load with same get_acs pull
# have to be done seperately.

# City of Dallas 5 Year ACS Economic Variables ----
econ_cityofdallS <- get_acs(
  geography = "place",
  variables = econ_Svariables,
  state = "TX",
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  filter(GEOID == 4819000) %>%
  mutate(GEOTYPE = "PLACE")

econ_cityofdallB <- get_acs(
  geography = "place",
  variables = econ_medinc,
  state = "TX",
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  filter(GEOID == 4819000) %>%
  mutate(
    GEOTYPE = "PLACE"
  )

econ_cityofdall <- full_join(econ_cityofdallS, econ_cityofdallB)

rm(econ_cityofdallB)
rm(econ_cityofdallS)

# Dallas County 5 Year ACS Community Variables
econ_dallcountyS <- get_acs(
  geography = "tract",
  variables = econ_Svariables,
  state = "TX",
  county = "dallas",
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(GEOTYPE = "TRACT")

econ_dallcountyB <- get_acs(
  geography = "tract",
  variables = econ_medinc,
  state = "TX", county = "dallas",
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(GEOTYPE = "TRACT")

econ_dallcounty <- full_join(econ_dallcountyS, econ_dallcountyB)

rm(econ_dallcountyB)
rm(econ_dallcountyS)

# North Texas Counties 5 Year ACS Community Variables
econ_ntxcountiesTs <- get_acs(
  geography = "tract",
  variables = econ_Svariables,
  state = "TX",
  county = counties,
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(GEOTYPE = "TRACT")

econ_ntxcountiesCs <- get_acs(
  geography = "county",
  variables = econ_Svariables,
  state = "TX",
  county = counties,
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(GEOTYPE = "COUNTY")

econ_Sntxcounties <- full_join(econ_ntxcountiesTs, econ_ntxcountiesCs)

rm(econ_ntxcountiesCs)
rm(econ_ntxcountiesTs)

econ_ntxcountiesTb <- get_acs(
  geography = "tract",
  variables = econ_medinc,
  state = "TX",
  county = counties,
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(GEOTYPE = "TRACT")

econ_ntxcountiesCb <- get_acs(
  geography = "county",
  variables = econ_medinc,
  state = "TX",
  county = counties,
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  mutate(GEOTYPE = "COUNTY")

econ_Bntxcounties <- full_join(econ_ntxcountiesTb, econ_ntxcountiesCb)

rm(econ_ntxcountiesCb)
rm(econ_ntxcountiesTb)

econ_ntxcounties <- full_join(econ_Sntxcounties, econ_Bntxcounties)

rm(econ_Sntxcounties)
rm(econ_Bntxcounties)

# Dallas/Ft. Worth MSA 5 Year ACS Community Variables
econ_dfwS <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = econ_Svariables,
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

econ_dfwB <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = econ_medinc,
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  filter(GEOID == 19100) %>%
  mutate(
    GEOTYPE = "MSA"
  )

econ_dfw <- full_join(econ_dfwS, econ_dfwB)

rm(econ_dfwB)
rm(econ_dfwS)

# merge datasets
econ_geo <- full_join(
  full_join(
    full_join(
      econ_cityofdall,
      econ_dallcounty
    ),
    econ_dfw
  ),
  econ_ntxcounties
)

# combining and estimating margins of estimate(M)
# and coefficient of variation(CV)
econ_geo <- mutate(econ_geo,
  econ_juvpopE = econ_juvpop1E + econ_juvpop2E,
  econ_juvpopM = sqrt((econ_juvpop1M^2) + (econ_juvpop2M^2)),
  econ_juvpopCV = (econ_juvpopM / 1.645) / econ_juvpopE * 100,
  econ_juvunemE = econ_juvunemM1E + econ_juvunemM2E + econ_juvunemM3E + econ_juvunemF1E + econ_juvunemF2E + econ_juvunemF3E
)

# dropping superfluous variables
econ_geoFINAL <- econ_geo %>%
  select(-(econ_juvpop1E:econ_juvpop2M),
         -(econ_juvunemM1E:econ_juvunemF3M)) %>%
  select("GEOID", "NAME", "GEOTYPE", sort(names(.)))

