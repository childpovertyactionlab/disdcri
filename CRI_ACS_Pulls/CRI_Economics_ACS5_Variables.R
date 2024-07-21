# Setup ----
library(tidycensus)
library(tidyverse)
library(tigris)

options(tigris_use_cache = TRUE)

vars_subj_acs18 <- load_variables(2018, "acs5/subject", cache = TRUE)

vars_acs18 <- load_variables(year = 2018, "acs5")

counties <- c(
  "dallas",
  "rockwall",
  "collin county",
  "denton",
  "tarrant",
  "kaufman",
  "ellis"
)

# CRI ACS Economics Variables no transformation ----
econ_variables <- c(
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
  econ_popov16 = "S0101_C01_025",
  econ_medinc = "B19013_001",
  econ_unem = "B23025_005",
  econ_juvunemM1 = "B23001_008",
  econ_juvunemM2 = "B23001_015",
  econ_juvunemM3 = "B23001_022",
  econ_juvunemF1 = "B23001_094",
  econ_juvunemF2 = "B23001_101",
  econ_juvunemF3 = "B23001_108"
)

# City of Dallas 5 Year ACS Economic Variables ----

econ_cityofdall <- cri_get_acs(
  geography = "place",
  year = 2018,
  variables = econ_variables
) %>%
  filter(GEOID == 4819000)

# Dallas County 5 Year ACS Community Variables
econ_dallcounty <- cri_get_acs(
  geography = "tract",
  year = 2018,
  variables = econ_variables,
  county = "dallas",
)
# North Texas Counties 5 Year ACS Community Variables

econ_ntxcounties <- list_rbind(
  map(c("tract", "county"),
    cri_get_acs,
    year = 2018, variables = econ_variables, county = counties
  )
)

# Dallas/Ft. Worth MSA 5 Year ACS Community Variables
econ_dfw <- get_acs(
  geography = "cbsa",
  variables = econ_variables,
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

# Transform ----

# merge datasets
econ_geo <- rbind(
  econ_cityofdall,
  econ_dallcounty,
  econ_dfw,
  econ_ntxcounties
) %>%
  distinct()

# combining and estimating margins of estimate(M)
# and coefficient of variation(CV)
econ_geo <- mutate(econ_geo,
  econ_juvpopE = econ_juvpop1E + econ_juvpop2E,
  econ_juvpopM = sqrt((econ_juvpop1M^2) + (econ_juvpop2M^2)),
  econ_juvpopCV = (econ_juvpopM / 1.645) / econ_juvpopE * 100,
  econ_juvunemE = econ_juvunemM1E + econ_juvunemM2E + econ_juvunemM3E + econ_juvunemF1E + econ_juvunemF2E + econ_juvunemF3E
)

# dropping superfluous variables
econ_geo <- econ_geo %>%
  select(
    -(econ_juvpop1E:econ_juvpop2M),
    -(econ_juvunemM1E:econ_juvunemF3M)
) %>%
  select("GEOID", "NAME", "GEOTYPE", sort(names(.)))
