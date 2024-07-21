# Setup ----
library(tidycensus)
library(tidyverse)

counties <- c(
  "dallas",
  "rockwall",
  "collin county",
  "denton",
  "tarrant",
  "kaufman",
  "ellis"
)

# CRI ACS Health Variables no transformation
hel_variables <- c(
  hel_totalpop = "S2701_C01_001",
  hel_popunins = "S2701_C04_001",
  hel_popins = "S2701_C02_001",
  hel_pripct = "S2703_C03_001",
  hel_pubpct = "S2704_C03_001",
  hel_poppri = "S2703_C01_001",
  hel_poppub = "S2704_C01_001"
)

# City of Dallas 5 Year ACS Family Variable
hel_cityofdall <- cri_get_acs(
  geography = "place",
  variables = hel_variables,
  year = 2018
) %>%
  filter(GEOID == 4819000) %>%
  mutate(
    GEOTYPE = "PLACE"
  )

# Dallas County 5 Year ACS Family Variables
hel_dallcounty <- cri_get_acs(
  geography = "tract",
  variables = hel_variables,
  county = "dallas", year = 2018,
)

# North Texas Counties 5 Year ACS Family Variables
hel_ntxcountiesTs <- cri_get_acs(
  geography = "tract",
  variables = hel_variables,
  county = counties, year = 2018,
)

hel_ntxcountiesCs <- cri_get_acs(
  geography = "county",
  variables = hel_variables,
  county = counties, year = 2018,
)

# Dallas/Ft. Worth MSA 5 Year ACS Family Variables
hel_dfw <- cri_get_acs(
  geography = "cbsa", variables = hel_variables,
  year = 2018,
) %>%
  filter(GEOID == 19100)

# merge datasets
hel_geo <- rbind(hel_cityofdall,
                 hel_dallcounty,
                 hel_dfw,
                 hel_ntxcounties) %>%
  distinct()

hel_geoFIN <- mutate(hel_geo,
  privateper = round(hel_totalpopE * (hel_pripctE / 100)),
  publicper = round(hel_totalpopE * (hel_pubpctE / 100)),
  difper = hel_popinsE - (privateper + publicper),
  difpop = hel_popinsE - (hel_poppriE + hel_poppubE),
  diftot = hel_totalpopE - (hel_poppriE + hel_poppubE),
  difidk = hel_totalpopE - (privateper + publicper + hel_popuninsE)
)
