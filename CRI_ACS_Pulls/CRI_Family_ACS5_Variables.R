# Setup ----
library(tidycensus)
library(tidyverse)

counties <- c("dallas",
              "rockwall",
              "collin county",
              "denton",
              "tarrant",
              "kaufman",
              "ellis")

# CRI ACS Family Variables no transformation
fam_variables <- c(
  fam_hhc = "S1101_C01_005",
  fam_hhcmh = "S1101_C03_005",
  fam_hhcfh = "S1101_C04_005",
  fam_mghh = "B11017_001"
)

# City of Dallas 5 Year ACS Family Variable
fam_cityofdall <- cri_get_acs(
  geography = "place",
  variables = fam_variables,
  year = 2018
) %>%
  filter(GEOID == 4819000)

# Dallas County 5 Year ACS Family Variables
fam_dallcounty <- cri_get_acs(
  geography = "tract",
  variables = fam_variables,
  year = 2018, county = "dallas",
) %>%
  mutate(GEOTYPE = "TRACT")

# North Texas Counties 5 Year ACS Family Variables
fam_ntxcounties <- list_rbind(
  map(c("tract", "county"),
      cri_get_acs,
      year = 2018, variables = fam_variables, county = counties
  )
)

# Dallas/Ft. Worth MSA 5 Year ACS Family Variables
fam_dfw <- cri_get_acs(
  geography = "cbsa",
  variables = fam_variables,
  year = 2018
) %>%
  filter(GEOID == 19100)

# merge datasets
fam_geo <- rbind(fam_cityofdall,
                 fam_dallcounty,
                 fam_dfw,
                 fam_ntxcounties) %>%
  distinct()

# combining and estimating
# margins of estimate(M) and coefficient of variation(CV)
fam_geo <- mutate(fam_geo,
  fam_tphhE = fam_hhcE - (fam_hhcmhE + fam_hhcfhE),
  fam_tphh_pctE = (fam_tphhE / fam_hhcE) * 100
)
