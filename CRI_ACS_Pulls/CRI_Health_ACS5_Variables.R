# Setup ----
source("toolkit.R")

# City of Dallas 5 Year ACS Family Variable
hel_cityofdall <- cri_get_acs(
  geography = "place",
  variables = hel_variables,
  year = 2018
) %>%
  filter(GEOID == 4819000)

# Dallas County 5 Year ACS Family Variables
hel_dallcounty <- cri_get_acs(
  geography = "tract",
  variables = hel_variables,
  county = "dallas", year = 2018,
)

# North Texas Counties 5 Year ACS Family Variables
hel_ntxcounties <- list_rbind(
  map(c("tract", "county"),
      cri_get_acs,
      year = 2018, variables = hel_variables, county = counties)
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
