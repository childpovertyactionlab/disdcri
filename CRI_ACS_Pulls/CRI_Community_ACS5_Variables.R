# Setup ----
source("toolkit.R")

# Import ----

comm_cityofdall <- cri_get_acs(
  geography = "place",
  year = 2018,
  variables = comm_variables
) %>%
  filter(GEOID == 4819000)

# Dallas County Tracts 5 Year ACS Community Variables
comm_dallcounty <- cri_get_acs(
  geography = "tract",
  year = 2018,
  variables = comm_variables,
  county = "dallas"
)

# North Texas Counties 5 Year ACS Community Variables
comm_ntxcounties <- list_rbind(
  map(c("tract", "county"),
    cri_get_acs,
    year = 2018, variables = comm_variables, county = counties
  )
)

# DFW MSA 5 Year ACS Community Variables
comm_dfw <- get_acs(
  geography = "cbsa",
  variables = comm_variables,
  year = 2018,
  survey = "acs5",
  output = "wide"
) %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

# Transform ----

# merge datasets
comm_geo <- rbind(
  comm_cityofdall,
  comm_dallcounty,
  comm_dfw,
  comm_ntxcounties
) %>%
  distinct()

# combining and estimating
# margins of estimate(M) and coefficient of variation(CV)
comm_geo <- mutate(comm_geo,
  comm_u18bbpE = comm_bb1E / comm_bb5E,
  comm_bbpE = (comm_bb1E + comm_bb2E + comm_bb3E) / comm_bb4E,
  comm_u18bbpM = comm_bb1M / comm_bb5M,
  comm_bbpM = sqrt((comm_bb1M^2) + (comm_bb2M^2) + (comm_bb3M^2) / comm_bb4M)
)
