# Setup ----
source("toolkit.R")

# City of Dallas 5 Year ACS Education Variables
edu_cityofdall <- cri_get_acs(
  geography = "place",
  year = 2018,
  variables = edu_variables
) %>%
  filter(GEOID == 4819000)

# Dallas County 5 Year ACS Education Variables
edu_dallcounty <- cri_get_acs(
  geography = "tract",
  year = 2018,
  variables = edu_variables,
  county = "dallas"
)
# North Texas Counties 5 Year ACS Education Variables
edu_ntxcounties <- list_rbind(
  map(c("tract", "county"),
    cri_get_acs,
    year = 2018, variables = edu_variables, county = counties
  )
)

# Dallas/Ft. Worth MSA 5 Year ACS Education Variables
edu_dfwS <- get_acs(
  geography = "cbsa",
  variables = edu_variables,
  year = 2018, survey = "acs5", output = "wide"
) %>%
  filter(GEOID == 19100) %>%
  mutate(GEOTYPE = "MSA")

# merge datasets
edu_geo <- rbind(
  edu_cityofdall,
  edu_dallcounty,
  edu_dfw,
  edu_ntxcounties
) %>%
  distinct()

# combining and estimating
# margins of estimate(M) and coefficient of variation(CV)
edu_geo <- mutate(edu_geo,
  edu_multdegE = edu_bachE + edu_profE,
  edu_multdegM = sqrt((edu_bachM^2) + (edu_profM^2)),
  edu_multdegCV = (edu_multdegM / 1.645) / edu_multdegE * 100,
  edu_k34esE = edu_k34mpuE + edu_k34mprE + edu_k34fpuE + edu_k34fprE,
  edu_k34esM = sqrt((edu_k34mpuM^2) + (edu_k34mprM^2) + (edu_k34fpuM^2) + (edu_k34fprM^2)),
  edu_k34esCV = (edu_k34esM / 1.645) / edu_k34esE * 100
)

# creating missing population estimates
edu_geo <- mutate(edu_geo,
  edu_perbach = (edu_multdegE / 100) * edu_popaduE,
  edu_perearlyed = (edu_k34esE / 100) * edu_k34E
)

# dropping superfluous variables
edu_geoFINAL <- select(edu_geo, -(edu_k34mpuE:edu_k34fprM))
