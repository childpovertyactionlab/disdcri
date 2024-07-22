# Setup ----
library(pastecs)
library(rio)
library(tidycensus)
library(tidyverse)
library(tigris)

options(tigris_use_cache = TRUE)

# Geography ----

counties <- c(
  "dallas",
  "rockwall",
  "collin county",
  "denton",
  "tarrant",
  "kaufman",
  "ellis"
)

# CRI ACS Community Variables ----
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

# CRI ACS Economics Variables ----
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



# CRI ACS Education Variables ----
edu_variables <- c(
  edu_popadu = "S1501_C01_006",
  edu_bach = "S1501_C01_012",
  edu_prof = "S1501_C01_013",
  edu_k34 = "B09001_004",
  edu_k34mpu = "B14003_004",
  edu_k34mpr = "B14003_013",
  edu_k34fpu = "B14003_032",
  edu_k34fpr = "B14003_041"
)
# CRI ACS Family Variables ----
fam_variables <- c(
  fam_hhc = "S1101_C01_005",
  fam_hhcmh = "S1101_C03_005",
  fam_hhcfh = "S1101_C04_005",
  fam_mghh = "B11017_001"
)
# CRI ACS Health Variables ----
hel_variables <- c(
  hel_totalpop = "S2701_C01_001",
  hel_popunins = "S2701_C04_001",
  hel_popins = "S2701_C02_001",
  hel_pripct = "S2703_C03_001",
  hel_pubpct = "S2704_C03_001",
  hel_poppri = "S2703_C01_001",
  hel_poppub = "S2704_C01_001"
)

# Descriptive Stats ----
desc_var <- c(
  "nbr.val",
  "nbr.null",
  "nbr.na",
  "min",
  "max",
  "range",
  "sum",
  "median",
  "mean",
  "SE.mean",
  "CI.mean.0.95",
  "var",
  "std.dev",
  "coef.var"
)
# Import ----
cri_get_acs <- function(geography, year, variables, ...) {

  if(geography == "cbsa"){
    state <- NULL
  }else{
    state <- "TX"
  }

  get_acs(
    geography = geography,
    variables = variables,
    state = state,
    year = year,
    survey = "acs5",
    output = "wide",
    ...
  ) %>%
    mutate(GEOTYPE = toupper(geography))
}

# Tidy ----

# Transform ----

# Visualize ----

# Model ----

# Communicate ----
