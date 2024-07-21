rm(list = ls(all = TRUE))
library(tidyverse)

# import USPS-HUD 2019 Quarterly Data
HUD2019q1 <- import("PASTE USPS Q1 FILE LOCATION HERE")
HUD2019q2 <- import("PASTE USPS Q2 FILE LOCATION HERE")
HUD2019q3 <- import("PASTE USPS Q3 FILE LOCATION HERE")
HUD2019q4 <- import("PASTE USPS Q4 FILE LOCATION HERE")

# merging quarter columns into single df listing every unique row
HUD2019 <- rbind(HUD2019q1, HUD2019q2, HUD2019q3, HUD2019q4)

# group dataframe by matching geoid
# then calculate mean of all columns
HUD2019.s <- HUD2019 %>%
  group_by(geoid) %>%
  summarise_all(list(~ mean(.)))

# select variables of interest
HUD2019.v <- select(HUD2019.s,
  geoid = geoid,
  comm_rv1 = ams_res,
  comm_rv2 = vac_6_12r,
  comm_rv3 = vac_12_24r,
  comm_rv4 = vac_24_36r,
  comm_rv5 = vac_36_res,
  comm_bv1 = ams_bus,
  comm_bv2 = vac_6_12b,
  comm_bv3 = vac_12_24b,
  comm_bv4 = vac_24_36b,
  comm_bv5 = vac_36_bus
)

# calculate indicators of interest
HUD2019.i <- mutate(HUD2019.v,
  comm_rvp = (comm_rv2 + comm_rv3 + comm_rv4 + comm_rv5) / comm_rv1,
  comm_bvp = (comm_bv2 + comm_bv3 + comm_bv4 + comm_bv5) / comm_bv1,
  comm_ltrvp = (comm_rv4 + comm_rv5) / comm_rv1,
  comm_ltbvp = (comm_bv4 + comm_bv5) / comm_bv1
)
