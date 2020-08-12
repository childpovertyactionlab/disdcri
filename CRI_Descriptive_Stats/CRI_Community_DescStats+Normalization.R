rm(list=ls(all=TRUE))
setwd("SET AS WORKING DIRECTORY OF DOWNLOADED FILE LOCATIONS")
library(tidyverse)
library(rio)
library(pastecs)

#import 2 mile buffer files for each indicator.
community.acs <- import("CRI Community Buffer Data.csv")

#rename variables where necessary
colnames(community)
community.r <- community %>%
  select(-(ends_with("M")), -("juvcrime"), -("STD_TotOff")) %>%
  rename(comm_pop = popE,
         comm_adupop = adupopE,
         comm_juvpop = juvpopE,
         comm_thh = thhE,
         comm_oohh = oohhE,
         comm_rohh = rohhE,
         comm_ocb1 = ocb1E,
         comm_ocb2 = ocb2E,
         comm_ocb3 = ocb3E,
         comm_ocb4 = ocb4E,
         comm_ocb5 = ocb5E,
         comm_rcb1 = rcb1E,
         comm_rcb2 = rcb2E,
         comm_rcb3 = rcb3E,
         comm_rcb4 = rcb4E,
         comm_rcb5 = rcb5E,
         comm_bb1 = bb1E,
         comm_bb2 = bb2E,
         comm_bb3 = bb3E,
         comm_bb4 = bb4E,
         comm_bb5 = bb5E,
         comm_cctrs = commctrs,
         comm_lib = libct,
         comm_park = SUM_SqMi,
         comm_aduincarct = aduincar_c,
         comm_rv1 = rv1,
         comm_rv2 = rv2,
         comm_rv3 = rv3,
         comm_rv4 = rv4,
         comm_rv5 = rv5,
         comm_bv1 = bv1,
         comm_bv2 = bv2,
         comm_bv3 = bv3,
         comm_bv4 = bv4,
         comm_bv5 = bv5,
         comm_rvp = rvp,
         comm_bvp = bvp,
         comm_ltrvp = ltrvp,
         comm_ltbvp = ltbvp,
         comm_evf = avgevf,
         comm_juvcrime = juvcrime_3
         ) %>%
  select("TEA", "SLN", "AreaSqMi", everything(.))
rm(community.j)

#create new indicator variables based on input variables
community.m <- mutate(community.r,
                      comm_bbp = (comm_bb1+comm_bb2+comm_bb3)/comm_bb4,
                      comm_ocb = (comm_ocb1+comm_ocb2+comm_ocb3+comm_ocb4+comm_ocb5),
                      comm_ocbp = (comm_ocb/comm_oohh),
                      comm_rcb = (comm_rcb1+comm_rcb2+comm_rcb3+comm_rcb4+comm_rcb5),
                      comm_rcbp = (comm_rcb/comm_rohh),
                      comm_u18bbp = (comm_bb1/comm_bb5),
                      comm_cctrcap = comm_cctrs/(comm_pop/10000),
                      comm_incarpct = (comm_aduincarct/comm_adupop),
                      comm_juvcrimecap = comm_juvcrime/(comm_juvpop/10000),
                      comm_libcap = comm_lib/(comm_pop/10000),
                      comm_parkcap = comm_park/(comm_pop/10000),
                      comm_evrate = comm_evf/comm_rohh) %>%
  select("TEA", "SLN", everything(.), -(comm_ocb1:comm_bb5), -(comm_rv1:comm_bv5), -("AreaSqMi"))
rm(community.r)

#new dataframe only containing indicator variables which need to be summarized
community.i <- select(community.m, "TEA", "SLN", "comm_bbp", "comm_bvp", "comm_cctrcap", "comm_incarpct", "comm_juvcrimecap", 
                      "comm_libcap", "comm_ltbvp", "comm_ltrvp", "comm_ocb", "comm_ocbp", "comm_parkcap", "comm_rcb", 
                      "comm_rcbp", "comm_rvp", "comm_u18bbp", "comm_evrate")
rm(community.m)

#descriptive stats for each dataframe
var <-  c("nbr.val", "nbr.null", "nbr.na", "min", "max", "range", "sum", "median", "mean", "SE.mean", "CI.mean.0.95", "var", "std.dev", "coef.var")
community.d <- cbind(var, stat.desc(community.i))
rm(var)

#shapiro-wilks test of normality on all estimate variables
#will then be filtered to only include variables with a p-value < 0.05 in new df
#must then manually check these new df to see which variables must be log transformed in next step
community.n <- as_tibble(do.call(rbind, lapply(community.i, function(x) shapiro.test(x)[c("statistic", "p.value")])), rownames = "var") %>%
  filter(p.value < 0.05)
community.n

#variables that are not normally distributed to sw will be log transformed
#variables that have a '0' value will have '0.0001' added to allow transformation
#variables that are a negative factor to communities will be multiplied by -1 to keep directionality of all variables consistent
#dataframes will then be sorted with identifiers followed by column names alphabetically

###THIS PORTION OF SCRIPT MUST BE REVISED MANUALLY EACH TIME A NEW INDICATOR IS ADDED, CHECK AGAINST 'community.n'###
community.l <- mutate(community.i,
                      comm_rvp = (log(comm_rvp))*(-1),
                      comm_bvp = (log(comm_bvp))*(-1),
                      comm_ltrvp = (log(comm_ltrvp))*(-1),
                      comm_ltbvp = (log(comm_ltbvp))*(-1),
                      comm_ocb = (log(comm_ocb))*(-1),
                      comm_ocbp = (log(comm_ocbp))*(-1),
                      comm_rcb = (log(comm_rcb))*(-1),
                      comm_rcbp = (log(comm_rcbp))*(-1),
                      comm_cctrcap = log(comm_cctrcap+0.0001),
                      comm_incarpct = (log(comm_incarpct))*(-1),
                      comm_juvcrimecap = (log(comm_juvcrimecap+0.0001))*(-1),
                      comm_libcap = log(comm_libcap+0.0001),
                      comm_parkcap = log(comm_parkcap+0.0001),
                      comm_evrate = log(comm_evrate)*(-1)) %>%
  select("SLN", "TEA", sort(names(.)))

#standardize all variables to z scores using the scale function
#second line of function omits the "SLN", "TEA", and "AreaSqMi" columns from being transformed with the 'scale' function
community.s <- community.l %>%
  mutate_at(c(3:18), funs(c(scale(.)))) %>%
  rename_all(funs(str_replace(.,"comm_", "ci_")))
rm(community.l)

#create 5 quintile bins for each indicator and input variables in the dataframe from the non-normalized/scaled data and renames columns
community.q <- community.i %>%
  mutate_at(c(3:18), funs(c(quin_ = cut(.,5)))) %>%
  setNames(c(names(.)[1], paste0(names(.)[-1],"_quintile")))%>%
  select(-("SLN_quintile")) %>%
  mutate_at(c(2:17), funs((.)-1)) 

community.qF <- left_join(community.i, community.q)
rm(community.q)

#export non-normalized/scaled data with quintiles to .csv
export(community.qF, "SET AS EXPORT DIRECTORY/CRI Community Buffer Variables Quintiles.csv")

#export normalized buffer dataframes for each indicator to .csv
export(community.s, "SET AS EXPORT DIRECTORY/CRI Community Buffer Variables Normalized.csv")

#export descriptive stats dataframes for each indicator to .csv
export(community.d, "SET AS EXPORT DIRECTORY/CRI Community Buffer Variables Descriptive Stats.csv")
