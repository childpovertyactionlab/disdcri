rm(list=ls(all=TRUE))
setwd("SET AS WORKING DIRECTORY OF DOWNLOADED FILE LOCATIONS")
library(tidyverse)
library(rio)
library(pastecs)

#import 2 mile buffer files for each indicator.
economics <- import("CRI Economics Buffer Data.csv")

#rename variables where necessary
colnames(economics)
economics.r <- economics %>%
  select(-(ends_with("M")), -("AreaSqMi"), -("blockcount"), -("econ_5t1_1"), -("econ_pop_1"), -("urE"), -("pyrE"), -("MEAN_eco_2"))

colnames(economics.r)
economics.r <- economics.r %>%
  rename(econ_adupop = adupopE,
         econ_pop = popE,
         econ_belpov = belpovE,
         econ_kids = kidsE,
         econ_cbp = cbpE,
         econ_u5 = u5E,
         econ_u5bp = u5bpE,
         econ_5t17 = econ_5t17E,
         econ_5t17bp = econ_5t17b,
         econ_juvpop = juvpopE,
         econ_unem = unemE,
         econ_juvunem = juvunemE,
         econ_lwjobs = lwjobs,
         econ_mwjobs = mwjobs,
         econ_totjobs = totjobs,
         econ_payday = paydayct,
         econ_creditunions = creditunio,
         econ_retailbanks = retailbank,
         econ_medinc = medincE,
         econ_popo16 = econ_popov) %>%
  select("TEA", "SLN", everything(.))
rm(economics.j)

#create new indicator variables based on input variables
economics.m <- economics.r %>%
  mutate(econ_cpr = (econ_cbp/econ_kids),
         econ_fincap = (econ_creditunions+econ_retailbanks)/(econ_pop/10000),
         econ_paydaycap = econ_payday/(econ_pop/10000), 
         econ_pctlwjobs = (econ_lwjobs/econ_totjobs),
         econ_pctmwjobs = (econ_mwjobs/econ_totjobs),
         econ_pr = (econ_belpov/econ_pop),
         econ_pyr = (econ_juvunem/econ_juvpop),
         econ_ur = (econ_unem/econ_popo16),
         econ_u5rbp = (econ_u5bp/econ_u5),
         econ_5t17rbp = (econ_5t17bp/econ_5t17))
rm(economics.r)

#new dataframe only containing indicator variables which need to be summarized
economics.i <- select(economics.m, "TEA", "SLN", "econ_cpr", "econ_fincap", "econ_medinc", "econ_paydaycap", 
                       "econ_pctlwjobs", "econ_pctmwjobs", "econ_pr", "econ_pyr", "econ_ur")
rm(economics.m)

#descriptive stats for each dataframe
var <-  c("nbr.val", "nbr.null", "nbr.na", "min", "max", "range", "sum", "median", "mean", "SE.mean", "CI.mean.0.95", "var", "std.dev", "coef.var")
economics.d <- cbind(var, stat.desc(economics.i))
rm(var)

#shapiro-wilks test of normality on all estimate variables
#will then be filtered to only include variables with a p-value < 0.05 in new df
#must then manually check these new df to see which variables must be log transformed in next step
economics.n <- as_tibble(do.call(rbind, lapply(economics.i, function(x) shapiro.test(x)[c("statistic", "p.value")])), rownames = "var") %>%
  filter(p.value < 0.05)
economics.n

#variables that are not normally distributed to sw will be log transformed
#variables that have a '0' value will have '0.0001' added to allow transformation
#variables that are a negative factor to communities will be multiplied by -1 to keep directionality of all variables consistent
#dataframes will then be sorted with identifiers followed by column names alphabetically

###THIS PORTION OF SCRIPT MUST BE REVISED MANUALLY EACH TIME A NEW INDICATOR IS ADDED, CHECK AGAINST 'economics.n'###
economics.l <- mutate(economics.i,
                      econ_medinc  = log(econ_medinc),
                      econ_cpr = (log(econ_cpr))*(-1),
                      econ_fincap = log(econ_fincap+0.0001),
                      econ_paydaycap = log(econ_paydaycap+0.0001),
                      econ_pr = (log(econ_pr))*(-1),
                      econ_pyr = (log(econ_pyr))*(-1),
                      econ_ur = (log(econ_ur))*(-1),
                      econ_pctlwjobs = log(econ_pctlwjobs),
                      econ_pctmwjobs = log(econ_pctmwjobs)) %>%
  select("SLN", "TEA", sort(names(.)))

#standardize all variables to z scores using the scale function
#second line of function omits the "SLN", "TEA", and "AreaSqMi" columns from being transformed with the 'scale' function
economics.s <- economics.l %>%
  mutate_at(c(3:11), funs(c(scale(.)))) %>%
  rename_all(funs(str_replace(.,"econ_", "eci_")))
rm(economics.l)

#create 5 quintile bins for each indicator and input variables in the dataframe from the non-normalized/scaled data and renames columns
economics.q <- economics.i %>%
  mutate_at(c(3:11), funs(c(quin_ = cut(.,5)))) %>%
  setNames(c(names(.)[1], paste0(names(.)[-1],"_quintile"))) %>%
  select(-("SLN_quintile")) %>%
  mutate_at(c(2:10), funs((.)-1)) 

economics.qF <- left_join(economics.i, economics.q)
rm(economics.q)

#export non-normalized/scaled data with quintiles to .csv
export(economics.qF, "SET AS EXPORT DIRECTORY/CRI Economics Buffer Variables Quintiles.csv")

#export normalized buffer dataframes for each indicator to .csv
export(economics.s, "SET AS EXPORT DIRECTORY/CRI Economics Buffer Variables Normalized.csv")

#export descriptive stats dataframes for each indicator to .csv
export(economics.d, "SET AS EXPORT DIRECTORY/CRI Economics Buffer Variables Descriptive Stats.csv")
