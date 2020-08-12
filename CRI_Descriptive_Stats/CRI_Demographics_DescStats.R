rm(list=ls(all=TRUE))
setwd("SET AS WORKING DIRECTORY OF DOWNLOADED FILE LOCATIONS")
library(tidyverse)
library(rio)
library(pastecs)

#import 2 mile buffer files for each indicator.
demographics <- import("CRI Demographics Buffer Data.csv")

#rename variables where necessary
colnames(demographics)
demographics.r <- demographics %>%
  rename(dem_totp = tpop,
         dem_popwh = pwh,
         dem_popbl = pbl,
         dem_popas = pas,
         dem_pophi = phis,
         dem_popm = popm,
         dem_popf = popf,
         dem_thh = thh,
         dem_popse = psen,
         dem_popch = pchi) %>%
  select("TEA", "SLN", everything(.))

#descriptive stats for each dataframe
var <-  c("nbr.val", "nbr.null", "nbr.na", "min", "max", "range", "sum", "median", "mean", "SE.mean", "CI.mean.0.95", "var", "std.dev", "coef.var")
demographics.d <- cbind(var, stat.desc(demographics.r))
rm(var)

#create 5 quintile bins for each indicator and input variables in the dataframe from the non-normalized/scaled data and renames columns
demographics.q <- demographics.r %>%
  mutate_at(c(3:12), funs(c(quin_ = cut(.,5)))) %>%
  setNames(c(names(.)[1], paste0(names(.)[-1],"_quintile"))) %>%
  select(-("SLN_quintile")) %>%
  mutate_at(c(2:11), funs((.)-1)) 

demographics.qF <- left_join(demographics.r, demographics.q)
rm(demographics.q)

#export non-normalized/scaled data with quintiles to .csv
export(demographics.qF, "SET AS EXPORT DIRECTORY/CRI Demographics Buffer Variables Quintiles.csv")

#export descriptive stats dataframes for each indicator to .csv
export(demographics.d, "SET AS EXPORT DIRECTORY/CRI Demographics Buffer Variables Descriptive Stats.csv")
