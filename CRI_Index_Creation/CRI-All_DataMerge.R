# Setup ----
library(tidyverse)
library(rio)
library(pastecs)

# import feeder/lat-long
feeder <- import("Feeder Info.csv")

# import indicator
demographics <- import("CRI Demographics Buffer Variables Quintiles.csv")
community <- import("CRI Community Buffer Variables Quintiles.csv")
economics <- import("CRI Economics Buffer Variables Quintiles.csv")
education <- import("CRI Education Buffer Variables Quintiles.csv")
family <- import("CRI Family Buffer Variables Quintiles.csv")
health <- import("CRI Health Buffer Variables Quintiles.csv")
cdc <- import("CRI Health CDC500 - General Health Index.csv")

cdc <- select(cdc,
              TEA,
              SLN,
              hi_gen = hi_genweight)
weight <- import("CRI Sub-Indices Weighted and Scaled.csv")

#transform weighted index variables into quintiles
weight.q <- weight %>%
  select(TEA, ci_weight:cri_weight) %>%
  mutate_at(c(2:7), funs(c(quin_ = cut(.,5)))) %>%
  setNames(c(names(.)[1], paste0(names(.)[-1],"_quintile"))) %>%
  mutate_at(c(2:7), funs((.)-1))

weight.qF <- left_join(weight, weight.q)

cri <- left_join(
  left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            left_join(
              left_join(
                feeder.f,demographics),
              community), economics),
          education),
        family),
      health),
    cdc),
  weight.qF)

# import descriptive stats  and merge
demographics.d <- import("CRI Demographics Buffer Variables Descriptive Stats.csv")
community.d <- import("CRI Community Buffer Variables Descriptive Stats.csv")
economics.d <- import("CRI Economics Buffer Variables Descriptive Stats.csv")
education.d <- import("CRI Education Buffer Variables Descriptive Stats.csv")
family.d <- import("CRI Family Buffer Variables Descriptive Stats.csv")
health.d <- import("CRI Health Buffer Variables Descriptive Stats.csv")
weight <- import("CRI Sub-Indices Weighted and Scaled.csv")

var <-  c("nbr.val",
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
          "var", "std.dev",
          "coef.var")

weight.d <- cbind(var, stat.desc(weight))

cri.d <- left_join(
  left_join(
    left_join(
      left_join(
        left_join(
          left_join(
            demographics.d,
            community.d),
          economics.d),
        education.d),
      family.d),
    health.d),
  weight.d)
