source("toolkit.R")

# import 2 mile buffer files for each indicator.
health <- import("CRI Health Buffer Data.csv")

# rename variables where necessary
health.r <- health %>%
  select(-ends_with("M")) %>%
  rename(
    hel_totalpop = totalpopE,
    hel_popunin = popunisE,
    hel_popins = popinsE,
    hel_poppri = priE,
    hel_poppub = pub,
    hel_bphigh = bphighE,
    hel_checkup = checkupE,
    hel_mhlth = SUM_mhlthE,
    hel_obesity = obseityE,
    hel_phlth = phlthE,
    hel_sleep = sleepE,
    hel_stroke = strokeE,
    hel_pop18 = POP,
    hel_le = MEAN_hel_l,
    hel_leSD = STD_hel_le,
    hel_clin = clinicct,
    hel_groc = groc,
    hel_fruitavg = avgfruits,
    hel_vegavg = avgveg,
    hel_pharm = pharm,
    hel_castthma = casthmaE
  ) %>%
  select(
    TEA,
    SLN,
    AreaSqMi,
    SCHOOLNAME,
    everything(.)
  )

# create new indicator variables based on input variables
health.m <- mutate(health.r,
  hel_perins = hel_popins / hel_totalpop,
  hel_perunin = hel_popunin / hel_totalpop,
  hel_perpri = hel_poppri / hel_totalpop,
  hel_perpub = hel_poppub / hel_totalpop,
  hel_prtopu = hel_perpri / hel_perpub,
  hel_bphigh = hel_bphigh / (hel_pop18 / 10000),
  hel_castthma = hel_castthma / (hel_pop18 / 10000),
  hel_checkup = hel_checkup / (hel_pop18 / 10000),
  hel_mhlth = hel_mhlth / (hel_pop18 / 10000),
  hel_obesity = hel_obesity / (hel_pop18 / 10000),
  hel_phlth = hel_phlth / (hel_pop18 / 10000),
  hel_sleep = hel_sleep / (hel_pop18 / 10000),
  hel_stroke = hel_stroke / (hel_pop18 / 10000),
  hel_fruitsveggies = hel_fruitavg + hel_vegavg,
  hel_pharmacap = hel_pharm / (hel_pop18 / 10000),
  hel_groccap = hel_groc / (hel_pop18 / 10000),
  hel_clincap = hel_clin / (hel_pop18 / 10000)
)

# new dataframe only containing indicator variables which need to be summarized
health.i <- health.m %>%
  select(
    TEA,
    SLN,
    hel_bphigh,
    hel_castthma,
    hel_checkup,
    hel_fruitsveggies,
    hel_le,
    hel_mhlth,
    hel_obesity,
    hel_perpri,
    hel_perpub,
    hel_prtopu,
    hel_pharmacap,
    hel_phlth,
    hel_sleep,
    hel_stroke,
    hel_perins,
    hel_perunin,
    hel_clincap,
    hel_groccap
  )

# descriptive stats for each dataframe
health.d <- cbind(desc_var, stat.desc(health.i))

# shapiro-wilks test of normality on all estimate variables
# only includes variables with a p-value < 0.05 in new df
# manually check to see which variables must be log transformed in next step
health.n <- as_tibble(
  do.call(
    rbind,
    lapply(health.i, function(x) shapiro.test(x)[c("statistic", "p.value")])
  ),
  rownames = "desc_var"
) %>%
  filter(p.value < 0.05)

# variables that are not normally distributed to sw will be log transformed
# variables that have a '0' value will have '0.0001' added to allow transformation
# variables that are a negative factor to communities will be multiplied by -1 to keep directionality of all variables consistent
# dataframes will then be sorted with identifiers followed by column names alphabetically

### THIS PORTION OF SCRIPT MUST BE REVISED MANUALLY EACH TIME A NEW INDICATOR IS ADDED, CHECK AGAINST 'health.n'###
health.l <- mutate(health.i,
  hel_bphigh = (log(hel_bphigh)) * (-1),
  hel_checkup = log(hel_checkup),
  hel_mhlth = (log(hel_mhlth)) * (-1),
  hel_obesity = (log(hel_obesity)) * (-1),
  hel_phlth = (log(hel_phlth)) * (-1),
  hel_sleep = log(hel_sleep),
  hel_stroke = (log(hel_stroke)) * (-1),
  hel_le = log(hel_le),
  hel_castthma = (log(hel_castthma)) * (-1),
  hel_perins = log(hel_perins),
  hel_perunin = (log(hel_perunin)) * (-1),
  hel_perpri = log(hel_perpri),
  hel_perpub = log(hel_perpub),
  hel_fruitsveggies = log(hel_fruitsveggies),
  hel_pharmacap = log(hel_pharmacap + 0.0001)
) %>%
  select(SLN, TEA, sort(names(.)))

# standardize all variables to z scores using the scale function
health.s <- health.l %>%
  mutate_at(c(3:20), funs(c(scale(.)))) %>%
  rename_all(funs(str_replace(., "hel_", "hi_")))

# create 5 quintile bins for each indicator and input variables in the dataframe from the non-normalized/scaled data and renames columns
health.q <- health.i %>%
  mutate_at(c(3:20), funs(c(quin_ = cut(., 5)))) %>%
  setNames(c(names(.)[1], paste0(names(.)[-1], "_quintile"))) %>%
  select(-("SLN_quintile")) %>%
  mutate_at(c(2:19), funs((.) - 1))

health.qF <- left_join(health.i, health.q)

# export non-normalized/scaled data with quintiles to .csv
export(health.qF, "SET AS EXPORT DIRECTORY/CRI Health Buffer Variables Quintiles.csv")

# export normalized buffer dataframes for each indicator to .csv
export(health.s, "SET AS EXPORT DIRECTORY/CRI Health Buffer Variables Normalized.csv")

# export descriptive stats dataframes for each indicator to .csv
export(health.d, "SET AS EXPORT DIRECTORY/CRI Health Buffer Variables Descriptive Stats.csv")
