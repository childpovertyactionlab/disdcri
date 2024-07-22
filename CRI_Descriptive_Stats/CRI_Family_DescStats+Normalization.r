source("toolkit.R")

# import 2 mile buffer files for each indicator.
family <- import("CRI Family Buffer Data.csv")

# rename variables where necessary
family.r <- family %>%
  select(-(ends_with("M"))) %>%
  rename(
    fam_hhc = hhcE,
    fam_hhcmh = hhcmhE,
    fam_hhcfh = hhcfhE,
    fam_tphh = tphhE,
    fam_affcc = affcc,
    fam_affcc_cap = affcc_capa,
    fam_lcc = lcc,
    fam_kidpop = kidsE
  ) %>%
  select(
    TEA,
    SLN,
    everything(.),
    -AreaSqMi
  )

# create new indicator variables based on input variables
family.m <- mutate(family.r,
  fam_affcckids = (fam_affcc / fam_kidpop),
  fam_lcckids = (fam_lcc / fam_kidpop),
  fam_tphhpct = (fam_tphh / fam_hhc)
)

# new dataframe only containing indicator variables which need to be summarized
family.i <- family.m %>%
  select(
    TEA,
    SLN,
    fam_affcckids,
    fam_lcckids,
    fam_tphh,
    fam_tphhpct
  )

# descriptive stats for each dataframe
family.d <- cbind(var, stat.desc(family.i))

# shapiro-wilks test of normality on all estimate variables
# will then be filtered to only include variables with a p-value < 0.05 in new df
# must then manually check these new df to see which variables must be log transformed in next step
family.n <- as_tibble(do.call(rbind, lapply(family.i, function(x) shapiro.test(x)[c("statistic", "p.value")])), rownames = "var") %>%
  filter(p.value < 0.05)

# variables that are not normally distributed to sw will be log transformed
# variables that have a '0' value will have '0.0001' added to allow transformation
# variables that are a negative factor to communities will be multiplied by -1 to keep directionality of all variables consistent
# dataframes will then be sorted with identifiers followed by column names alphabetically

### THIS PORTION OF SCRIPT MUST BE REVISED MANUALLY EACH TIME A NEW INDICATOR IS ADDED, CHECK AGAINST 'family.n'###
family.l <- mutate(family.i,
  fam_tphh = log(fam_tphh),
  fam_tphhpct = log(fam_tphhpct),
  fam_affcckids = log(fam_affcckids),
  fam_lcckids = log(fam_lcckids)
) %>%
  select(SLN, TEA, sort(names(.)))

# standardize all variables to z scores using the scale function
family.s <- family.l %>%
  mutate_at(c(3:6), funs(c(scale(.)))) %>%
  rename_all(funs(str_replace(., "fam_", "fi_")))
rm(family.l)

# create 5 quintile bins for each indicator and input variables in the dataframe from the non-normalized/scaled data and renames columns
family.q <- family.i %>%
  mutate_at(c(3:6), funs(c(quin_ = cut(., 5)))) %>%
  setNames(c(names(.)[1], paste0(names(.)[-1], "_quintile"))) %>%
  select(-SLN_quintile) %>%
  mutate_at(c(2:5), funs((.) - 1))

family.qF <- left_join(family.i, family.q)
rm(family.q)

# export non-normalized/scaled data with quintiles to .csv
export(family.qF, "SET AS EXPORT DIRECTORY/CRI Family Buffer Variables Quintiles.csv")

# export normalized buffer dataframes for each indicator to .csv
export(family.s, "SET AS EXPORT DIRECTORY/CRI Family Buffer Variables Normalized.csv")

# export descriptive stats dataframes for each indicator to .csv
export(family.d, "SET AS EXPORT DIRECTORY/CRI Family Buffer Variables Descriptive Stats.csv")
