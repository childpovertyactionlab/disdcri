source("toolkit.R")

#import 2 mile buffer files for each indicator.
education <- import("CRI Education Buffer Data.csv")

#rename variables where necessary
colnames(education)
education.r <- education %>%
  select(-(ends_with("M")), -(ends_with("CV"))) %>%
  rename(edu_popadu = popaduE,
         edu_popkid = kidsE,
         edu_bach = bachE,
         edu_prof = profE,
         edu_k34 = k34E,
         edu_multdeg = multdegE,
         edu_k34es = k34esE,
         edu_qec = qualcc,
         edu_oostct = oostct,
         edu_oostcap = oostcap,
         edu_oostcap_mean = oostcap_me) %>%
  select("TEA", "SLN", everything(.), -"AreaSqMi")
rm(education.j)

#create new indicator variables based on input variables
education.m <- mutate(education.r,
                      edu_perbach = (edu_multdeg+edu_bach)/edu_popadu,
                      edu_perearlyed = (edu_k34es/edu_k34),
                      edu_qeckids = edu_qec/(edu_popkid/10000),
                      edu_oostkids = edu_oostcap/(edu_popkid/10000))
rm(education.r)

#new dataframe only containing indicator variables which need to be summarized
education.i <- select(education.m, "TEA", "SLN", "edu_oostkids", "edu_perbach", "edu_perearlyed", "edu_qeckids")
rm(education.m)

#descriptive stats for each dataframe
var <-  c("nbr.val", "nbr.null", "nbr.na", "min", "max", "range", "sum", "median", "mean", "SE.mean", "CI.mean.0.95", "var", "std.dev", "coef.var")
education.d <- cbind(var, stat.desc(education.i))
rm(var)

#shapiro-wilks test of normality on all estimate variables
#will then be filtered to only include variables with a p-value < 0.05 in new df
#must then manually check these new df to see which variables must be log transformed in next step
economics.n <- as_tibble(do.call(rbind, lapply(education.i, function(x) shapiro.test(x)[c("statistic", "p.value")])), rownames = "var") %>%
  filter(p.value < 0.05)
economics.n

#variables that are not normally distributed to sw will be log transformed
#variables that have a '0' value will have '0.0001' added to allow transformation
#variables that are a negative factor to communities will be multiplied by -1 to keep directionality of all variables consistent
#dataframes will then be sorted with identifiers followed by column names alphabetically

###THIS PORTION OF SCRIPT MUST BE REVISED MANUALLY EACH TIME A NEW INDICATOR IS ADDED, CHECK AGAINST 'education.n'###
education.l <- mutate(education.i,
                      edu_perbach = log(edu_perbach),
                      edu_perearlyed = log(edu_perearlyed),
                      edu_qeckids = log(edu_qeckids+0.0001),
                      edu_oostkids = log(edu_oostkids+0.0001)) %>%
  select("SLN", "TEA", sort(names(.)))

#standardize all variables to z scores using the scale function
#second line of function omits the "SLN", "TEA", and "AreaSqMi" columns from being transformed with the 'scale' function
education.s <- education.l %>%
  mutate_at(c(3:6), funs(c(scale(.)))) %>%
  rename_all(funs(str_replace(.,"edu_", "edi_")))
rm(education.l)

#create 4 quintile bins for each indicator and input variables in the dataframe from the non-normalized/scaled data and renames columns
education.q <- education.i %>%
  mutate_at(c(3:6), funs(c(quin_ = cut(.,5)))) %>%
  setNames(c(names(.)[1], paste0(names(.)[-1],"_quintile"))) %>%
  select(-("SLN_quintile")) %>%
  mutate_at(c(2:5), funs((.)-1))

education.qF <- left_join(education.i, education.q)
rm(education.q)

#export non-normalized/scaled data with quintiles to .csv
export(education.qF, "SET AS EXPORT DIRECTORY/CRI Education Buffer Variables Quintiles.csv")

#export normalized buffer dataframes for each indicator to .csv
export(education.s, "SET AS EXPORT DIRECTORY/CRI Education Buffer Variables Normalized.csv")

#export descriptive stats dataframes for each indicator to .csv
export(education.d, "SET AS EXPORT DIRECTORY/CRI Education Buffer Variables Descriptive Stats.csv")
