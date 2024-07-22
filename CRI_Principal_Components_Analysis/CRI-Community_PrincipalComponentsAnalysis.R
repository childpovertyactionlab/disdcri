source("toolkit.R")

# import indicator files
community <- import("CRI Community Buffer Variables Normalized.csv")
tea_sln <- select(community, "TEA", "SLN")

# perform PCA on health to evaluate correct dimensions
comm.pca <- prcomp(community[, c(3:18)], center = TRUE, scale = FALSE) # scale false as normalization has already occured
summary(comm.pca) # proportion of variance is the important indicator, it reveals which PC are the most valued.
fviz_eig(comm.pca) # scree plot visualization depicting what percentage of variance is explained by each principal component.
fviz_pca_var(comm.pca, # plots all variables against first and scommd dimension to visualize contribution of each variable to PCA.
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
) # Avoid text overlapping

comm.eig <- get_eigenvalue(comm.pca)


# PCA by variable, extract quality of representation (cos2) of each variable
# cos2 will reveal the importance of each variable by dimension for the creation of the sub-index further ahead.
commvar <- get_pca_var(comm.pca)
commvar.name <- rownames(commvar$cos2)
commV.cos <- as_tibble(cbind(commvar.name, commvar$cos2))

# PCA by individual, extract quality of representation (cos2) of each campus
# cos2 will reveal the importance of each school campus by dimension for the creation of the sub-index further ahead.
commind <- get_pca_ind(comm.pca)
commind.cos <- cbind(tea_sln, as_tibble(commind$cos2)) # coordinates
commI.cos <- left_join(community, commind.cos)

# community Sub-Index Creation
summary(comm.pca)

ci_fin <- commI.cos %>%
  mutate(ci_index = ((ci_bbp + ci_evrate + ci_incarpct + ci_ltrvp + ci_ocbp + ci_parkcap + ci_rvp + ci_u18bbp) * (Dim.1)) + ((ci_bvp + ci_ltbvp + ci_ocb + ci_rcb + ci_rcbp) * (Dim.2)) + ((ci_cctrcap + ci_juvcrimecap + ci_libcap) * (Dim.3))) %>%
  select(everything(.), "TEA", "SLN", names(.), -(Dim.1:Dim.16))

# OWEN I'm unsure about ocb/rcb and ocbp/rcbp being included in the index
# but the eigenvalue plot earlier in the PCA shows them in pretty disparate areas,
# your input would be appreciated.

###########################################################
## Dim.1 ~34% # ci_bbp, ci_evrate, ci_incarpct, ci_ltrvp  #
##            # ci_ocbp, ci_parkcap, ci_rvp, ci_u18bbp    #
## Dim.2 ~18% # ci_bvp, ci_ltbvp, ci_ocb, ci_rcb, ci_rcbp #
## Dim.3 ~13% # ci_cctrcap, ci_juvcrimecap, ci_libcap     #
## Dim.4 ~9%  #                                           #
###########################################################

# export tables
export(commI.cos, "SET AS EXPORT DIRECTORY/CRI Community - PCA Indiviual Cos.csv")
export(commV.cos, "SET AS EXPORT DIRECTORY/CRI Community - PCA Variable Cos.csv")
export(ci_fin, "SET AS EXPORT DIRECTORY/CRI Community - Community Sub-Index.csv")
