source("toolkit.R")

# import indicator files
education <- import("CRI Education Buffer Variables Normalized.csv")
tea_sln <- select(education, "TEA", "SLN")

# perform PCA on health to evaluate correct dimensions
edu.pca <- prcomp(education[, c(3:6)], center = TRUE, scale = FALSE) # scale false as normalization has already occured
summary(edu.pca) # proportion of variance is the important indicator, it reveals which PC are the most valued.
fviz_eig(edu.pca) # scree plot visualization depicting what percentage of variance is explained by each principal component.
fviz_pca_var(edu.pca, # plots all variables against first and sedud dimension to visualize contribution of each variable to PCA.
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
) # Avoid text overlapping

edu.eig <- get_eigenvalue(edu.pca)

# PCA by variable, extract quality of representation (cos2) of each variable
# cos2 will reveal the importance of each variable by dimension for the creation of the sub-index further ahead.
eduvar <- get_pca_var(edu.pca)
eduvar.name <- rownames(eduvar$cos2)
eduV.cos <- as_tibble(cbind(eduvar.name, eduvar$cos2))
rm(eduvar.name)

# PCA by individual, extract quality of representation (cos2) of each campus
# cos2 will reveal the importance of each school campus by dimension for the creation of the sub-index further ahead.
eduind <- get_pca_ind(edu.pca)
eduind.cos <- cbind(tea_sln, as_tibble(eduind$cos2)) # coordinates
eduI.cos <- left_join(education, eduind.cos)

# Education Sub-Index Creation
summary(edu.pca)
view(eduI.cos)
edi_fin <- eduI.cos %>%
  mutate(edi_index = ((edi_perbach + edi_perearlyed) * (Dim.1)) + (edi_qeckids * Dim.2) + (edi_oostkids * Dim.3)) %>%
  select(everything(.), TEA, SLN, names(.), -(Dim.1:Dim.4))

##################################################
## Dim.1 ~42% # edi_perbach, edi_perearlyed      #
## Dim.2 ~26% # edi_qeckids                      #
## Dim.3 ~23% # edi_oostkids                     #
## Dim.4 ~7%  #                                  #
##################################################

# export tables
export(eduI.cos, "SET AS EXPORT DIRECTORY/CRI Education - PCA Indiviual Cos.csv")
export(eduV.cos, "SET AS EXPORT DIRECTORY/CRI Education - PCA Variable Cos.csv")
export(edi_fin, "SET AS EXPORT DIRECTORY/CRI Education - Education Sub-Index.csv")
