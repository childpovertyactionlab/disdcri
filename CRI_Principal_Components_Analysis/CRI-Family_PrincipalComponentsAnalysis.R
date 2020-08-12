rm(list=ls(all=TRUE))
setwd("SET AS WORKING DIRECTORY OF DOWNLOADED FILE LOCATIONS")
library(tidyverse)
library(rio)
library(factoextra)

#import indicator files
family <- import("CRI family Buffer Variables Normalized.csv")
tea_sln <- select(family, "TEA", "SLN")

#perform PCA on health to evaluate correct dimensions
fam.pca <- prcomp(family[,c(3:6)], center = TRUE, scale = FALSE)  #scale false as normalization has already occured
summary(fam.pca)                                                      #proportion of variance is the important indicator, it reveals which PC are the most valued.
fviz_eig(fam.pca)                                                     #scree plot visualization depicting what percentage of variance is explained by each principal component.
fviz_pca_var(fam.pca,                                                 #plots all variables against first and sfamd dimension to visualize contribution of each variable to PCA.
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

fam.eig <- get_eigenvalue(fam.pca)
fam.eig

#PCA by variable, extract quality of representation (cos2) of each variable
#cos2 will reveal the importance of each variable by dimension for the creation of the sub-index further ahead.
famvar <- get_pca_var(fam.pca)
famvar.name <- rownames(famvar$cos2)
famV.cos <- as_tibble(cbind(famvar.name, famvar$cos2))   
rm(famvar.name)

#PCA by individual, extract quality of representation (cos2) of each campus
#cos2 will reveal the importance of each school campus by dimension for the creation of the sub-index further ahead.
famind <- get_pca_ind(fam.pca)
famind.cos <- cbind(tea_sln, as_tibble(famind$cos2))     #coordinates
famI.cos <- left_join(family, famind.cos)

#family Sub-Index Creation
summary(fam.pca)
view(famI.cos)
famV.cos
colnames(famI.cos)
fi_fin <- famI.cos %>%
  mutate(fi_index = ((fi_affcckids+fi_tphhpct+fi_tphh)*(Dim.1))+(fi_lcckids*Dim.2)) %>%
  select(everything(.), "TEA", "SLN", names(.), -(Dim.1:Dim.4))

#                      #################################################
#                      # Dim.1 ~61% # fi_affcckids, fi_tphhpct, fi_tphh#
#                      # Dim.2 ~24% # fi_lcckids                       #
#                      # Dim.3 ~10% #                                  #
#                      # Dim.4 ~3%  #                                  #
#                      #################################################

#export tables
export(famI.cos, "SET AS EXPORT DIRECTORY/CRI family - PCA Indiviual Cos.csv")
export(famV.cos, "SET AS EXPORT DIRECTORY/CRI family - PCA Variable Cos.csv")
export(fi_fin, "SET AS EXPORT DIRECTORY/CRI family - Family Sub-Index.csv")

