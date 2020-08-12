rm(list=ls(all=TRUE))
setwd("SET AS WORKING DIRECTORY OF DOWNLOADED FILE LOCATIONS")
library(tidyverse)
library(rio)
library(factoextra)

#import indicator files
economics <- import("CRI Economics Buffer Variables Normalized.csv")
tea_sln <- select(economics, "TEA", "SLN")

#perform PCA on health to evaluate correct dimensions
econ.pca <- prcomp(economics[,c(3:11)], center = TRUE, scale = FALSE)  #scale false as normalization has already occured
summary(econ.pca)                                                      #proportion of variance is the important indicator, it reveals which PC are the most valued.
fviz_eig(econ.pca)                                                     #scree plot visualization depicting what percentage of variance is explained by each principal component.
fviz_pca_var(econ.pca,                                                 #plots all variables against first and second dimension to visualize contribution of each variable to PCA.
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

econ.eig <- get_eigenvalue(econ.pca)
econ.eig

#PCA by variable, extract quality of representation (cos2) of each variable
#cos2 will reveal the importance of each variable by dimension for the creation of the sub-index further ahead.
econvar <- get_pca_var(econ.pca)
econvar.name <- rownames(econvar$cos2)
econV.cos <- as_tibble(cbind(econvar.name, econvar$cos2))   
rm(econvar.name)

#PCA by individual, extract quality of representation (cos2) of each campus
#cos2 will reveal the importance of each school campus by dimension for the creation of the sub-index further ahead.
econind <- get_pca_ind(econ.pca)
econind.cos <- cbind(tea_sln, as_tibble(econind$cos2))     #coordinates
econI.cos <- left_join(economics, econind.cos)

#Economics Sub-Index Creation
summary(econ.pca)
view(econI.cos)
econV.cos
colnames(econI.cos)
eci_fin <- econI.cos %>%
  mutate(eci_index = ((eci_cpr+eci_medinc+eci_pctmwjobs+eci_pr+eci_pyr+eci_ur)*(Dim.1))+((eci_fincap+eci_paydaycap)*(Dim.2))+(eci_pctlwjobs*Dim.3)) %>%
  select(everything(.), "TEA", "SLN", names(.), -(Dim.1:Dim.9))

#                    #################################################
#                    # Dim.1 ~46% # eci_cpr, eci_medinc, eci_ur      #
#                    #            # eci_pctmwjobs, eci_pr, eci_pyr   #
#                    # Dim.2 ~20% # eci_fincap, eci_paydaycap        #
#                    # Dim.3 ~15% # eci_pctlwjobs                    #
#                    # Dim.4 ~9%  #                                  #
#                    #################################################

#export tables
export(econI.cos, "SET AS EXPORT DIRECTORY/CRI Economics - PCA Indiviual Cos.csv")
export(econV.cos, "SET AS EXPORT DIRECTORY/CRI Economics - PCA Variable Cos.csv")
export(eci_fin, "SET AS EXPORT DIRECTORY/CRI Economics - Economics Sub-Index.csv")
