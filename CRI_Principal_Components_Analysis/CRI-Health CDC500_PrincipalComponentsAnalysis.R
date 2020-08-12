rm(list=ls(all=TRUE))
setwd("SET AS WORKING DIRECTORY OF DOWNLOADED FILE LOCATIONS")
library(tidyverse)
library(rio)
library(factoextra)
library(scales)

#import indicator files
health <- import("CRI Health Buffer Variables Normalized.csv")
colnames(health)
health.cdc <- select(health, "TEA", "SLN", "hi_bphigh", "hi_castthma", "hi_checkup", "hi_obesity", "hi_sleep", "hi_stroke") #"hi_mhlth", "hi_phlth"
tea_sln <- select(health, "TEA", "SLN")

#perform PCA on health to evaluate correct dimensions
hel.pca <- prcomp(health.cdc[,c(3:8)], center = TRUE, scale = FALSE) #scale false as normalization has already occured
summary(hel.pca)                                                      #proportion of variance is the important indicator, it reveals which PC are the most valued.
fviz_eig(hel.pca)                                                     #scree plot visualization depicting what percentage of variance is explained by each principal component.
fviz_pca_var(hel.pca,                                                 #plots all variables against first and second dimension to visualize contribution of each variable to PCA.
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

hel.eig <- get_eigenvalue(hel.pca)
hel.eig

#PCA by variable, extract quality of representation (cos2) of each variable
#cos2 will reveal the importance of each variable by dimension for the creation of the sub-index further ahead.
helvar <- get_pca_var(hel.pca)
helvar.name <- rownames(helvar$cos2)
helV.cos <- as_tibble(cbind(helvar.name, helvar$cos2))
rm(helvar.name)
head(helV.cos)                                                              #quality of representation (cos2)

#PCA by individual, extract quality of representation (cos2) of each campus
#cos2 will reveal the importance of each school campus by dimension for the creation of the sub-index further ahead.
helind <- get_pca_ind(hel.pca)
helind.cos <- cbind(tea_sln, as_tibble(helind$cos2))   #quality of representation
helI.cos <- left_join(health.cdc, helind.cos)
head(helI.cos)

#General Health Index calculated from CDC500 variables.
#CDC500 variables are all highly correlated to one another and in order to prevent them from affecting other variables in a health index a general health index was calculated.
#all variables in this index are calculated based on Dim.1 of the PCA as all variables are above 75% in Dim.1 whereas Dim.2 and beyond variables are seen with under 10%
summary(hel.pca)
helV.cos
colnames(helI.cos)

health.ghi <- helI.cos %>%
  mutate(hi_gen = (hi_bphigh+hi_castthma+hi_checkup+hi_obesity+hi_sleep+hi_stroke)*(Dim.1)) %>%
  select("TEA", "SLN", "hi_gen")

health.fin <- health.ghi %>%
  mutate(hi_genweight = round(rescale(hi_gen, to = c(0, 100), from = range(hi_gen))))
  
#                      #################################################
#                      # Dim.1 ~92% # ALL VARIABLES                    #
#                      # Dim.2 ~5%  #                                  #
#                      # Dim.3 ~2%  #                                  #
#                      # Dim.4 ~0%  #                                  #
#                      #################################################


#export tables
export(helI.cos, "SET AS EXPORT DIRECTORY/CRI Health CDC500 - PCA Indiviual Cos.csv")
export(helV.cos, "SET AS EXPORT DIRECTORY/CRI Health CDC500 - PCA Variable Cos.csv")
export(health.fin, "SET AS EXPORT DIRECTORY/CRI Health CDC500 - General Health Index.csv")
