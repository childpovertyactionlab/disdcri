rm(list=ls(all=TRUE))
setwd("SET AS WORKING DIRECTORY OF DOWNLOADED FILE LOCATIONS")
library(tidyverse)
library(rio)
library(factoextra)

#import indicator files
health <- import("CRI Health Buffer Variables Normalized.csv")
health.ghi <- import("CRI Health CDC500 - General Health Index.csv")
tea_sln <- select(health, "TEA", "SLN")

colnames(health)
#Plot of Private vs Public Insurance
ggplot(health) + 
  geom_point(aes(x = hi_perpri, y = hi_perpub)) +
  geom_smooth(aes(x = hi_perpri, y = hi_perpub))

ggplot(health) + 
  geom_histogram(aes(hi_prtopu))

#New selection of variables to conduct PCA on
health.in <- left_join(health, health.ghi) %>%
  select("TEA", "SLN", "hi_clincap", "hi_fruitsveggies", "hi_groccap", "hi_le", "hi_perunin", "hi_pharmacap", "hi_prtopu", "hi_mhlth", "hi_phlth", "hi_gen")

#perform PCA on health to evaluate correct dimensions
hel.pca <- prcomp(health.in[,c(3:12)], center = TRUE, scale = FALSE) #scale false as normalization has already occured
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
helI.cos <- left_join(health.in, helind.cos)
head(helI.cos)

#Health Sub-Index Creation
summary(hel.pca)
helV.cos
colnames(helI.cos)
hi_fin <- helI.cos %>%
  mutate(hi_index = ((hi_prtopu+hi_perunin+hi_mhlth+hi_phlth+hi_gen)*(Dim.1))+ ((hi_clincap+hi_groccap+hi_le+hi_pharmacap)*(Dim.2))+(hi_fruitsveggies*Dim.3)) %>%
  select(everything(.), "TEA", "SLN", names(.), -(Dim.1:Dim.10))

#Dropped perins variable and kept per unins instead for PCA

#                      ###########################################
#                      # Dim.1 ~62% # hi_prtopu, hi_perunin      #
#                      #            # hi_mhlth, hi_phlth, hi_gen #
#                      # Dim.2 ~16% # hi_clincap, hi_groccap,    #
#                      #            # hi_le, hi_pharmacap        #
#                      # Dim.3 ~7%  # hi_fruitsveggies           #
#                      # Dim.4 ~5%  #                            #
#                      ###########################################

#export tables
export(helI.cos, "SET AS EXPORT DIRECTORY/CRI Health - PCA Indiviual Cos.csv")
export(helV.cos, "SET AS EXPORT DIRECTORY/CRI Health - PCA Variable Cos.csv")
export(hi_fin, "SET AS EXPORT DIRECTORY/CRI Health - Health Sub-Index.csv")
