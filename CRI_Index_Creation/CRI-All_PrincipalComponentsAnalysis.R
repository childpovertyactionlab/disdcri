library(tidyverse)
library(rio)
library(factoextra)
library(scales)

# import indicator files
community <- import("CRI Community - Community Sub-Index.csv")
economics <- import("CRI Economics - Economics Sub-Index.csv")
education <- import("CRI Education - Education Sub-Index.csv")
family <- import("CRI Family - Family Sub-Index.csv")
health <- import("CRI Health - Health Sub-Index.csv")

cri <- left_join(
  left_join(
    left_join(
      left_join(
        community,
        economics
      ),
      education
    ),
    family
  ),
  health
) %>%
  select(TEA, SLN, ci_index, eci_index, edi_index, fi_index, hi_index)

tea_sln <- select(cri, TEA, SLN)

# histograms of each sub-index
ggplot(cri) +
  geom_histogram(aes(ci_index))

ggplot(cri) +
  geom_histogram(aes(eci_index))

ggplot(cri) +
  geom_histogram(aes(edi_index))

ggplot(cri) +
  geom_histogram(aes(fi_index))

ggplot(cri) +
  geom_histogram(aes(hi_index))

# standardize index values with min-max scaling
cri.mm <- cri %>%
  mutate(
    ci_index = ((ci_index - min(ci_index)) / (max(ci_index) - min(ci_index))),
    eci_index = ((eci_index - min(eci_index)) / (max(eci_index) - min(eci_index))),
    edi_index = ((edi_index - min(edi_index)) / (max(edi_index) - min(edi_index))),
    fi_index = ((fi_index - min(fi_index)) / (max(fi_index) - min(fi_index))),
    hi_index = ((hi_index - min(hi_index)) / (max(hi_index) - min(hi_index)))
  )

# histogram of min-max scaled variables
ggplot(cri.mm) +
  geom_histogram(aes(ci_index))

ggplot(cri.mm) +
  geom_histogram(aes(eci_index))

ggplot(cri.mm) +
  geom_histogram(aes(edi_index))

ggplot(cri.mm) +
  geom_histogram(aes(fi_index))

ggplot(cri.mm) +
  geom_histogram(aes(hi_index))

# normalize data with various types of transformations
cri.sk <- cri.mm %>%
  mutate(
    ci_index = ci_index,
    eci_index = sqrt(eci_index),
    edi_index = sqrt(max(edi_index) - edi_index),
    fi_index = sqrt(max(fi_index) - fi_index),
    hi_index = sqrt(hi_index)
  )

################################################################################
# Square Root for Moderate Skew:
## sqrt(x) for positively skewed data,
## sqrt(max(x+1) - x) for negatively skewed data
# Log for Greater Skew:
## log10(x) for positively skewed data,
## log10(max(x+1) - x) for negatively skewed data
# Inverse for Severe Skew
## 1/x for positively skewed data
## 1/(max(x+1) - x) for negatively skewed data
################################################################################

# histogram of normalized skew corrected variables
ggplot(cri.sk) +
  geom_histogram(aes(ci_index))

ggplot(cri.sk) +
  geom_histogram(aes(eci_index))

ggplot(cri.sk) +
  geom_histogram(aes(edi_index))

ggplot(cri.sk) +
  geom_histogram(aes(fi_index))

ggplot(cri.sk) +
  geom_histogram(aes(hi_index))

# perform PCA on cri to evaluate correct dimensions
cri.pca <- prcomp(cri.sk[, c(3:7)], center = TRUE, scale = TRUE) # scale false as normalization has already occurred with min-max normalization in prior section
summary(cri.pca) # proportion of variance is the important indicator, it reveals wcrich PC are the most valued.
fviz_eig(cri.pca) # scree plot visualization depicting what percentage of variance is explained by each principal component.
fviz_pca_var(cri.pca, # plots all variables against first and second dimension to visualize contribution of each variable to PCA.
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
) # Avoid text overlapping

cri.eig <- get_eigenvalue(cri.pca)
cri.eig

# PCA by variable, extract quality of representation (cos2) of each variable
# cos2 will reveal the importance of each variable by dimension for the creation of the sub-index further ahead.
crivar <- get_pca_var(cri.pca)
crivar.name <- rownames(crivar$cos2)
criV.cos <- as_tibble(cbind(crivar.name, crivar$cos2))
rm(crivar.name)
head(criV.cos) # quality of representation (cos2)

# PCA by individual, extract quality of representation (cos2) of each campus
# cos2 will reveal the importance of each school campus by dimension for the creation of the sub-index further ahead.
criind <- get_pca_ind(cri.pca)
criind.cos <- cbind(tea_sln, as_tibble(criind$cos2)) # quality of representation
criI.cos <- left_join(cri, criind.cos)
head(criI.cos)

# cri Sub-Index Creation
summary(cri.pca)
criV.cos
colnames(criI.cos)
cri_sca <- criI.cos %>%
  mutate(cri_index = ((ci_index + eci_index + hi_index + fi_index) * (Dim.1)) + ((edi_index) * Dim.2)) %>%
  select(everything(.), "TEA", "SLN", names(.), -(Dim.1:Dim.5))

#########################################################
## Dim.1 ~57% # ci_index, eci_index, hi_index, fi_index #
## Dim.2 ~18% # edi_index                               #
## Dim.3 ~15%  #                                        #
#########################################################

# OLS on newly created indices
lm1 <- lm(data = cri_sca, cri_index ~ ci_index + eci_index + edi_index + fi_index + hi_index)
summary(lm1)

# Tuning variables based on lm estimates ((1-lmest)*index)
cri_fin <- cri_sca %>%
  mutate(
    ci_weight = (1 - 0.788) * ci_index,
    eci_weight = (1 - 0.661) * eci_index,
    edi_weight = (1 - 0.384) * edi_index,
    fi_weight = (1 - 0.690) * fi_index,
    hi_weight = (1 - 0.733) * hi_index,
    cri_weight = (ci_index + eci_index + edi_index + hi_index + fi_index)
  )

# OLS on newly created indices
lm2 <- lm(data = cri_fin, cri_weight ~ ci_weight + eci_weight + edi_weight + fi_weight + hi_weight)
summary(lm2)

# scale each index to be between 0-100 and round to the nearest whole.
cri_scale <- cri_fin %>%
  mutate_at(c(3:14), funs(c(rescale(., to = c(0, 100), from = range(.))))) %>%
  round()

# histogram of normalized skew corrected variables
ggplot(cri_scale) +
  geom_histogram(aes(cri_weight), bins = 20)
