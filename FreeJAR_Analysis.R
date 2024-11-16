library(FactoMineR)
library(SensoMineR)
library(dplyr)
library(tidyr)
library(readxl)
library(knitr)
library(FactoMineR)
library(ggplot2)
library(MASS)
library(corrplot)
#library(R4SPISE2022)
library(factoextra)
library(ExPosition)
library(rstatix)
library(ggpubr)
library(car)
library(carData)
library(fmsb)


## Prepare Data of Valency Score
data <- read_excel("C:/Users/LeTuanPhuc/Desktop/FREEJAR/FreeJAR_ML_Processed2.xlsx")
data <- as.data.frame(data)
data <- data[,c(4,5,88)]
data$Jugde <- as.factor(data$Jugde)
data$Product <- as.factor(data$Product)
data$RF_VS

df_wide <- data %>%
    pivot_wider(names_from = Jugde, values_from = RF_VS)
df_wide <- as.data.frame(df_wide)
rownames(df_wide) <- df_wide$Product
df_wide <- df_wide[,-1]

## Plot boxplot of probability for hedonic categories
data1 <- read_excel("C:/Users/LeTuanPhuc/Desktop/FREEJAR/FreeJAR_ML_Processed2.xlsx")
data1 <- as.data.frame(data1)
boxplot(data1[,c(85,86,87)])


## Prepare Data of Contigen table
df2 <- read_excel("C:/Users/LeTuanPhuc/Desktop/FREEJAR/FreeJAR_ML_Processed_Total.xlsx")
df2 <- as.data.frame(df2)
row.names(df2) <- df2$...1
df2 <- df2[,-1]
df2$total_count <- rowSums(df2)
df2$percentage <- df2$total_count*100/(88*7)

##### Choose the attribute have frequency mentioned greater than 5%
df3 <- df2[df2$percentage>5,]
df3 <- t(df3[,-c(8,9)])


# Combine data by rowname
combined_df <- merge(df_wide, df3, by = "row.names", all = TRUE)
row.names(combined_df) <- combined_df$Row.names
combined_df$Row.names <- NULL

## Plot Preference Mapping
res.pca <- PCA(combined_df[,c(89:103)], scale.unit = FALSE)
fviz_pca_biplot(res.pca)
fviz_pca_var(res.pca)
res.carto <- carto(res.pca$ind$coord[,1:2], combined_df[,c(1:88)])

#### Try it with Correspondence Analysis
res.ca <- CA(combined_df[,c(89:103)], graph = FALSE)
res.carto <- carto(res.ca$row$coord[,1:2], combined_df[,c(1:88)], regmod = 2)
fviz_ca_biplot(res.ca, repel = TRUE)



## Internal Preference Mapping and Cluster consumer
dat <- combined_df[,c(1:88)]
res.pca1 <- PCA(dat)
library(ClustVarLV)
res.seg <- CLV(X = dat, method = "local")
plot(res.seg, "dendrogram")
table(get_partition(res.seg, K = 3))
plot_var(res.seg, K = 3, v_symbol = F, label = T, beside = T)
plot(res.seg,type="delta")
summary(res.seg, K = 3)
cov(dat, use = "pairwise.complete.obs", method = c("pearson"))
comp <- get_comp(res.seg, K = 3)
plot_var(res.seg, K = 3, v_colors = c('red','blue','green'))



