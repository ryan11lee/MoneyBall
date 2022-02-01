setwd("~/code/MoneyBall")
library(devtools)
install_github("vqv/ggbiplot")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("reshape2")
install.packages("RColorBrewer")
install.packages("naniar")
install.packages("mice")
install.packages("readr")
install.packages("GGally")
install.packages("mlbench")
install.packages("Rtsne")
install.packages("data.table")
install.packages("ggrepel")
install.packages("cluster")
install.packages("maptree")
install.packages("ggridges")        
install.packages("devtools")
install.packages("ggbiplot")
install.packages("factoextra")
install.packages("corrplot")

library(tidyverse)
library(reshape2)
library(RColorBrewer)
library(ggpubr)
library(naniar)
library(mice)
library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(dplyr)
library(mlbench)
library(Rtsne)
library(data.table)
library(ggrepel)
library(cluster)
library(maptree)
library(ggridges)        
library(devtools)
library(ggbiplot)
library(factoextra)
library(corrplot)


my.data <- read_csv("MoneyBall.csv")

#visualize boxplots of all variables, we see a large amount of outliers
my.data %>% 
  select(-INDEX) %>% 
  gather(key = "Parameter", value) %>% 
  ggplot(aes(factor(Parameter), value)) +
  geom_boxplot() + 
  facet_wrap(~Parameter, scale="free")


#### COunt and visualize missing data

MissingProportions <- my.data %>%
  select(-INDEX) %>%
  mutate_all(is.na) %>%
  summarise_all(sum) %>%
  reshape2::melt() %>%
  mutate(value = value / nrow(my.data)*100) %>%
  filter(value > 0)


vis_miss(my.data)

gg_miss_var(my.data, show_pct = TRUE) + labs(y = "Missing % of Data")

nb.cols <- length(my.data)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)

#data cleanup and imputation

removed.data <-my.data %>% 
  select(-c("TEAM_BATTING_HBP","INDEX"))

tempData <- mice(removed.data,m=5,maxit=50,meth='sample',seed=500)

densityplot(tempData)
summary(tempData)
completedData <- mice::complete(tempData,1)

glimpse(completedData)

corrs<-cor(completedData)
corrplot(corrs, method="circle", type = 'upper')

#normalize data 

library(caret)
preproc1 <- preProcess(completedData, method=c("center", "scale"))
norm1 <- predict(preproc1, completedData)



# compare the histograms of the data pre and post normalization
my.data[,-1] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


norm1 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# PCA Analysis to determine relationships in the data, also comparing raw vs imputed daa to ensure
# relationships between data is not compromised
returns.pca <- princomp(x=completedData,cor=TRUE)
names(returns.pca)

plot(returns.pca)

pc.1 <- returns.pca$loadings[,1]
pc.2 <- returns.pca$loadings[,2]

factoextra::fviz_pca_biplot(returns.pca,geom = c(arrow))


normed.pca <- princomp(x=completedData,cor=TRUE)

names(normed.pca)

plot(normed.pca)

pc.norm.1 <- normed.pca$loadings[,1]
pc.norm.2 <- normed.pca$loadings[,2]
pc.norm.3 <- normed.pca$loadings[,3]
pc.norm.4 <- normed.pca$loadings[,4]
pc.norm.5 <- normed.pca$loadings[,5]


norm.PCA <- as.data.frame(cbind(pc.norm.1,pc.norm.2,pc.norm.3,pc.norm.4,pc.norm.5 ))
norm.PCA <- tibble::rownames_to_column(norm.PCA, "VALUE")


ggplot(norm.PCA, aes(label = VALUE)) +
  geom_point(aes(x= pc.norm.1,y= pc.norm.2)) +
  geom_text(aes(x= pc.norm.1,y= pc.norm.2,label= VALUE)) +
  xlim(-0.5,.5)

ggplot(norm.PCA, aes(label = VALUE)) +
  geom_point(aes(x= pc.norm.1,y= pc.norm.3)) +
  geom_text(aes(x= pc.norm.1,y= pc.norm.3,label= VALUE)) +
  xlim(-0.5,.5)
   

factoextra::fviz_pca_biplot(normed.pca,geom = c(arrow)) +
  labs(title = "Principal Component")         
     

#We see the data relationships are maintained in the imputed data set giving us confidence in the new data.

# Make Scree Plot
scree.values <- (normed.pca$sdev^2)/sum(normed.pca$sdev^2)
plot(scree.values,xlab='Number of Components',ylab='',type='l',lwd=2) 
points(scree.values,lwd=2,cex=1.5)
title('Scree Plot')
abline(h=0.0685,lwd=1.5,col='red')
abline(v=5,lwd=1.5,col='red')
# Make Proportion of Variance Explained
variance.values <- cumsum(normed.pca$sdev^2)/sum(normed.pca$sdev^2)
plot(variance.values ,xlab='Number of Components',ylab='',type='l',lwd=2) 
points(variance.values,lwd=2,cex=1.5)
abline(h=0.760,lwd=1.5,col='red')
abline(v=5,lwd=1.5,col='red')
text(4,.5,'Keep 5 Principal Components',col='red') 
title('Total Variance Explained Plot')







clusterresults$cluster

norm.PCA.Cluster <- cbind(norm.PCA,clusterresults$cluster)

fviz_nbclust(norm.PCA[,-1], kmeans, method = "wss")

# Perform Kmeans on the first 5 principal components selecting 5-K's
clusterresults <- kmeans(norm.PCA[,2:6],4)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss

clusplot(norm.PCA, clusterresults$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)


####
wins <- completedData$TARGET_WINS
kmeans.data <- completedData %>% 
  select(-TARGET_WINS)

fviz_nbclust(kmeans.data, kmeans, method = "wss")

clusterresults <- kmeans(kmeans.data,3)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss


clusplot(kmeans.data, clusterresults$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)



kmeans.data <- completedData %>% 
  mutate(high.med.low = ifelse(TARGET_WINS <= 71,"Low", 
                               ifelse(TARGET_WINS > 71 & TARGET_WINS <=92,"Medium", 
                                      ifelse(TARGET_WINS>92,"High",NA))))

high.low <- kmeans.data$high.med.low
kmeans.data <- kmeans.data %>% 
  select(-c("high.med.low", "TARGET_WINS"))

# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(kmeans.data), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster
# Dimension reduction using PCA
res.pca <- prcomp(kmeans.data,  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$wins <- high.low
# Data inspection
head(ind.coord)

eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "wins", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)

# As we see in the data despite having distinct clusters we do not see a seperation of wins within this data.
#######################

tsne_vis <- function( perplexity = 1, learning = 200, iterations = 500, cor) {
  
  set.seed(1)
  tsne <- Rtsne(cor, dims = 2, perplexity=perplexity, verbose =TRUE, max_iter = iterations, learning = learning)
}

corrs <-corrs[-1,-1]

tsne.results<- tsne_vis(perplexity = 1, learning = 500, iterations = 10000, corrs)
tsne.results <- data.table(x = tsne.results$Y[,1], y = tsne.results$Y[,2], Symbol = row.names(corrs))

ggplot(tsne.results, aes(x, y)) +
  geom_point() +
  geom_text_repel(aes(label = Symbol),
                  size = 4, box.padding = 1.5,
                  segment.size  = 0.2, segment.color = "grey50") +
  labs(title = paste("t-SNE: Perplexity=", 1, " Learning=", 500, "Iterations=", 10000 ), x = "tSNE dimension 1", y = "tSNE dimension 2")
  


###########


completedData$u <- runif(n = nrow(completedData), min = 0, max = 1)
split.ratio <- .7

data.train <- completedData %>% 
  filter( u < split.ratio)


data.test <- completedData %>% 
  filter( u >= split.ratio)


clus1.lm <- lm(TARGET_WINS ~ TEAM_BATTING_2B+TEAM_PITCHING_BB+TEAM_PITCHING_SO+TEAM_FIELDING_DP , data=data.train)

clus2.lm <- lm(TARGET_WINS ~ TEAM_BATTING_3B+TEAM_BASERUN_SB+TEAM_BASERUN_CS+TEAM_FIELDING_E , data=data.train)

clus3.lm <- lm(TARGET_WINS ~ TEAM_BATTING_HR+TEAM_BATTING_BB+TEAM_BATTING_SO+TEAM_PITCHING_HR , data=data.train)

clus4.lm <- lm(TARGET_WINS ~ TEAM_BATTING_H+TEAM_PITCHING_H , data=data.train)



value.test <- data.test  %>% 
select(-u) %>% 
  select(-TARGET_WINS)



data.test$Pred <- round(predict(clus1.lm, newdata = value.test))
plot(data.test$Pred,data.test$TARGET_WINS)

#Set threshold within 10 wins to determine accuracy of linear model
data.test$correct <- ifelse(abs(data.test$TARGET_WINS -data.test$Pred)<10,1,0)
sum(data.test$correct)/nrow(data.test)
