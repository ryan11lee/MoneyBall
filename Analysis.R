my.data <- read.csv("MoneyBall.csv")

library(ggplot2)
my.data %>% 
  select(-INDEX) %>% 
  gather(key = "Paramter", value) %>% 
  ggplot(aes(factor(Paramter), value)) +
  geom_boxplot() + 
  facet_wrap(~Paramter, scale="free")


my_data2 <- gather(my_data,
                   key = "arrest_attribute",
                   value = "arrest_estimate",
                   -state)

install.packages("dplyr")
library(tidyverse)
library(reshape2)
library(RColorBrewer)


MissingCount <- my.data %>%
  select(-INDEX) %>%
  mutate_all(is.na) %>%
  summarise_all(sum) %>%
  melt() %>%
  mutate(value = value / nrow(my.data)) %>%
  filter(value > 0) %>%
  rename("Missing_Fraction" = value)

library(naniar)
vis_miss(my.data)

library(ggplot2)
gg_miss_var(my.data, show_pct = TRUE) + labs(y = "Missing % of Data")

library(RColorBrewer)
nb.cols <- length(my.data)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)


library(mice)
droppeddata <-my.data %>% select(-TEAM_BATTING_HBP)
droppeddata <-my.data %>% select(-INDEX)



#tempData <- mice(droppeddata,m=5,maxit=50,meth='2l.bin',seed=500) 
tempData <- mice(droppeddata,m=5,maxit=50,meth='sample',seed=500)

densityplot(tempData)
summary(tempData)
completedData <- complete(tempData,1)

library(corrplot)
corrs<-cor(completedData)
corrplot(corrs, method="circle", type = 'upper')

returns.pca <- princomp(x=completedData,cor=TRUE)
# See the output components returned by princomp();
names(returns.pca)

plot(returns.pca)

pc.1 <- returns.pca$loadings[,1]
pc.2 <- returns.pca$loadings[,2]


library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(dplyr)
library(mlbench)

glimpse(completedData)

library(caret)
completedData <- completedData
preproc1 <- preProcess(completedData, method=c("center", "scale"))
norm1 <- predict(preproc1, completedData)

my.data[,-1] %>%
  select(TEAM_BATTING_HBP, -height)(TEAM_BATTING_HBP)
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

normed.pca <- princomp(x=completedData,cor=TRUE)

names(normed.pca)

plot(normed.pca)

summary(normed.pca))

pc.norm.1 <- normed.pca$loadings[,1]
pc.norm.2 <- normed.pca$loadings[,2]
pc.norm.3 <- normed.pca$loadings[,3]


norm.PCA <- as.data.frame(cbind(pc.norm.1,pc.norm.2,pc.norm.3 ))
norm.PCA <- tibble::rownames_to_column(norm.PCA, "VALUE")


ggplot(norm.PCA, aes(label = VALUE)) +
  geom_point(aes(x= pc.norm.1,y= pc.norm.2)) +
  geom_text(aes(x= pc.norm.1,y= pc.norm.2,label= VALUE)) +
  xlim(-0.5,.5)

ggplot(norm.PCA, aes(label = VALUE)) +
  geom_point(aes(x= pc.norm.1,y= pc.norm.3)) +
  geom_text(aes(x= pc.norm.1,y= pc.norm.3,label= VALUE)) +
  xlim(-0.5,.5)
   
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(normed.pca) +
  labs(title = "Principal Component Dimensions")         
     
normed.pca$scores

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
abline(h=0.785,lwd=1.5,col='red')
abline(v=5,lwd=1.5,col='red')
text(4,.5,'Keep 5 Principal Components',col='red') 
title('Total Variance Explained Plot')


       
 #write.csv(completedData,file="imputedMoneyball.csv")
plot(normed.pca)
library(cluster)
library(maptree)
library(ggridges)        



clusterresults <- kmeans(norm.PCA[,2:3],4)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss


clusplot(norm.PCA, clusterresults$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)



clusterresults$cluster

norm.PCA.Cluster <- cbind(norm.PCA,clusterresults$cluster)

library(factoextra) # clustering algorithms & visualization
fviz_nbclust(norm.PCA[,-1], kmeans, method = "wss")


#######################
library(Rtsne)
library(data.table)
tsne_vis <- function( perplexity = 1, learning = 200, iterations = 500, cor) {
  
  set.seed(1)
  tsne <- Rtsne(cor, dims = 2, perplexity=perplexity, verbose =TRUE, max_iter = iterations, learning = learning)
  #tsne.results <- data.table(x = tsne$Y[,1], y = tsne$Y[,2], Symbol = row.names(cor))
  #tsne.results <- merge(tsne.results, by = c("Symbol"))[, c("PC1", "PC2") := NULL]
  
  #ggplot(tsne.results, aes(x, y)) +
   # geom_text() +
    #labs(title = paste("t-SNE: Perplexity=", perplexity, " Learning=", learning, "Iterations=", iterations ),
    x = "tSNE dimension 1", y = "tSNE dimension 2")+
    #theme(legend.position = "right")
}

library(ggrepel)
corrs <-corrs[-1,-1]

tsne.results<- tsne_vis(perplexity = 1, learning = 500, iterations = 10000, corrs)
tsne.results <- data.table(x = tsne.results$Y[,1], y = tsne.results$Y[,2], Symbol = row.names(corrs))

ggplot(tsne.results, aes(x, y)) +
  geom_point() +
  geom_text_repel(aes(label = Symbol),
                  size = 4, box.padding = 1.5,
                  segment.size  = 0.2, segment.color = "grey50") +
  labs(title = paste("t-SNE: Perplexity=", 1, " Learning=", 500, "Iterations=", 10000 ), x = "tSNE dimension 1", y = "tSNE dimension 2")
  
  
tsne.ret <- t(returns.df[, -c(21, 22)])


###########

norm.PCA.Cluster %>% 
filter( clusterresults$cluster == 4)
completedData

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

princcomponents <- 

value.test <- data.test  %>% 
select(-u) %>% 
  select(-TARGET_WINS)



data.test$Pred <- round(predict(clus1.lm, newdata = value.test))
plot(data.test$Pred,data.test$TARGET_WINS)

data.test$correct <- ifelse(abs(data.test$TARGET_WINS -data.test$Pred)>10,1,0)
sum(data.test$correct)/nrow(data.test)


set.seed(123)

set.seed(123)





