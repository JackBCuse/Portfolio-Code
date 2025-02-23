PlayerData <- SideProjectData
library(dplyr)

df <- PlayerData %>%
  select(8:13,15:29)


library(factoextra)
install.packages("NbClust")
library(NbClust)
library(cluster)

df2 <- scale(df)

fviz_nbclust(df2, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(df2, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")

set.seed(123)  
gap_stat <- clusGap(df2, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

fviz_gap_stat(gap_stat)


set.seed(123)
nbclust_result <- NbClust(df2, min.nc = 2, max.nc = 10, method = "kmeans")
rownames(df2) <- PlayerData$newPlayer

kmeans_result` <- kmeans(df2, centers = 3, nstart = 25)

fviz_cluster(kmeans_result, data = df2)

PlayerData$OffensiveType <- kmeans_result$cluster

View(PlayerData)

PaintPlayers <- PlayerData[PlayerData$OffensiveType == 2, ]
Playmakers <- PlayerData[PlayerData$OffensiveType == 1, ]
Shooters <- PlayerData[PlayerData$OffensiveType == 3, ]




PPClusters <- PaintPlayers %>%
  select(8:13,15:29)

PPClusters <- scale(PPClusters)

fviz_nbclust(PPClusters, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(PPClusters, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")

set.seed(123)  
gap_stat <- clusGap(PPClusters, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

fviz_gap_stat(gap_stat)

nbclust_resultPP <- NbClust(PPClusters, min.nc = 2, max.nc = 10, method = "kmeans")


kmeans_resultPP3 <- kmeans(PPClusters, centers = 3, nstart = 25)
kmeans_resultPP2 <- kmeans(PPClusters, centers = 2, nstart = 25)


PMClusters <- Playmakers %>%
  select(8:13,15:29)

PMClusters <- scale(PMClusters)

fviz_nbclust(PMClusters, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(PMClusters, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")

nbclust_resultPM <- NbClust(PMClusters, min.nc = 2, max.nc = 10, method = "kmeans")

kmeans_resultPM <- kmeans(PMClusters, centers = 3, nstart = 25)

Playmakers$OffensiveRole <- kmeans_resultPM$cluster
View(Playmakers)



SClusters <- Shooters %>%
  select(8:13,15:29)

SClusters <- scale(SClusters)

fviz_nbclust(SClusters, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(SClusters, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")


nbclust_resultS <- NbClust(SClusters, min.nc = 2, max.nc = 10, method = "kmeans")

kmeans_resultS2 <- kmeans(SClusters, centers = 2, nstart = 25)

Shooters$OffensiveRole <- kmeans_resultS2$cluster
View(Shooters)

write.csv(PaintPlayers, file = "C:/Users/Jackson Bayuk/OneDrive/Documents/R/PaintPlayers.csv", row.names = FALSE)
write.csv(Playmakers, file = "C:/Users/Jackson Bayuk/OneDrive/Documents/R/Plamkers.csv", row.names = FALSE)
write.csv(Shooters, file = "C:/Users/Jackson Bayuk/OneDrive/Documents/R/Shooters.csv", row.names = FALSE)



TeamData <- AllTeamData

TeamDataClusters <- TeamData %>%
  select(8:15)

TeamDataClusters <- scale(TeamDataClusters)
fviz_nbclust(TeamDataClusters, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(TeamDataClusters, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette Method")

nbclust_resultTeam <- NbClust(TeamDataClusters, min.nc = 2, max.nc = 15, method = "kmeans")


pca_result <- prcomp(TeamDataClusters, scale. = TRUE)

summary(pca_result)  

pca_data <- pca_result$x[, 1:5]


nb <- NbClust(pca_data, distance = "euclidean", min.nc = 2, max.nc = 12, method = "kmeans")

install.packages("compositions")
library(compositions)

Teamdf <- AllTeamData %>% select(8:15)

Teamdf <- scale(Teamdf)

set.seed(123)

wss <- (nrow(Teamdf)-1)*sum(apply(Teamdf, 2, var))  # Total within-cluster sum of squares
for (i in 2:10) wss[i] <- sum(kmeans(Teamdf, centers=i)$tot.withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

library(factoextra)

sil_scores <- numeric()

for (k in 2:15) {
  kmeans_result <- kmeans(Teamdf, centers = k)
  sil <- silhouette(kmeans_result$cluster, dist(Teamdf))
  sil_scores[k] <- mean(sil[, 3])  # Extract silhouette width
}

# Plot silhouette width for each number of clusters
plot(2:15, sil_scores[2:15], type = "b", xlab = "Number of Clusters", ylab = "Average Silhouette Width",
     main = "Silhouette Analysis for Optimal Clusters")

kmeans_result3 <- kmeans(Teamdf, centers = 3)
kmeans_result4 <- kmeans(Teamdf, centers = 4)
kmeans_result5 <- kmeans(Teamdf, centers = 5)
kmeans_result6 <- kmeans(Teamdf, centers = 6)
kmeans_result7 <- kmeans(Teamdf, centers = 7)
kmeans_result8 <- kmeans(Teamdf, centers = 8)
kmeans_result9 <- kmeans(Teamdf, centers = 9)
kmeans_result10 <- kmeans(Teamdf, centers = 10)



cluster <- kmeans_result6$cluster

AllTeamData$PlayType <- cluster

write.csv(AllTeamData, file = "C:/Users/Jackson Bayuk/OneDrive/Documents/R/LineupTypeData.csv", row.names = FALSE)

