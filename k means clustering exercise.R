install.packages(c("cluster", "rattle", "NbClust"))

data(wine, package="rattle")

head(wine)


df <- scale(wine[-1])

head(df)



wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)



library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


k=3
set.seed(1)
fit.km <- kmeans(df, centers=k, iter.max = 1000)


ct.km <- table(wine$Type, fit.km$cluster)
ct.km



library(cluster)
clusplot(df, fit.km$cluster)
