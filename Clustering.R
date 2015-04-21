### K-means Clustering

set.seed(2)
x = matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] = x[1:25,1] + 3
x[1:25, 2] = x[1:25,2] - 4

# perform k-means with k = 2
km.out = kmeans(x, 2, nstart = 20)

km.out$cluster

plot(x, col = km.out$cluster + 1, 
     main = "K-means Clustering Results with K = 2",
     xlab ="X1",
     ylab = "X2",
     pch = 20, 
     cex = 2)


# perform k-means with k = 3
set.seed(4)
km.out = kmeans(x, 3, nstart = 20)

km.out$clusters

plot(x, col = km.out$cluster + 1, 
     main = "K-means Clustering Results with K = 2",
     xlab ="X1",
     ylab = "X2",
     pch = 20, 
     cex = 2)

### we want to minimize
km.out$tot.withinss



##### Apply Hierarchical Clustering 

### dist(x) computes the Eculidean distances among observations
hc.complete = hclust(dist(x), method = "complete")
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")

par(mfrow=c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "X1", ylab = "X2", cex = 0.9)
plot(hc.average, main = "Average Linkage", xlab = "X1", ylab = "X2", cex = 0.9)
plot(hc.single, main = "Single Linkage", xlab = "X1", ylab = "X2", cex = 0.9)

## to determine the cluster labels for each 
## observation associated with a given cut of the dendogram
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)

library(MASS)
parcoord(x, col = cutree(hc.complete, 2) + 1, lty = 1)

