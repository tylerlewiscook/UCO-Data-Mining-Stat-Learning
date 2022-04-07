# Chapter 12 Code

leuk <- read.csv(file = "Data/leukemia.csv", stringsAsFactors = TRUE)
genes <- leuk[, -11]

# PCA --------------------------------------------------------------------------

pca <- prcomp(genes) 

pca$rotation 		
pca$x				
pca$sdev^2			
sum(pca$sdev^2)		

summary(pca)		
summary(pca)[[6]][2, ]	
summary(pca)[[6]][3, ]	

biplot(pca, scale = 0)

plot(pca)
plot(1:10, pca$sdev^2/10, type = "l", xlab = "Prin Comp", 
     ylab = "% Var Explained")
plot(1:10, summary(pca)[[6]][2,], type = "l", xlab = "Prin Comp", 
     ylab = "% Var Explained")



# k-means ----------------------------------------------------------------------

set.seed(245)

newgenes <- genes[, c(1, 4)]

km2 <- kmeans(newgenes, 2, nstart = 20)
km2$cluster
plot(newgenes, col = km2$cluster)

km3 <- kmeans(newgenes, 3, nstart = 20)
km3$cluster
plot(newgenes, col = km3$cluster)

kmfull <- kmeans(genes, 2, nstart = 20)

plot(pca$x[, 1], pca$x[, 2], col = kmfull$cluster, xlab = "PC1", ylab = "PC2")



# Hierarchical clustering ------------------------------------------------------

genedist <- dist(genes)
hcComp <- hclust(genedist, method = "complete")
hcAvg <- hclust(genedist, method = "average")
hcSingle <- hclust(genedist, method = "single")

par(mfrow = c(1, 3))
plot(hcComp, main = "Complete", cex = 0.75)
plot(hcAvg, main = "Average", cex = 0.75)
plot(hcSingle, main = "Single", cex = 0.75)

?cutree
cutree(hcComp, k = 2)
cutree(hcComp, h = 8.25)

par(mfrow = c(1,1))
plot(hcComp, main = "Complete", cex = 0.75)
rect.hclust(hcComp, 2)

par(mfrow = c(1, 2))
newgenedist <- as.dist(1 - cor(t(genes)))
hcCorr <- hclust(newgenedist, method = "complete")
plot(hcCorr, cex = 0.5)

newgenedist2 <- as.dist(1 - cor(genes))
hcCorr2 <- hclust(newgenedist2, method = "complete")
plot(hcCorr2)