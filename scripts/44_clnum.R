library(cluster)
library(factoextra)
data(mtcars)
data_cluster<-mtcars[,c("mpg","disp","hp","wt","qsec")]
data_scaled<-scale(data_cluster)
print(head(data_cluster))
print(summary(data_cluster))
set.seed(123)
fviz_nbclust(data_scaled,kmeans,method="wss")
fviz_nbclust(data_scaled,kmeans,method="silhouette")
k<-3
kmeans_model<-kmeans(data_scaled,centers=k,nstart=25)
print(kmeans_model)
kmeans_clusters<-kmeans_model$cluster
print(kmeans_clusters)
dist_matrix<-dist(data_scaled)
hc_model<-hclust(dist_matrix,method="complete")
plot(hc_model,main="Hierarchical Clustering Dendrogram",xlab="",sub="")
rect.hclust(hc_model,k=k)
hc_clusters<-cutree(hc_model,k=k)
print(hc_clusters)
pam_model<-pam(data_scaled,k=k)
print(pam_model)
pam_clusters<-pam_model$clustering
print(pam_clusters)
fviz_cluster(kmeans_model,data=data_scaled,
             main="K-Means Clustering")
fviz_cluster(list(data=data_scaled,cluster=hc_clusters),
             main="Hierarchical Clustering")
fviz_cluster(pam_model,main="PAM Clustering")
comparison<-data.frame(
  Car=rownames(mtcars),KMeans=kmeans_clusters,
  Hierarchical=hc_clusters,PAM=pam_clusters)
print(comparison)
cat("K-Means Silhouette Width =",mean(silhouette(kmeans_clusters,dist_matrix)[,3]),"\n")
cat("Hierarchical Silhouette Width =",mean(silhouette(hc_clusters,dist_matrix)[,3]),"\n")
cat("PAM Silhouette Width =",mean(silhouette(pam_clusters,dist_matrix)[,3]),"\n")
