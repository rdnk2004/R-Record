library(ggplot2)
customer_data<-read.csv("data/Mall_Customers.csv")
names(customer_data)<-c("CustomerID","Gender","Age","AnnualIncome","SpendingScore")
customer_data<-unique(customer_data)
customer_data$Gender<-factor(trimws(customer_data$Gender))
customer_data$Age<-as.numeric(customer_data$Age)
customer_data$AnnualIncome<-as.numeric(customer_data$AnnualIncome)
customer_data$SpendingScore<-as.numeric(customer_data$SpendingScore)
customer_data<-customer_data[complete.cases(customer_data),]
print(head(customer_data))
print(summary(customer_data))
print(colSums(is.na(customer_data)))
print(sum(duplicated(customer_data)))
ggplot(customer_data,aes(x=Age))+
  geom_histogram(binwidth=5,fill="skyblue",color="black")+
  labs(title="Customer Age Distribution",x="Age",y="Count")
ggplot(customer_data,aes(x=Gender,fill=Gender))+
  geom_bar()+
  labs(title="Gender Distribution",x="Gender",y="Count")
ggplot(customer_data,aes(x=AnnualIncome,y=SpendingScore))+
  geom_point(color="blue")+
  labs(title="Income vs Spending Score",x="Annual Income (k$)",y="Spending Score")
features<-customer_data[,c("AnnualIncome","SpendingScore")]
scaled_data<-scale(features)
set.seed(123)
wss<-sapply(1:10,function(k)
  kmeans(scaled_data,centers=k,nstart=25)$tot.withinss)
plot(1:10,wss,type="b",pch=19,
     xlab="Number of Clusters",
     ylab="Within-Cluster Sum of Squares",
     main="Elbow Method")
set.seed(123)
kmeans_model<-kmeans(scaled_data,centers=5,nstart=25)
customer_data$Cluster<-factor(kmeans_model$cluster)
print(kmeans_model)
print(aggregate(customer_data[,c("Age","AnnualIncome","SpendingScore")],
                by=list(Cluster=customer_data$Cluster),FUN=mean))
print(table(customer_data$Cluster,customer_data$Gender))
ggplot(customer_data,aes(x=AnnualIncome,y=SpendingScore,color=Cluster))+
  geom_point(size=3)+
  labs(title="Customer Segmentation Using K-Means",
       x="Annual Income (k$)",y="Spending Score")+
  theme_minimal()
dir.create("output",showWarnings=FALSE)
write.csv(customer_data,"output/Mall_Customers_Segmented.csv",row.names=FALSE)