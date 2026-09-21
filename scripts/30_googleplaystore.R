library(dplyr)
library(stringr)
library(ggplot2)
apps<-read.csv("data/googleplaystore.csv",stringsAsFactors=FALSE,check.names=FALSE)
print(head(apps,10))
print(tail(apps,10))
str(apps)
summary(apps)
dim(apps)
print(colSums(is.na(apps)))
apps<-apps%>%distinct()
if("App"%in%names(apps)) apps$App<-str_squish(apps$App)
if("Category"%in%names(apps)){
  apps$Category<-str_to_upper(str_trim(apps$Category))
  apps$Category<-str_replace_all(apps$Category,"[^A-Z0-9_]","_")
}
if("Size"%in%names(apps)){
  apps$Size<-str_trim(apps$Size)
  apps$Size<-ifelse(apps$Size=="Varies with device",NA,apps$Size)
  apps$Size<-ifelse(grepl("M$",apps$Size),as.numeric(sub("M","",apps$Size)),ifelse(grepl("k$",apps$Size),as.numeric(sub("k","",apps$Size))/1024,NA))
  apps$Size<-as.numeric(apps$Size)
}
if("Installs"%in%names(apps)){
  apps$Installs<-str_replace_all(apps$Installs,",","")
  apps$Installs<-str_replace_all(apps$Installs,"\\+","")
  apps$Installs<-as.numeric(apps$Installs)
}
if("Price"%in%names(apps)){
  apps$Price<-str_replace_all(apps$Price,"\\$","")
  apps$Price<-as.numeric(apps$Price)
}
if("Reviews"%in%names(apps)){
  apps$Reviews<-as.numeric(apps$Reviews)
}
if("Rating"%in%names(apps)){
  apps$Rating<-as.numeric(apps$Rating)
}
numeric_columns<-names(apps)[sapply(apps,is.numeric)]
for(col in numeric_columns){
  apps[[col]][is.na(apps[[col]])]<-median(apps[[col]],na.rm=TRUE)
}
if("App"%in%names(apps)){
  apps<-apps%>%distinct(App,.keep_all=TRUE)
}
if("Type"%in%names(apps)){
  apps$Type<-str_to_title(str_trim(apps$Type))
  apps$Type[is.na(apps$Type)|apps$Type==""]<-"Free"
}
if("Category"%in%names(apps)){
  apps$Category<-str_to_upper(str_trim(apps$Category))
}
if("Rating"%in%names(apps)){
  q1<-quantile(apps$Rating,0.25,na.rm=TRUE)
  q3<-quantile(apps$Rating,0.75,na.rm=TRUE)
  iqr_value<-q3-q1
  lower<-q1-1.5*iqr_value
  upper<-q3+1.5*iqr_value
  apps$Rating<-pmin(pmax(apps$Rating,lower),upper)
}
if("Reviews"%in%names(apps)){
  q1<-quantile(apps$Reviews,0.25,na.rm=TRUE)
  q3<-quantile(apps$Reviews,0.75,na.rm=TRUE)
  iqr_value<-q3-q1
  lower<-q1-1.5*iqr_value
  upper<-q3+1.5*iqr_value
  apps$Reviews<-pmin(pmax(apps$Reviews,lower),upper)
}
if("Type"%in%names(apps)){
  apps$Free_Paid_Flag<-ifelse(apps$Type=="Free","Free","Paid")
}
if("Size"%in%names(apps)){
  apps$Size_Category<-cut(apps$Size,breaks=c(-Inf,10,50,100,Inf),labels=c("Small","Medium","Large","Very Large"))
}
print(colSums(is.na(apps)))
print(sum(duplicated(apps)))
if("Rating"%in%names(apps)){
  boxplot(apps$Rating,main="Rating Outliers",ylab="Rating")
}
if("Reviews"%in%names(apps)){
  boxplot(apps$Reviews,main="Reviews Outliers",ylab="Reviews")
}
if("Category"%in%names(apps)){
  ggplot(apps,aes(x=Category))+geom_bar()+labs(title="Applications by Category",x="Category",y="Count")+theme(axis.text.x=element_text(angle=90,vjust=0.5))
}
if("Free_Paid_Flag"%in%names(apps)){
  ggplot(apps,aes(x=Free_Paid_Flag))+geom_bar()+labs(title="Free vs Paid Applications",x="Type",y="Count")
}
if("Size_Category"%in%names(apps)){
  ggplot(apps,aes(x=Size_Category))+geom_bar()+labs(title="Applications by Size Category",x="Size Category",y="Count")
}
apps<-apps%>%distinct()
write.csv(apps,"output/GooglePlayStore_Cleaned.csv",row.names=FALSE)
print(head(apps,10))
print(dim(apps))
str(apps)