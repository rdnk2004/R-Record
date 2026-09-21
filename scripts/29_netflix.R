library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
netflix<-read.csv("data/netflix_titles.csv",stringsAsFactors=FALSE,check.names=FALSE)
print(head(netflix,10))
print(tail(netflix,10))
str(netflix)
summary(netflix)
glimpse(netflix)
missing_values<-colSums(is.na(netflix))
print(missing_values)
netflix<-netflix%>%distinct()
netflix<-netflix%>%distinct(title,.keep_all=TRUE)
text_columns<-c("title","director","country","listed_in","description")
for(col in text_columns){
  if(col%in%names(netflix)){
    netflix[[col]]<-str_trim(netflix[[col]])
    netflix[[col]]<-str_squish(netflix[[col]])
  }
}
if("title"%in%names(netflix)) netflix$title<-str_to_title(netflix$title)
if("director"%in%names(netflix)){
  netflix$director[is.na(netflix$director)|netflix$director==""]<-"Unknown"
}
if("country"%in%names(netflix)){
  netflix$country[is.na(netflix$country)|netflix$country==""]<-"Unknown"
}
if("listed_in"%in%names(netflix)){
  netflix$listed_in[is.na(netflix$listed_in)|netflix$listed_in==""]<-"Unknown"
  netflix$listed_in<-str_replace_all(netflix$listed_in,",\\s*"," | ")
}
if("date_added"%in%names(netflix)){
  netflix$date_added<-str_trim(netflix$date_added)
  netflix$date_added<-as.Date(netflix$date_added,format="%B %d, %Y")
}
if("date_added"%in%names(netflix)){
  netflix$Added_Year<-year(netflix$date_added)
  netflix$Added_Month<-month(netflix$date_added,label=TRUE,abbr=FALSE)
}
valid_ratings<-c("G","PG","PG-13","R","NC-17","NR","UR","TV-G","TV-PG","TV-14","TV-MA","TV-Y","TV-Y7","TV-Y7-FV")
if("rating"%in%names(netflix)){
  netflix$rating<-str_trim(netflix$rating)
  netflix$rating[!netflix$rating%in%valid_ratings]<-NA
  netflix$rating[is.na(netflix$rating)]<-"Unknown"
}
if("country"%in%names(netflix)){
  netflix$country<-str_replace_all(netflix$country,"United States","United States")
  netflix$country<-str_replace_all(netflix$country,"United States of America","United States")
  netflix$country<-str_replace_all(netflix$country,"UK","United Kingdom")
  netflix$country<-str_replace_all(netflix$country,"South Korea","South Korea")
  netflix$country<-str_squish(netflix$country)
}
if("type"%in%names(netflix)){
  netflix$type<-str_to_title(netflix$type)
}
if("cast"%in%names(netflix)){
  netflix$cast[is.na(netflix$cast)|netflix$cast==""]<-"Unknown"
  netflix$cast<-str_squish(netflix$cast)
}
if("description"%in%names(netflix)){
  netflix$description[is.na(netflix$description)|netflix$description==""]<-"Unknown"
}
if("listed_in"%in%names(netflix)){
  categories<-str_split(netflix$listed_in,"\\s*\\|\\s*")
  netflix$Category_1<-sapply(categories,function(x)ifelse(length(x)>=1,x[1],"Unknown"))
  netflix$Category_2<-sapply(categories,function(x)ifelse(length(x)>=2,x[2],"Unknown"))
  netflix$Category_3<-sapply(categories,function(x)ifelse(length(x)>=3,x[3],"Unknown"))
}
if("title"%in%names(netflix)){
  netflix$title<-str_squish(netflix$title)
}
if("director"%in%names(netflix)){
  netflix$director<-str_squish(netflix$director)
}
if("country"%in%names(netflix)){
  netflix$country<-str_squish(netflix$country)
}
print(colSums(is.na(netflix)))
print(sum(duplicated(netflix)))
print(table(netflix$type,useNA="ifany"))
print(table(netflix$rating,useNA="ifany"))
if("Added_Year"%in%names(netflix)){
  print(table(netflix$Added_Year,useNA="ifany"))
}
if("type"%in%names(netflix)){
  ggplot(netflix,aes(x=type))+geom_bar()+labs(title="Netflix Content by Type",x="Type",y="Count")
}
if("rating"%in%names(netflix)){
  ggplot(netflix,aes(x=rating))+geom_bar()+labs(title="Netflix Content by Rating",x="Rating",y="Count")
}
if("Added_Year"%in%names(netflix)){
  ggplot(netflix,aes(x=Added_Year))+geom_histogram(binwidth=1)+labs(title="Netflix Content Added by Year",x="Year",y="Count")
}
netflix<-netflix%>%distinct()
write.csv(netflix,"output/Netflix_Cleaned.csv",row.names=FALSE)
print(head(netflix,10))
print(dim(netflix))
str(netflix)