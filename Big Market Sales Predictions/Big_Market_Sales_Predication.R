#Loadind test and train dataset
train<-read.csv('Train.csv',header=TRUE)
test<-read.csv('Test.csv',header=TRUE)

#Adding new Column to combine test and train
test1 <- data.frame(Item_Outlet_Sales=rep(0,nrow(test)),test[,])

#Combing test and train using rbind
data.combined <- rbind(train,test)

#Finding the structure of train dataset
str(train)

#Finding missing values in table
table(is.na(data.combined))

#Finding cols for missing values
colSums(is.na(data.combined))

#imputing data
data.combined$Item_Weight[is.na(data.combined$Item_Weight)]<-median(data.combined$Item_Weight,na.rm=TRUE)

#imputing Item_Visibility  if 0 replace with median
data.combined$Item_Visibility<-ifelse(data.combined$Item_Visibility==0,median(data.combined$Item_Visibility),data.combined$Item_Visibility)

#rename level in Outlet_size
levels(data.combined$Outlet_Size)[1]<-"Other"

#rename levels of fat content
library(dplyr)
library(plyr)
#Converting to same case
data.combined$Item_Fat_Content<-revalue(data.combined$Item_Fat_Content,c("LF"="Low Fat","reg"="Regular"))
data.combined$Item_Fat_Content<-revalue(data.combined$Item_Fat_Content,c("low fat"="Low Fat"))

#create a new column
data.combined$Year <- 2013 -data.combined$Outlet_Establishment_Year 

#Dropping variable which are not required for modelling
data.combined1 <- select(data.combined,-c(Item_Identifier,Outlet_Identifier,Outlet_Establishment_Year))

#divided dataset back into test and train
new_train <- data.combined1[1:nrow(train),]
new_test <- data.combined1[-(1:nrow(train)),]

linear_model <- lm(Item_Outlet_Sales~.,data=new_train)

  


