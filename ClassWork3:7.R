mower.df<-read.csv("RidingMowers.csv")
set.seed(111)
train.index<-sample(row.names(mower.df),0.6*dim(mower.df)[1])
valid.index<-setdiff(row.names(mower.df),train.index)
train.df<-mower.df[train.index,]
valid.df<-mower.df[valid.index,]
## new household
new.df<-data.frame(Income=60, Lot_Size=20)

#Scatter Plot
plot(Lot_Size~Income, data = train.df, pch=ifelse(train.df$Ownership == "Owner",
                                                  1,3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df),pos=4)
text(60,20,"X")
legend("topright",c("owner","non-owner","newhousehold"),pch=c(1,3,4))



# initialize normalized training, validation data, complete data frames to 
#originals 
train.norm.df<- train.df
valid.norm.df<- valid.df
mower.norm.df<- mower.df

#use preProcess from the caret package to normalize income and lot size. 
library(caret)
norm.values<-preProcess(train.df[,1:2],method=c("center","scale"))
train.norm.df[,1:2]<-predict(norm.values,train.df[,1:2])
valid.norm.df[,1:2]<-predict(norm.values,valid.df[,1:2])
mower.norm.df[,1:2]<-predict(norm.values,mower.df[,1:2])
new.norm.df<- predict(norm.values, new.df)

#use knn 
library(class)
nn<- knn(train = train.norm.df[,1:2],test = new.norm.df,cl = train.norm.df[,3],k=3)
row.names(train.df)[attr(nn,"nn.index")]
