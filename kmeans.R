##Clustering in R

#Read and explore the data

##--------------------------------------Step1 : Reading and exploring the dataset ---------------------------------------------

data<-read.csv("cust.csv")

dim(data)
str(data)
names(data)
summary(data)

colSums(is.na(data))
#Frequency distribution
table(data$Channel)
table(data$Region)


#Subsetting Data to only Price ,Bedrooms,Squarefeet
names(data)
sample<-data[,3:8]
dim(sample)
str(sample)
names(sample)

##--------------------------------------Step2 : Scaling the data ---------------------------------------------
#(column  - mean(column))/sd(column)
#Repeat for all columns

list<-names(sample)
scaled_data<-data.frame(rownum<-1:440)
for(i in 1:length(list))
{
  
  x<-(sample[,i]-mean(sample[,i]))/(sd(sample[,i]))
  scaled_data<-cbind(scaled_data,x)
  names(scaled_data)[i+1]<-paste("scaled_",list[i])
  print(list[i])
  
}
head(scaled_data)

scaled_data<-scaled_data[,-1]
# 
sample<-cbind(sample,scaled_data)
names(sample)
# head(sample)


##--------------------------------------Step3 : kmeans algorithm ---------------------------------------------

#syntax : kmeans(scaled_data,k) ; where k refers to the number of clusters
set.seed(200)
names(sample)
fit.km<-kmeans(sample[,7:12],3)

#We will get a list object
fit.km$size #No of observations in each cluster
fit.km$withinss #Within sum of squares metric for each cluster
fit.km$totss #The total sum of squares
fit.km$tot.withinss #Total within-cluster sum of squares, i.e., sum(withinss)
fit.km$betweenss #The between-cluster sum of squares, i.e. totss-tot.withinss

##--------------------------------------Step4 : find the optimal number of clusters (k value) ---------------------------------------------

#Create a screeplot-plot of cluster's tot.withinss wrt number of clusters

wss<-1:15
number<-1:15

for (i in 1:15)
  
{
  wss[i]<-kmeans(sample[,7:12],i)$tot.withinss
}

#Shortlised optimal number of clusters : between 7 and 9

#Better plot using ggplot2
library(ggplot2)
data<-data.frame(wss,number)
p<-ggplot(data,aes(x=number,y=wss),color="red")
p+geom_point()+scale_x_continuous(breaks=seq(1,20,1))

##--------------------------------------Step5a : Rerun the algorithm with k=7(optimal no)---------------------------------------------

#Build 7 cluster model
set.seed(100)
fit.km<-kmeans(sample[,4:6],7)

##Merging the cluster output with original data

sample$cluster<-fit.km$cluster

##--------------------------------------Step5b : Profile the clusters---------------------------------------------


#Cluster wise Aggregates
cmeans<-aggregate(sample[,1:6],by=list(sample$cluster),FUN=mean)
cmeans
dim(cmeans)

#Population Aggregates
apply(sample[,1:6],2,mean)
apply(sample[,1:6],2,sd)

list1<-names(cmeans)

#Z score calculation
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list1))
{
y<-(cmeans[,i+1] - apply(sample[,1:6],2,mean)[i])/(apply(sample[,1:6],2,sd)[i])
cmeans<-cbind(cmeans,y)
names(cmeans)[i+7]<-paste("z",list1[i+1],sep="_")
print(list1[i+1])
}

cmeans<-cmeans[,-14]
names(cmeans)

##--------------------------------------Step6a : Rerun the algorithm with k=9(in order to compare)---------------------------------------------

#Build a 9 clusters

fit.km1<-kmeans(sample[,7:12],9)

sample$cluster9<-fit.km1$cluster


##--------------------------------------Step6b : Profile the clusters---------------------------------------------

#Cluster summary
cmeans<-aggregate(sample[,1:6],by=list(sample$cluster9),mean)

#Population summary
apply(sample[,1:6],2,mean)
apply(sample[,1:6],2,sd)
cmeans1

#z-score calculation function
list2<-names(cmeans1)
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list2))
{
  print(i)
  y<-(cmeans1[,i+1] - apply(sample[,1:6],2,mean)[i])/(apply(sample[,1:6],2,sd)[i])
  cmeans1<-cbind(cmeans1,y)
  names(cmeans1)[i+7]<-paste("z",list2[i+1],sep="_")
  print(list2[i+1])
}

cmeans1<-cmeans1[,-14]
names(cmeans1)

#Note : The higher the value of z, the different is the group from the population

#If still not satisfied, again run an iteration


##--------------------------------------Step7 : Visualise the clusters---------------------------------------------

#Plotting groups across two dimensions

library(ggplot2)
data<-cbind(data,sample)

#For cluster7
#Milk Vs Grocery
p<-ggplot(data,aes(x=Milk,y=Grocery))
p+geom_point(aes(colour=as.factor(cluster)))
  
#Across Region
p+geom_point(aes(colour=as.factor(cluster)))+
  facet_grid(Region~.)

#Across Channel
p+geom_point(aes(colour=as.factor(cluster)))+
  facet_grid(Channel~.)

#For cluster9

#Milk Vs Grocery
p<-ggplot(data,aes(x=Milk,y=Grocery))
p+geom_point(aes(colour=as.factor(cluster9)))

#Across Region
p+geom_point(aes(colour=as.factor(cluster9)))+
  facet_grid(Region~.)

#Across Channel
p+geom_point(aes(colour=as.factor(cluster9)))+
  facet_grid(Channel~.)

#Analysis
p+geom_point(aes(colour=as.factor(cluster9),size=Fresh))+
  facet_grid(Region~.)

p+geom_point(aes(colour=as.factor(cluster9),size=Frozen))+
  facet_grid(Region~.)

p+geom_point(aes(colour=as.factor(cluster9),size=Detergents_Paper))+
  facet_grid(Region~.) #You see some impact

p+geom_point(aes(colour=as.factor(cluster9),size=Delicassen))+
  facet_grid(Region~.) 

#Further Deep dive
p+geom_point(aes(colour=as.factor(cluster9),size=Detergents_Paper))+
  facet_grid(Region~Channel) #You see some impact

