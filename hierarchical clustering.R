options(scipen=10)

##--------------------------------------Step1 : Read the data---------------------------------------------

europe<-read.csv("E:\\Data\\11\\clust\\europe.csv")
names(europe)
dim(europe)

##--------------------------------------Step2 : Scaling data + Finding Distance Measures---------------------------------------------

#Distance calculation : Euclidean
#Method : Average

#Daisy function in R scales the data and computes the distance measures
library(cluster)
#?daisy
dmatrix<-daisy(europe[-1],metric="euclidean",stand=TRUE)
summary(dmatrix)
class(dmatrix)

distmatrix<-dist(dmatrix)
str(distmatrix)

#Writing out the file
d<-as.matrix(distmatrix)
class(d)
write.csv(d,"distmatrix.csv",row.names=F)

##--------------------------------------Step3 : Hierarchcal clusterting algorithm---------------------------------------------

#hclust
?hclust
euroclust<-hclust(distmatrix,method="average")
str(euroclust)
#euroclust<-hclust(distmatrix,method="complete")


##--------------------------------------Step4 : Plotting a Dendogram---------------------------------------------

plot(as.dendrogram(euroclust))
#plot(as.dendrogram(euroclust),labels=europe$Country)
rect.hclust(euroclust, 5)

##--------------------------------------Step5 : Examining the hclust object---------------------------------------------


#The cluster height used in the dendogram
euroclust$height

#labels for each of the objects being clustered
euroclust$labels<-europe$Country


#distance measure used
euroclust$dist.method

##----------------------------------Step6 : Slicing the dendogram to get finite number of clusters---------------------------------------------

#To get flat clustering : 
k<-cutree(euroclust,k = 5)
head(k)
#Once you have k which is the cluster no, attach it to your dataset
europe$cluster<-k

##----------------------------------Step7 : Profiling clusters---------------------------------------------
#You can now profile your data similar to kmeans clustering example

#Cluster wise Summaries
cmeans<-aggregate(europe[,2:8],
                  by=list(europe$cluster),
                  FUN=mean)
names(cmeans)
dim(cmeans)

#Population Summaries
options(scipen=999)
apply(europe[,2:8],2,mean)
apply(europe[,2:8],2,sd)

#Z value normalisation

#Function to calculate Z values
list<-names(cmeans)
#z score =population_mean - group_mean /population_sd
for(i in 1:length(list))
{
  y<-(cmeans[,i+1] - apply(europe[,2:8],2,mean)[i])/(apply(europe[,2:8],2,sd)[i])
  cmeans<-cbind(cmeans,y)
  names(cmeans)[i+8]<-paste("z",list[i+1],sep="_")
  print(list[i+1])
}

cmeans<-cmeans[,-16]
write.csv(cmeans,"cmeans.csv",row.names=F)

library(sqldf)
sqldf("select count(*) from europe group by cluster")

##----------------------------------Step7 : Other Packages to Plot a dendogram---------------------------------------------

#ggdendro
# install.packages('ggdendro')
library(ggdendro)
# basic option

ggdendrogram(euroclust, 
             rotate = FALSE, 
             theme_dendro = TRUE, 
             color = "tomato")

#A2R
#load code of A2R function
source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
#colored dendrogram
op = par(bg = "#EFEFEF")
A2Rplot(euroclust, 
        k = 5, 
        cex=0.2,
        boxes = FALSE, 
        col.up = "grey50", 
        col.down = c("green","blue", "black","red","brown"))
