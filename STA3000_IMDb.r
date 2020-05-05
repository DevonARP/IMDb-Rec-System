library(tidyverse)
library(plyr)
library(factoextra)
library(NbClust)
options(max.print = 8000)

#Replace your file location here
imdb <- read_csv("C:/Users/poona/Desktop/3000/imdb.csv")
View(imdb)
dim(imdb)
str(imdb)

#Changeing Datatypes
imdb$nrOfPhotos = as.numeric(imdb$nrOfPhotos)
imdb$nrOfNominations = as.numeric(imdb$nrOfNominations)
imdb$nrOfWins = as.numeric(imdb$nrOfWins)
imdb$duration = as.numeric(imdb$duration)
imdb$ratingCount = as.numeric(imdb$ratingCount)
imdb$imdbRating = as.numeric(imdb$imdbRating)

#Getting rid of empty columns
imdb$X45 = NULL
imdb$X46 = NULL
imdb$X47 = NULL
imdb$X48 = NULL
str(imdb)

#Getting movies only
unique(imdb$type)
practical = filter(imdb,imdb$type=='video.movie')

#Getting rid of characteristic columns and omitting empty data
practical$type=NULL
practical$url=NULL
practical$wordsInTitle=NULL
practical$fn=NULL
practical$title=NULL
practical$tid=NULL
practical=practical[0:10]
practical = na.omit(practical)

#Scaled it
practical=scale(practical)
practical = as.data.frame(practical)
view(practical)
summary(practical)

#Another replicated data frame with three less columns
practical1=practical
practical1$imdbRating=NULL
practical1$nrOfUserReviews=NULL
practical1$ratingCount=NULL

#Kmeans with 90 centers
p1 = kmeans(practical, centers=90,iter.max = 30)
summary(p1)
str(p1)
p1$cluster
p1$centers
p1$totss
p1$withinss
p1$tot.withinss
p1$betweenss
p1$betweenss/p1$totss
p1
count(practical$imdbRating)
table(practical$imdbRating,p1$cluster)

#Graph on Entire Dataset
library(cluster)
clusplot(practical,p1$cluster)

#Kmeans with 240 centers
p2= kmeans(practical1,center=240, iter.max=30)
p2$tot.withinss
clusplot(practical1,p2$cluster)
clusplot(practical1,p1$cluster)

#Rand Index
library(fossil)
rand.index(practical$imdbRating,p2$cluster)
rand.index(practical$imdbRating,p1$cluster)

#Data frame with one column
practical2=practical
practical2$ratingCount=NULL
practical2$duration=NULL
practical2$year=NULL
practical2$nrOfWins=NULL
practical2$nrOfNominations=NULL
practical2$nrOfPhotos=NULL
practical2$nrOfNewsArticles=NULL
practical2$nrOfUserReviews=NULL
practical2$nrOfGenre=NULL

#Graph of only IMDb rating
clusplot(practical2,p1$cluster)
clusplot(practical2,p2$cluster)
