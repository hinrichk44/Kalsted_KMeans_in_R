#Here we will using K-Means Clustering for this project in R

#We will be using the iris dataset, which is inherent in R via the ISLR library
#Let's import the ISLR library
library(ISLR)

#Now let's create our dataframe for analysis
#There are 150 observations of five variables
#Of course, usually you won't have labeled data. We are going to pretend we dont for this exercise.
#K-Means Clustering is meant for UNLABELED data, it is UNSUPERVISED learning. 
irises = iris

#Here is just a quick scatterplot of the data
library(ggplot2)
ggplot(irises, aes(Petal.Length, Petal.Width, color = Species)) + geom_point(size = 4)



#Let's set our seed to make sure we get consistent results
set.seed(101)

#Here we are building our K-Means algorithm. 
#Because we tend to not have labeled data, we are removing the last column
#We're going to start with 3 clusters
#nstart is the amount of random starts we can do. We will set it to 20 for this example
irisclusters = kmeans(irises[, 1:4], 3, nstart = 20)

#Here is a look at the model results
#In our three clusters we have cluster sizes of 38, 62, and 50. 
print(irisclusters)


#Let's check our clustering against actual observed data (once again not usually doable)
#You have three rows for three clusters
table(irisclusters$cluster, irises$Species)


#Here we are going to do a simple plot of the clusters
#First we will import the cluster library
library(cluster)

#And here is our plot. Three clusters, with different shapes in each cluster
#It states that components 1 and 2 explain 95.02% of the variance
clusplot(irises, irisclusters$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0)
