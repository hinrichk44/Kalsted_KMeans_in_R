#For this project we will use K-Means CLUSTERING in R
#This is NOT K-NN, there is a difference

#Here we are importing the dataset
#The mall has a purchase history of all their customers
#We have CustomerID, Gender, Age, Annual Income, and Spending Score
#The mall wants us to segment their customers based on annual income and spending score
mall = read.csv('Mall_Customers.csv')


#Let's separate the variables we will segment customers by
X = mall[4:5]


#We will use the elbow method to find the optimal number of clusters

#Let's set the seed so we get consistent results
set.seed(6)

#The elbow method is derived from within sum of squares, wcss
#We will initialize an empty vector called "wcss" and then have a for-loop populate the vector
wcss = vector()
#i is going to take values from 1 to 10
#Then, at each iteration we will calculate the within sum of squares for each i-clusters
#The wcss vector will now be populated with 10 different within sum of squares for the clusters 1-10
for (i in 1:10) wcss[i] = sum(kmeans(X, i)$withinss)

#Now we will plot the result of the for-loop
#We use b for "both" plotting points and lines
#As you can see this plot is in the shape of an elbow, hence the Elbow Method
#What is the optimal amount of clusters to use based on the elbow?
#From the plot it looks like FIVE clusters is the optimal amount
plot(1:10, wcss, type = 'b', main = paste('Clusters of Clients'),
     xlab = 'Number of Clusters',
     ylab = 'WCSS')


#Now lets apply the K-Means algorithm to the mall dataset

#First we set our seed so we get consistent results
set.seed(29)

#Now we fit kmeans to our data (X)
#We are creating an object "kmeans" from the CLASS kmeans
#The first argument to input is our data, which is X in this case
#The second argument is the number of clusters, in this case is 5
#Third argument is the maximum amount of iterations we will perform
#Fourth argument is the amount of random sets to be chosen (denoted by nstart)
kmeans = kmeans(X, 5, iter.max = 300, nstart = 10)


#Now let's visualize the clusters.
#Remember, at the top we visualized the Elbow Method
#Now, we are visualizing the clusters that we created that will separate our mall customers!

#First we will import the cluster library
library(cluster)

#Here is our official visualization
#The first argument is our data, which is (X) in this case
#The second parameter is the vector of information that tells which cluster the vector belongs to
#The third parameter is lines, which show the distance between clusters. We are choosing 0 for this project
#We are setting shade = TRUE so the clusters can be shaded by density
#We are setting labels = 2 so all points and clusters can be labeled
#We are setting plotchar = FALSE because we don't want different plotting symbols
#We are setting span = TRUE because we want to have ellipses around the clusters
clusplot(X, kmeans$cluster, lines = 0, shade = TRUE, 
         color = TRUE, labels = 2, plotchar = FALSE, 
         span = TRUE, main = paste("Clusters of Clients"),
         xlab = 'Annual Income', ylab = "Spending Score")

#REVIEW OF PLOT
#You can now see how the customers are sorted
#This means you can label groups of customers by their cluster
#Cluster 1 looks like high income and high spending score. This must be coveted by the mall
#Cluster 4 looks like low income and low spending score. Probably not what the mall wants