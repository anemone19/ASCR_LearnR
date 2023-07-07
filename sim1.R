
# Load required libraries
library(MASS)
library(tidyverse)
library(spatstat)

# Number of points to generate in each cluster
cluster_sizes <- sample(c(2, 3), num_clusters, replace = TRUE)
# Cluster 1 parameters
mean_1 <- c(2, 2)
cov_1 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)

# Cluster 2 parameters
mean_2 <- c(-2, -2)
cov_2 <- matrix(c(1, -0.5, -0.5, 1), nrow = 2)

# Cluster 3 parameters
mean_3 <- c(-3, 3)
cov_3 <- matrix(c(1, 0, 0, 1), nrow = 2)

# Cluster 4 parameters
mean_4 <- c(3, -1)
cov_4 <- matrix(c(1, 0, 0, 1), nrow = 2)

# Generate points from each cluster
points_1 <- mvrnorm(n = cluster_sizes[1], mu = mean_1, Sigma = cov_1)
points_2 <- mvrnorm(n = cluster_sizes[2], mu = mean_2, Sigma = cov_2)
points_3 <- mvrnorm(n = cluster_sizes[3], mu = mean_3, Sigma = cov_3)
points_4 <- mvrnorm(n = cluster_sizes[4], mu = mean_4, Sigma = cov_4)

# Combine the points and cluster labels into a single data frame
data <- rbind(data.frame(X1 = points_1[, 1], X2 = points_1[, 2], Frog = "Frog 1"),
              data.frame(X1 = points_2[, 1], X2 = points_2[, 2], Frog = "Frog 2"),
              data.frame(X1 = points_3[, 1], X2 = points_3[, 2], Frog = "Frog 3"),
              data.frame(X1 = points_4[, 1], X2 = points_4[, 2], Frog = "Frog 4"))

# microphones 

microphones <- data.frame(x=c(-2,-2,2,2),
                          y=c(-2,2,-2,2),
                          rad = rep(10,4))


# Plot the points with color-coded clusters using ggplot
ggplot(data, aes(x = X1, y = X2, color = Frog)) +
  geom_point()+
  geom_point(data= microphones, aes(x=x,y=y, size = rad), shape = 21, alpha = 0.09, fill ='blue',
             color = alpha("white", 0)) +
  geom_point(data= microphones, aes(x=x,y=y),color="black")+
  labs(title = "") +
  theme_minimal()+
  scale_size(range=c(10,40))+
  xlim(c(-5,5)) +
  ylim(c(-5,5)) +
  guides(size="none")

# Poisson process 

ppp_object <- rpoispp(50)
ppp_df <- as.data.frame(ppp_object)


# Define the range of x and y
x_range <- seq(0.25, 0.75, by = 0.125)
y_range <- seq(0.25, 0.75, by = 0.125)

# Create the grid of points
microphones <- expand.grid(x = x_range, y = y_range)

plot_df <- rbind(ppp_df,microphones[,1:2])
plot_df$type <- as.factor(c(rep("call",nrow(ppp_df)),rep("micro",25)))

ggplot()+
  #geom_circle(aes(x0 = x, y0 = y, r = rad), fill="NA",colour="black",alpha=0.05,data= microphones)+
  geom_point(data=plot_df,aes(x=x,y=y,colour=type))+
  scale_x_continuous(breaks=seq(0, 1,length.out=9)) +
  scale_y_continuous(breaks=seq(0, 1,length.out=9)) +
  guides(colour="none")+
  theme_minimal()

library(raster)

dist <- pointDistance(ppp_df,microphones[,1:2],lonlat = FALSE)
dist <- ifelse(dist <= 0.15, 1, 0)
dist <- data.frame(dist)
dist$probd <-apply(dist,1,mean)
dist

ave<-1-(1-mean(dist$probd))^4
nrow(ppp_df)/ave



library(dplyr)

calculate_prob_succ <- function(dataframe, detector) {
  # Calculate distance between each point and the detector
  dataframe$distance <- sqrt((dataframe$x - detector$x)^2 + (dataframe$y - detector$y)^2)
  
  # Define distance intervals and associated probabilities
  distance_intervals <- seq(0,0.,0.01)
  probabilities <- seq(1,0,length.out=6)
  
  # Create a new column to store the probabilities
  dataframe$prob_succ <- NA
  
  # Create a new column to store the probabilities
  dataframe$det <- NA
  
  # Iterate over each row and assign the corresponding probability based on the distance
  for (i in 1:nrow(dataframe)) {
    distance <- dataframe$distance[i]
    
    # Find the index of the interval where the distance falls
    interval_index <- findInterval(distance, distance_intervals)
    
    # Assign the probability based on the interval index
    dataframe$prob_succ[i] <- probabilities[interval_index]
    
    dataframe$det[i] <- rbinom(1,1,dataframe$prob_succ[i])
  }
  
  # Remove the distance column
  dataframe$distance <- NULL
  
  # Mutate each element based on the probability using the binomial distribution
  # dataframe <- dataframe %>%
  #   mutate(result = rbinom(n(), size=1, prob_succ))
  
  return(dataframe$det)
}

# Poisson process 

ppp_object <- rpoispp(50)
ppp_df <- as.data.frame(ppp_object)

test<-calculate_prob_succ(ppp_df, microphones[1,1:2])

# Calculate probabilities
prob_hist <- matrix(NA,nrow=50,ncol=4)
for(i in 1:nrow(microphones)){
  prob_hist[,i] <- calculate_prob_succ(ppp_df, microphones[i,1:2])
}


prob_hist <-apply(prob_hist,1,mean)

ave<-1-(1-mean(prob_hist))^4
nrow(ppp_df)/ave


halfnormal_detection <- function(x) {
  prob = 1-exp(-(x/0.05)^-4)
  return(prob)
}

d<-seq(0,0.125,0.001)
plot(d, 1-exp(-(d/0.05)^-4), type="l", col=4, ylim=c(0,1),
     xlab="Distance", ylab="P(detection)", main="Hazard rate")

# Call the halfnormal_detection() function with a number
probability <- halfnormal_detection(0)

# Print the resulting probability
print(probability)
library(ggplot2)



