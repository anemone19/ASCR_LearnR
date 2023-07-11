# Load required libraries
library(MASS)
library(tidyverse)
library(spatstat)
library(ggimage)
library(mt5751a)


# Microphone array

# Capture histories
# Detection function
hazard_halfnormal_detection <- function(sigma, lam0, d) {
  prob <- 1 - exp(-lam0 * exp(-d * (2 * sigma^2)))
  return(prob)
}

calculate_prob_succ <- function(dataframe, detector) {
  # Calculate distance between each point and the detector
  dataframe$distance <- sqrt((dataframe$x - detector$x)^2 + (dataframe$y - detector$y)^2)
  
  # Define distance intervals and associated probabilities
  distance_intervals <- seq(0, 5, 0.1)
  probabilities <- hazard_halfnormal_detection(1.5, 15, distance_intervals)
  
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
    
    dataframe$det[i] <- rbinom(1, 1, dataframe$prob_succ[i])
  }
  
  return(dataframe$det)
}


# Define the range of x and y
x_range <- seq(1, 4, by = 1)
y_range <- seq(2, 3, by = 1)

# Create the grid of points
microphones <- expand.grid(x = x_range, y = y_range)

# Poisson point process
set.seed(1000)
window <- owin(c(0,5), c(0, 5))
ppp_object <- rpoispp(2, win = window)

ppp_df <- as.data.frame(ppp_object)

calculate_prob_succ(ppp_df, microphones)


prob_hist <- sapply(1:nrow(microphones), function(j) {
  calculate_prob_succ(ppp_df, microphones[j, ])
})


simulate_surveys <- function(points, micro, nsims=1000){
  nhats <- rep(0, nsims)
  for(i in 1:nsims){
    # capture histories
    prob_hist <- sapply(1:nrow(micro), function(j) {
      calculate_prob_succ(points, micro[j, ])
    })
    
    model <- fit.cr(as.data.frame(prob_hist[rowSums(prob_hist)>0,]),
                    model="M0",
                    start.p = 0.05)
    nhats[i] <- model$Nhat[1]
  }
 
  return(list(nhats = nhats, true = nrow(ppp_df), probh = prob_hist))
}

simulate_surveys2 <- function(points, micro){
 
    prob_hist <- sapply(1:nrow(micro), function(j) {
      calculate_prob_succ(points, micro[j, ]) })
  
    model <- fit.cr(as.data.frame(prob_hist[rowSums(prob_hist)>0,]),model="M0")
  
  return(nhat = model$Nhat[1])
}

data1000 <- simulate_surveys(ppp_df,microphones,nsims=1000)


ggplot(data.frame(nhats=data1000$nhats),aes(x=nhats))+
  geom_histogram(bins=10,fill="#97cba9",colour="#668ba4")+
  geom_vline(xintercept=data1000$true)+
  theme_minimal()+
  labs(
    x = "\nAbundance estimates",
    y = "Frequency\n"
  )






