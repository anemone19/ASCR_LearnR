# Load required libraries
library(MASS)
library(tidyverse)
library(spatstat)
library(raster)
library(ggimage)
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
  
  # Remove the distance column
  dataframe$distance <- NULL
  
  # Mutate each element based on the probability using the binomial distribution
  # dataframe <- dataframe %>%
  #   mutate(result = rbinom(n(), size=1, prob_succ))
  
  return(dataframe$det)
}

simulate_surveys <- function(nsims=1000){
  # Define the range of x and y
  x_range <- seq(1, 4, by = 1)
  y_range <- seq(2, 3, by = 1)
  
  # Create the grid of points
  microphones <- expand.grid(x = x_range, y = y_range)
  
  # Poisson point process
  
  window <- owin(c(0, 5), c(0, 5))
  ppp_object <- rpoispp(0.5, win = window)
  
  ppp_df <- as.data.frame(ppp_object)
  
  nhats <- NULL
  for(i in 1:nsims){
    # Calculate probabilities
    prob_hist <- matrix(NA, nrow = nrow(ppp_df), ncol = nrow(microphones))
    for (i in 1:nrow(microphones)) {
      prob_hist[, i] <- calculate_prob_succ(ppp_df, microphones[i, ])
    }
    ave <- 1 - (1 - mean(prob_hist))^8
    est <- nrow(prob_hist[rowSums(prob_hist)>0,]) / ave
    nhats <- c(nhats,est)
  }
 
  return(list(nhats = nhats, true = nrow(ppp_df)))
}


data1000 <- simulate_surveys()

ggplot(data.frame(nhats=data1000$nhats),aes(x=nhats))+
  geom_histogram(bins=10)+
  geom_vline(xintercept=data1000$true)
