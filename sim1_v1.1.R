# Load required libraries
library(MASS)
library(tidyverse)
library(spatstat)
library(raster)

# Microphone array

# Define the range of x and y
x_range <- seq(1, 4, by = 1)
y_range <- seq(2, 3, by = 1)

# Create the grid of points
microphones <- expand.grid(x = x_range, y = y_range)

# plot array

ggplot(microphones, aes(x = x, y = y)) +
  geom_point() +
  theme_minimal()

# Poisson point process

window <- owin(c(0, 5), c(0, 5))
ppp_object <- rpoispp(1, win = window)

ppp_df <- as.data.frame(ppp_object)

ggplot(ppp_df, aes(x = x, y = y)) +
  geom_point()

# Combined plot

plot1_df <- rbind(ppp_df, microphones)
plot1_df$type <- as.factor(c(rep("call", nrow(ppp_df)), rep("micro", nrow(microphones))))

ggplot() +
  geom_point(data = plot1_df, aes(x = x, y = y, colour = type))


# Detection function
hazard_halfnormal_detection <- function(sigma, lam0, d) {
  prob <- 1 - exp(-lam0 * exp(-d * (2 * sigma^2)))
  return(prob)
}

distance_intervals <- seq(0, 5, 0.1)
probs <- hazard_halfnormal_detection(1.5, 15, distance_intervals)

det_func <- data.frame(dist = distance_intervals, dprob = probs)

ggplot(det_func, aes(x = dist, y = dprob)) +
  geom_line()

# Capture histories

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

# Calculate probabilities
prob_hist <- matrix(NA, nrow = nrow(ppp_df), ncol = nrow(microphones))
for (i in 1:nrow(microphones)) {
  prob_hist[, i] <- calculate_prob_succ(ppp_df, microphones[i, ])
}


det_ind <- as.data.frame(prob_hist) %>%
  mutate(num = paste("Frog",row_number()))

det_ind <- det_ind[rowSums(det_ind[,1:8])>0,]

dat <- cbind(ppp_df, prob_hist) 

det_dat <- dat %>%
  rowwise() %>%
  mutate(
    sum = sum(c_across(3:10)),
    det = ifelse(sum > 0, "Detected", "Not Detected")
  ) %>%
  select(x,y,det) %>%
  data.frame() %>%
  mutate(num = row_number())


ggplot(det_dat, aes(x = x, y = y,colour=det)) +
  geom_point(shape="\u1318") +
  geom_point(data=microphones,aes(x=x,y=y),colour="black")+
  xlim(0,5)+
  geom_text(aes(label = num), vjust = -0.5)

  
mean(det_ind)

indp <- apply(det_ind, 2, mean)
boxplot(indp)

ave <- 1 - (1 - mean(det_ind))^8

nrow(det_ind) / ave



