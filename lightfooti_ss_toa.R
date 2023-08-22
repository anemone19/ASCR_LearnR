library(secr)
library(acre)
library(ascr)
library(tidyverse)

# SS & TOA Analysis ----------------------------------------------------------------------

ascr::lightfooti # data object

# extract signal strength and time of arrival
signal_strength_mat <- lightfooti$capt$ss
time_of_arrival_mat <- lightfooti$capt$toa

# Convert matrix to a single numeric vector excluding zeros
signal_strength <- signal_strength_mat[signal_strength_mat != 0]
time_of_arrival <- time_of_arrival_mat[time_of_arrival_mat != 0]
time_of_arrival <- time_of_arrival / 1000 

# add to captures data frame
captures_ss_toa <- captures

captures_ss_toa$ss <- signal_strength
captures_ss_toa$toa <- time_of_arrival

# ACRE FORMATTING -------------------------------------------------------------------------

lightf_ss_toa_data <- read.acre(
  captures = captures_ss_toa,
  traps = traps,
  control_create_mask = list(buffer=14),
  cue.rates = lightfooti$freqs/25,
  survey.length = 25
)


# MODEL FITTING -------------------------------------------------------------------------

model_ss_toa <- fit.acre(
  dat = lightf_ss_toa_data,
  detfn = "ss",
  ss.opts = list(cutoff = 130)
)

# INFERENCE -------------------------------------------------------------------------

summary(model_ss_toa)

show_detfn(model2)

# Function for plotting signal strength detection function
# first argument: acre_model is a model object created with fit.acre()
# model has to be fitted with signal strength included
# second argument is the SS cutoff used
# third is the buffer distance used
# VALUE: ggplot object
ss_detfunc_plot <- function(acre_model, ss_cutoff, buffer) {
  b0 <- acre_model$coefficients[2]
  b1 <- acre_model$coefficients[4]
  sigma_ss <- acre_model$coefficients[6]

  d <- seq(0, buffer, 0.1)
  cdf <- pnorm(
    cutoff,
    (b0 - b1 * d),
    sigma_ss
  )

  probs <- 1 - cdf

  plot_data <- data.frame(probs = probs, d = d)

  df_plot <- ggplot(plot_data, aes(x = d, y = probs)) +
    geom_line() +
    labs(
      x = "\nDistance (m)",
      y = "Probability of detection\n"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
    )

  return(df_plot)
}

# plot detection function
ss_detfunc_plot(model_ss_toa)

# save data for tutorial
save(signal_strength,
     time_of_arrival, lightf_ss_toa_data, captures_ss_toa,
     model_ss_toa, file = "data/ss_toa.RData")



