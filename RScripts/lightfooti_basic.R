# ACRE: LIGHTFOOTI SINGLE SURVEY ANALYSIS ----------------------------------------------

# load required libraries
library(ascr)
library(acre)
library(tidyverse)
library(secr)

# Data preparation ----------------------------------------------------------------------

# get data from ascr package 
lightfooti_data<-ascr::lightfooti

# Convert binary capture history data to format required for data function 
capt_single <- lightfooti_data$capt$bincapt

# METHOD 1 - FOR LOOPS

# # add ids as rownames 
# first_survey_ids <- ids.both[[1]]
# rownames(capt_single) <- first_survey_ids

# Create empty vectors to store results
traps_ID<- c()
ID <- c()

for (i in 1:nrow(capt_single)) {
  for (j in 1:ncol(capt_single)) {
    if (capt_single[i, j] == 1) {
      traps_ID <- c(traps_ID, j)
      ID <- c(ID, rownames(capt_single)[i])
    }
  }
}

# Create a dataframe with the results
captures <- data.frame(ID, traps = traps_ID)
nrow(captures)

# add session and occasion columns 
captures$session <- 1
captures$occasion <- 1 

# reorder 
col_order <- c("session","ID","occasion","traps")
captures <- captures[,col_order]

# METHOD 2 - NO FOR LOOPS 

# Create a matrix of row and column indices where capt == 1
indices <- which(lightfooti_data$capt$bincapt == 1, arr.ind = TRUE)

# Extract the row and column indices
ID <- indices[, 1]
traps_ID <- indices[, 2]

# Create a dataframe with the results
captures <- data.frame(ID, trap = traps_ID) %>%
  arrange(ID)

# add session and occasion columns
# replace rownames in ID with ids 

captures <- captures %>%
  mutate(session = 1, occasion = 1) %>%
  select(session,ID,occasion,trap)

# some EDA 
length(unique(captures$ID)) # 225 CALLS 

# trap dataframe for tutorial
traps <- as.data.frame(lightfooti_data$traps)

# Introduction to package acre -----------------------------------------------------------

# Buffer distance ----------------------------------------------------------------------

# trap dataframe 
traps2 <- traps
traps2$trap <- rownames(traps) # add trap column with trap ids 

secr_traps <- read.traps(data = traps2, # trap layout data
                         trapID = "trap", # name of ID column
                         detector = "proximity") # detector type

secr_capthist <- make.capthist(captures = captures,
                               traps = secr_traps,
                               fmt = "trapID")

# quick and biased sigma

initialsigma <- RPSV(secr_capthist, CC = TRUE)

bufferdist <- 4 * initialsigma

# using suggest.buffer()

suggest.buffer(secr_capthist, detectfn = "halfnormal") 

### Pre-acre -------------------------------------------------------------------

# look at function help file 
# ?read.acre()

# 1st and second arguments should be dataframes that are passed to the function 
# create.capt()
# ?create.capt

# captures must be a dataframes with at least 4 columns: col 1 -> session, col 2 -> ID 
# col 4-> traps_ID 

captures

# traps -> matrix, list or dataframe -> two columns x and y cartesian coordinates
# of trap locations 

traps

dim(traps) # six microphones 

# arrangement of microphones 

ggplot(as.data.frame(traps),aes(x=x,y=y))+
  geom_point()+
  ylim(-5,10)+
  xlim(-5,10)+
  coord_fixed()+
  theme_bw()

# third argument MASK 
# we can create our own mask with the function create.mask() which takes the same traps_ID
# dataframe as first argument and then buffer distance 

mask <- create.mask(traps, 14)

# create covariate data frame using mask coordinates 

cov_dat <- data.frame(x = mask[,1], y = mask[,2], spat_trend = "yes")

# plot mask and detectors

ggplot() +
  geom_point(data = as.data.frame(mask[,1:2]), aes(x = x, y = y, color = "Mask"), alpha = 0.2) +
  geom_point(data = as.data.frame(traps), aes(x = x, y = y, color = "Detector")) +
  scale_color_manual(values = c("Mask" = "darkgrey", "Detector" = "blue")) +
  labs(title = "Habitat mask & microphones", x = "X", y = "Y") +
  theme_minimal()

# OR 

# can pass buffer distance to the argument control_create_mask in the read.acre() and it will
# create a mask for you 

# per individual per 25 seconds 
call_rates <- lightfooti_data$freqs

# create data object for model fitting 

### ACRE Formatting ---------------------------------------------------------------------------

# no survey length specified, survey.length = 1 -> density per length of survey, i.e. per 25 seconds
# survey.length = 25 (25 seconds) -> density per second 
# survey.length = 25/60 (0.4167 minutes) -> density per minute 

lightfooti_data <- read.acre(
  captures = captures,
  traps = traps,
  mask = mask
)

### Model fitting ---------------------------------------------------------------------------

model1 <- fit.acre(
  dat = lightfooti_data,
  detfn = "hn",
  fix = list(g0=1)
)

### Inference ---------------------------------------------------------------------------

# model summary 
summary(model1)

# variance 

model1_boot <- boot.acre(model1, N = 500)

model1_boot$coefficients

summary(model1_boot)

acre::stdEr(model1_boot) # standard errors

confint(model1_boot,type="fitted")

# detection function

show_detfn(model1)

# density 

show_Dsurf(model1) # constant like we specified 

# animal density 

# average call rate
ave_call_rate <- mean(call_rates)

# convert to animal density 

model1$coefficients[5]/ave_call_rate

# EXERCISE: TIME UNITS --------------------------------------------------------------------------------------

acre_data <- read.acre(
  captures = captures,
  traps = traps,
  mask = mask,
  survey.length = 25
)

model2 <- fit.acre(
  dat = acre_data,
  detfn = "hn",
  fix = list(g0=1)
)

summary(model2)

# variance

model2_boot <- boot.acre(model2, N = 10) # bootstrapping

acre::stdEr(model2_boot) # standard errors
confint(model2_boot,type="fitted") # confidence intervals

# SPATIAL TREND ------------------------------------------------------------------------

## ACRE Formatting ------------------------------------------------------------------------

lightfooti_data2 <- read.acre(
  captures = captures,
  traps = traps,
  mask = mask,
  loc_cov = cov_dat,
  cue.rates = call_rates/25,
  survey.length = 25
)

## Model Fitting ------------------------------------------------------------------------

model3 <- fit.acre(
  dat = lightfooti_data2,
  detfn = "hn", 
  par_extend_model = list(D=~x),
  fix = list(g0 = 1)
)

## Inference ---------------------------------------------------------------------------

# model output 

summary(model3)

# exponentiate regression coefficients

exp(model3$coefficients)[3]
exp(model3$coefficients)[4]

# variance

model3_boot <- boot.acre(model3, N = 10) # bootstrapping

acre::stdEr(model3_boot) # standard errors
confint(model3_boot) # confidence intervals

# detection function

show_detfn(model3)

# density 

show_Dsurf(model3) 

# save data as single RData object for tutorial
save(captures,traps,mask,lightfooti_data,
     call_rates,ave_call_rate,model1,model1_boot,
     model2, acre_data,model2_boot,
     lightfooti_data2, cov_dat,
     model3,
     file="data/CH5.RData")


