# Chapter 5: aSCR-CD setup script ---------------------------------------------------------------------

# libraries -------------------------------------------------------------------------------------------

libraries <- c("learnr","acre","ggplot2","xfun","kableExtra","xfun","secr")

lapply(libraries, require, character.only = TRUE)

# load data -------------------------------------------------------------------------------------------

# loading data from lightfooti_basic.R 
# List of objects in lightfooti_data.Rdata: 
# captures dataframe 
# traps dataframe
# created mask buffer 15
# lightfooti_data (read.acre data object)
# model1 fit
# call rates in per minute
# average call rate 
# model1_boot
# lightfooti_data2 (read.acre data object 2)
# model2
# model2_boot

load("data/CH5.RData")

