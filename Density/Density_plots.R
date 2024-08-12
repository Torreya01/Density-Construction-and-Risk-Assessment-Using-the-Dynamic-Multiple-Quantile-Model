# Import libraries
library(DMQ)
library(np)
library(ks)

# Source functions in source.R
source("Density/source.R")

################################## Apple Inc ###################################

# Read the cleaned data
apple = read.csv("Data/Derived/apple_cleaned.csv")

# Read the RDS file fit
Fit_apple = readRDS(file = "Data/Fitted_RDS/Fit_apple.rds")

# Extract the quantiles from the fit and eliminate the first column
quantiles_apple = Fit_apple$lFilter$mQ
quantiles_apple = quantiles_apple[,-1]
apple = apple[-1, ]
