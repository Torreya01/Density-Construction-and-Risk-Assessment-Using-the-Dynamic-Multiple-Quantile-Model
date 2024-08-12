# Import libraries
library(DMQ)
library(np)
library(ks)

# Source functions in source.R
source("Density/source.R")

################################## Apple Inc ###################################

# Read the cleaned data
apple = read.csv("Data/Derived/apple_cleaned.csv")
apple_new = read.csv("Data/Derived/apple_new_cleaned.csv")

# Read the RDS file fit
Fit_apple = readRDS(file = "Data/Fitted_RDS/Fit_apple.rds")
Fit_apple_new = readRDS(file = "Data/Fitted_RDS/Fit_apple_new.rds")

# Five-step ahead forecast
Forecast_apple_5 = ForecastDMQ(Fit_apple, 5)

# Extract the quantiles from the fit and eliminate the first column
quantiles_apple_new = Fit_apple_new$lFilter$mQ
quantiles_apple_new = quantiles_apple_new[,-1]
apple_new = apple_new[-1, ]

# Select the last 5 days of the data and plot density
apple_compare_select = c(2517, 2518, 2519, 2520, 2521)
apple_mQ_compare = quantiles_apple_new[, apple_compare_select]

# Plot the forecast density plots
par(mfrow = c(5,1))

# Plot the actual results of the forecast results from the DMQ model
for (i in 1:5) {
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(apple_mQ_compare[, i], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(apple_mQ_compare[, i], bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$apple_mQ_compare, Density = kde_ls$dens)
  
  # Plotting using ggplot2
  plot(kde_ls_frame, type = "l", 
       main = paste("Forecasted and estimated denity plot for Apple Inc. on", apple_new[apple_compare_select[i], "Date"]))
  
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls_forecast = npudensbw(Forecast_apple_5[i, ], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls_forecast = npudens(Forecast_apple_5[i, ], bws = bw_ls_forecast, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame_forecast = data.frame(Quantile = kde_ls_forecast$eval$Forecast_apple_5, Density = kde_ls_forecast$dens)
  
  # Plotting using ggplot2
  lines(kde_ls_frame_forecast, type = "l", col = "blue")
}

# Calculate the MSE between the forecast and true density
MSE(Forecast_apple_5, apple_mQ_compare)

# Plot a week of forecast 
# Make the a week forecasts into a whole list
Forecast_apple = sort(as.vector(Forecast_apple_5))

# Plot the forecasts into a density plot
bw_ls = npudensbw(Forecast_apple, ckertype = "gaussian", bwmethod = "cv.ls")

# Perform kernel density estimation using the optimal bandwidth
kde_ls = npudens(Forecast_apple, bws = bw_ls, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_ls_frame = data.frame(Quantile = kde_ls$eval$Forecast_apple, Density = kde_ls$dens)

# Create density plot using ggplot2
ggplot(kde_ls_frame, aes(x = Quantile, y = Density)) +
  geom_line() +
  labs(title = "A Week of forecast density on Apple Inc. from 24th June, 2024 to 28th June, 2024", x = "Quantile", y = "Density") +
  theme_minimal()

################################## FTSE 100 ####################################

# Read the cleaned data
FTSE = read.csv("Data/Derived/FTSE_cleaned.csv")
FTSE_new = read.csv("Data/Derived/FTSE_new_cleaned.csv")

# Read the RDS file fit
Fit_FTSE = readRDS(file = "Data/Fitted_RDS/Fit_FTSE.rds")
Fit_FTSE_new = readRDS(file = "Data/Fitted_RDS/Fit_FTSE_new.rds")

# Five-step ahead forecast
Forecast_FTSE_5 = ForecastDMQ(Fit_FTSE, 5)

# Extract the quantiles from the fit and eliminate the first column
quantiles_FTSE_new = Fit_FTSE_new$lFilter$mQ
quantiles_FTSE_new = quantiles_FTSE_new[,-1]
FTSE_new = FTSE_new[-1, ]

# Select the last 5 days of the data and plot density
FTSE_compare_select = c(2577, 2578, 2579, 2580, 2581)
FTSE_mQ_compare = quantiles_FTSE_new[, FTSE_compare_select]

# Plot the random selected density plots into a four plot column
par(mfrow = c(5,1))

# Plot the actual results of the forecast results from the DMQ model
for (i in 1:5) {
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(FTSE_mQ_compare[, i], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(FTSE_mQ_compare[, i], bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$FTSE_mQ_compare, Density = kde_ls$dens)
  
  # Plotting using ggplot2
  plot(kde_ls_frame, type = "l", 
       main = paste("Forecasted and estimated denity plot for FTSE 100 Index on", FTSE_new[FTSE_compare_select[i], "Date"]))
  
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls_forecast = npudensbw(Forecast_FTSE_5[i, ], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls_forecast = npudens(Forecast_FTSE_5[i, ], bws = bw_ls_forecast, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame_forecast = data.frame(Quantile = kde_ls_forecast$eval$Forecast_FTSE_5, Density = kde_ls_forecast$dens)
  
  # Plotting using ggplot2
  lines(kde_ls_frame_forecast, type = "l", col = "blue")
}

# Calculate the MSE between the forecast and true density
MSE(Forecast_FTSE_5, FTSE_mQ_compare)

# Plot a week of forecast 
# Make the a week forecasts into a whole list
Forecast_FTSE = sort(as.vector(Forecast_FTSE_5))

# Plot the forecasts into a density plot
bw_ls = npudensbw(Forecast_FTSE, ckertype = "gaussian", bwmethod = "cv.ls")

# Perform kernel density estimation using the optimal bandwidth
kde_ls = npudens(Forecast_FTSE, bws = bw_ls, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_ls_frame = data.frame(Quantile = kde_ls$eval$Forecast_FTSE, Density = kde_ls$dens)

# Create density plot using ggplot2
ggplot(kde_ls_frame, aes(x = Quantile, y = Density)) +
  geom_line() +
  labs(title = "A Week of forecast density on FTSE 100 Index. from 24th June, 2024 to 28th June, 2024", x = "Quantile", y = "Density") +
  theme_minimal()

