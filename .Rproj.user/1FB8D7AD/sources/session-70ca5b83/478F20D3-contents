# Import libraries
library(DMQ)
library(np)
library(ks)
library(ggplot2)
library(gridExtra)
library(moments)
library(dplyr)

######################## Time series plots for data ############################

# Import data of FTSE share prices
FTSE = read.csv("Data/FTSE_100.csv")

# Apply the function to the Date column
FTSE$Date = as.Date(FTSE$Date, format = "%m/%d/%y")

# Sort the dates into the correct order
FTSE$Date = sort(FTSE$Date)

# Step 3: Convert the cleaned Price column to numeric
FTSE$Close = as.numeric(FTSE$Close)

# Set up the date limits
date_limits = range(FTSE$Date, na.rm = TRUE)

# Plot the data using ggplot
ggplot(data = FTSE, aes(x = Date, y = Close, group = 1)) + 
  geom_line() +
  labs(title = "FTSE Share Price - Daily Closing Prices (2014-2024)",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = date_limits)

############################### Log-returns ####################################

# Make a new data column that contains the differences between each log entry
FTSE$log = log(FTSE$Close)
FTSE_diff = FTSE[-1, ]
FTSE_diff = diff(FTSE$log)

# Plot the time series of the log returns 
return.ts = ts(FTSE_diff, frequency = 260, start = c(2014,7,14))
plot(return.ts, xlab = "Time", ylab = "Index")

############################## Density plots ###################################

# Set the tau values
vTau = seq(0.01, 0.99, length.out = 99)

# Fit the data into model
Fit_FTSE = EstimateDMQ(vY = FTSE_diff, vTau = vTau, 
                  iTau_star = 50,  # Median as reference
                  FixReference = TRUE,
                  fn.optimizer = fn.solnp)

# Save the fit to RDS file
saveRDS(Fit_FTSE, file = "Data/Fitted_RDS/Fit_FTSE.rds")

# Read the RDS file fit
Fit_FTSE = readRDS(file = "Data/Fitted_RDS/Fit_FTSE.rds")

# Extract the quantiles from the fit and eliminate the first column
quantiles = Fit_FTSE$lFilter$mQ
quantiles = quantiles[,-1]
FTSE = FTSE[-1, ]

# Select four random dates to see what the density plots look like
selected_columns = c(300, 1300, 2000, 2576)
mQ_days = quantiles[, selected_columns]

# Select a whole month 6/12 to 7/12 and see what the density plots look like
selected_month = seq(2570, 2576, 1)
mQ_month = quantiles[, selected_month]

# Plot the random selected density plots into a four plot column
par(mfrow = c(4,1))

for (i in 1:length(selected_columns)) {
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(mQ_days[, i], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(mQ_days[, i], bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$mQ_days, Density = kde_ls$dens)
  
  # Plotting using ggplot2
  plot(kde_ls_frame, type = "l", 
       main = paste("Denity plot for FTSE on", FTSE[selected_columns[i], "Date"]))
}

# Plot a month density plots into a four plot column
par(mfrow = c(4,1))

for (i in 1:length(selected_month)) {
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(mQ_month[, i], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(mQ_month[, i], bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$mQ_month, Density = kde_ls$dens)
  
  # Plotting using ggplot2
  plot(kde_ls_frame, type = "l", 
       main = paste("Denity plot for FTSE on", FTSE[selected_month[i], "Date"]))
}

################################## Forecast ####################################

# One-step ahead
Forecast_FTSE_1 = ForecastDMQ(Fit_FTSE, 1)

# Twp-step ahead
Forecast_FTSE_2 = ForecastDMQ(Fit_FTSE, 2)

# Three-step ahead
Forecast_FTSE_3 = ForecastDMQ(Fit_FTSE, 3)

# Four-step ahead
Forecast_FTSE_4 = ForecastDMQ(Fit_FTSE, 4)

# Five-step ahead
Forecast_FTSE_5 = ForecastDMQ(Fit_FTSE, 5)

########################### Forecast density plots #############################

# Plot the forecast density plots
par(mfrow = c(5,1))

for (i in 1:5) {
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(Forecast_FTSE_5[i, ], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(Forecast_FTSE_5[i, ], bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$Forecast_FTSE_5, Density = kde_ls$dens)
  
  # Plotting using ggplot2
  plot(kde_ls_frame, type = "l", 
       main = paste("Forecast denity plot for FTSE on", as.Date(FTSE$Date[nrow(FTSE)]) + i + 2))
}

############################# Kurtosis and skewness ############################

# Generate blank kurtosis and skewness for the 5 forecasts
kurtosis = rep(0, 5)
skewness = rep(0, 5)

# Calculate kurtosis and skewness
for (i in 1:5) {
  k = kurtosis(Forecast_FTSE_5[i, ])
  s = skewness(Forecast_FTSE_5[i, ])
  kurtosis[i] = k
  skewness[i] = s
}

kurtosis
skewness

################# Compare the existing result with the forecast ###############
# Load the new data set
FTSE_new = read.csv("Data/FTSE_100_new.csv")

# Apply the function to the Date column
FTSE_new$Date = as.Date(FTSE_new$Date, format = "%m/%d/%y")

# Sort the dates into the correct order
FTSE_new$Date = sort(FTSE_new$Date)

# Convert the cleaned Price column to numeric
FTSE_new$Close = as.numeric(FTSE_new$Close)

# Set up the date limits
date_limits_new = range(FTSE_new$Date, na.rm = TRUE)

# Plot the data using ggplot
ggplot(data = FTSE_new, aes(x = Date, y = Close, group = 1)) + 
  geom_line() +
  labs(title = "FTSE Share Price - Daily Closing Prices (2014-2024)",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = date_limits_new)

original.ts = ts(FTSE$Close, frequency = 260, start = c(2014,7,14))
plot(original.ts, xlab = "Time", ylab = "Close")

############################### Log-returns ####################################

# Make a new data column that contains the differences between each log entry
FTSE_new$log = log(FTSE_new$Close)
FTSE_diff_new = FTSE_new[-1, ]
FTSE_diff_new = diff(FTSE_new$log)

# Plot the time series of the log returns 
return.ts = ts(FTSE_diff_new, frequency = 260, start = c(2014,7,14))
plot(return.ts, xlab = "Time", ylab = "Close")

# Fit the data into model
Fit_FTSE_new = EstimateDMQ(vY = FTSE_diff_new, vTau = vTau, 
                            iTau_star = 50,  # Median as reference
                            FixReference = TRUE,
                            fn.optimizer = fn.solnp)

# Save the fit to RDS file
saveRDS(Fit_FTSE_new, file = "Data/Fitted_RDS/Fit_FTSE_new.rds")

# Read the RDS file fit
Fit_FTSE_new = readRDS(file = "Data/Fitted_RDS/Fit_FTSE_new.rds")

# Extract the quantiles from the fit and eliminate the first column
quantiles_new = Fit_FTSE_new$lFilter$mQ
quantiles_new = quantiles_new[,-1]
FTSE_new = FTSE_new[-1, ]

# Select the last 5 days of the data and plot density
compare_select = c(2577, 2578, 2579, 2580, 2581)
mQ_compare = quantiles_new[, compare_select]

# Plot the random selected density plots into a four plot column
par(mfrow = c(5,1))

# Plot the actual results of the forecast results from the DMQ model
for (i in 1:5) {
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(mQ_compare[, i], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(mQ_compare[, i], bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$mQ_compare, Density = kde_ls$dens)
  
  # Plotting using ggplot2
  plot(kde_ls_frame, type = "l", 
       main = paste("Denity plot for FTSE on", FTSE_new[compare_select[i], "Date"]))
  
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls_forecast = npudensbw(Forecast_FTSE_5[i, ], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls_forecast = npudens(Forecast_FTSE_5[i, ], bws = bw_ls_forecast, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame_forecast = data.frame(Quantile = kde_ls_forecast$eval$Forecast_FTSE_5, Density = kde_ls_forecast$dens)
  
  # Plotting using ggplot2
  lines(kde_ls_frame_forecast, type = "l", col = "blue")
}

# Calculate the MSE to evaluate the differences between forecast and reality
MSE = function(data1, data2){
  mse = rep(0, 5)
  for (i in seq(1:5)){
    bw_1 = npudensbw(data1[i, ], ckertype = "gaussian", bwmethod = "cv.ls") 
    kde_1 = npudens(data1[i, ], bws = bw_1, ckertype = "gaussian")
    kde_frame_1 = data.frame(Quantile = kde_1$eval$data1, Density = kde_1$dens)
    bw_2 = npudensbw(data2[, i], ckertype = "gaussian", bwmethod = "cv.ls") 
    kde_2 = npudens(data2[, i], bws = bw_2, ckertype = "gaussian")
    kde_frame_2 = data.frame(Quantile = kde_2$eval$data2, Density = kde_2$dens)
    estimated_density = kde_frame_1$Density
    estimated_density
    true_density = kde_frame_2$Density
    true_density
    mse[i] = mean((true_density - estimated_density)^2)
  }
  return(mse)
}

MSE(Forecast_FTSE_5, mQ_compare)

############################## Yearly density pot ###############################

# Extract the year from the Date column
FTSE$Year = year(FTSE$Date)
FTSE$Month = month(FTSE$Date)

# Iterate through each year and month
for (year in 2014:2024) {
  for (month in 1:12) {
    
    # Create the name for the variable
    var_name = paste(year, month, sep = "_")
    
    # Find the indices for the current year and month in data1
    indices = which(FTSE$Year == as.character(year) & FTSE$Month == month)
    
    # Select data from data2 using the indices
    selected_data = sort(as.vector(quantiles[, unlist(indices)]))
    
    # Assign the selected data to a dynamically named variable
    assign(var_name, selected_data)
  }
}

# Store dataset names in a vector
dataset_names = c("2014_6", "2014_7", "2014_8", "2014_9", "2014_10",
                  "2014_10", "2014_11", "2014_12", "2015_1", "2015_2", 
                  "2015_3", "2015_4", "2015_5", "2015_6", "2015_7", 
                  "2015_8", "2015_9", "2015_10", "2015_11", "2015_12",
                  "2016_1", "2016_2", "2016_3", "2016_4", "2016_5",
                  "2016_6", "2016_7", "2016_8", "2016_9", "2016_10", 
                  "2016_11", "2016_12", "2017_1", "2017_2", "2017_3", 
                  "2017_4", "2017_5", "2017_6", "2017_7", "2017_8", 
                  "2017_9", "2017_10", "2017_11", "2017_12", "2018_1",
                  "2018_2", "2018_3", "2018_4", "2018_5", "2018_6",
                  "2018_7", "2018_8", "2018_9", "2018_10", "2018_11", 
                  "2018_12", "2019_1", "2019_2", "2019_3",
                  "2019_4", "2019_5", "2019_6", "2019_7", "2019_8",
                  "2019_9", "2019_10", "2019_11", "2019_12", "2020_1",
                  "2020_2", "2020_3", "2020_4", "2020_5", "2020_6", 
                  "2020_7", "2020_8", "2020_9" ,"2020_10", "2020_11",
                  "2020_12", "2021_1", "2021_2", "2021_3", "2021_4",
                  "2021_5", "2021_6", "2021_7", "2021_8", "2021_9", 
                  "2021_10", "2021_11", "2021_12", "2022_1", "2022_2",
                  "2022_3", "2022_4", "2022_5", "2022_6", "2022_7", 
                  "2022_8", "2022_9", "2022_10", "2022_11", "2022_12",
                  "2023_1", "2023_2", "2023_3", "2023_4", "2023_5",
                  "2023_6", "2023_7", "2023_8", "2023_9", "2023_10", 
                  "2023_11", "2023_12", "2024_1", "2024_2", "2024_3",
                  "2024_4", "2024_5", "2024_6") 

# Plot the yearly density plots

# Initialize a list to store the KDE results
kde_results = list()

# Loop through the dataset names and compute KDE
for (name in dataset_names) {
  # Retrieve the dataset using the name
  data = get(name)
  
  # Extract the year and month from the dataset name
  year = substr(name, 1, 4)
  month = substr(name, 6, 7)
  
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(data, ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(data, bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$data, Density = kde_ls$dens, Year = year, Month = month)
  
  # Store the KDE result in the list
  kde_results[[paste(year, month, sep = "_")]] = kde_ls_frame
}

# Save the kde results into RDS file
saveRDS(kde_results, file = "Data/Fitted_RDS/kde_result_FTSE.rds")

# Read the kde results
kde_results = readRDS(file = "Data/Fitted_RDS/kde_result_FTSE.rds")

# Combine all KDE results into a single data frame
kde_all = do.call(rbind, kde_results)

# Define the color palette
colors = c("1" = "black", "2" = "blue", "3" = "red", "4" = "green", 
           "5" = "purple", "6" = "yellow", "7" = "orange", "8" = "pink", 
           "9" = "cyan", "10" = "brown", "11" = "magenta", "12" = "turquoise")

# List of months to plot
months = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
years = c("2014", "2015", "2016", "2017", "2018", 
          "2019", "2020", "2021", "2022", "2023", "2024")

# Loop through each month and create the plots
for (year in years) {
  # Subset the data for the current year
  kde_year = kde_all %>% filter(Year == year)
  # Create the plot
  p = ggplot(kde_year, aes(x = Quantile, y = Density, color = Month, group = Month)) +
    geom_line(linewidth = 1) +
    labs(title = paste("Density Plot for Year", year),
         x = "Quantile", y = "Density") +
    theme_minimal() +
    theme(legend.title = element_blank()) + 
    scale_color_manual(values = colors, breaks = months)
  
  # Print the plot
  print(p)
}
