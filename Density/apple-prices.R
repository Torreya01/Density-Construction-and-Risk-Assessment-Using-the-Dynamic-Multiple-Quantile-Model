# Import libraries
library(DMQ)
library(np)
library(ks)
library(ggplot2)
library(moments)
library(dplyr)
library(lubridate)
library(PerformanceAnalytics)

######################## Time series plots for data ############################

# Import data of apple share prices
apple = read.csv("Data/apple_share_prices.csv")

# Turn apple$Date into Date object
apple$Date = as.Date(apple$Date)

# Convert the cleaned Price column to numeric
apple$Close = as.numeric(apple$Close)

# Set up the date limits
date_limits = range(apple$Date, na.rm = TRUE)

# Plot the data using ggplot
ggplot(data = apple, aes(x = Date, y = Close, group = 1)) + 
  geom_line() +
  labs(title = "apple Share Price - Daily Closing Prices (2014-2024)",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = date_limits)

# Use built-in R function to plot the time series
original.ts = ts(apple$Close, frequency = 260, start = c(2014,7,14))
plot(original.ts, xlab = "Time", ylab = "Close")

############################### Log-returns ####################################

# Make a new data column that contains the differences between each log entry
apple$log = log(apple$Close)
apple_diff = apple[-1, ]
apple_diff = diff(apple$log)

# Plot the time series of the log returns 
return.ts = ts(apple_diff, frequency = 260, start = c(2014,7,14))
plot(return.ts, xlab = "Time", ylab = "Index")

############################## Density plots ###################################

# Set the tau values
vTau = seq(0.01, 0.99, length.out = 99)

# Fit the data into model
Fit_apple = EstimateDMQ(vY = apple_diff, vTau = vTau, 
                       iTau_star = 50,  # Median as reference
                       FixReference = TRUE,
                       fn.optimizer = fn.solnp)

# Save the fit to RDS file
saveRDS(Fit_apple, file = "Data/Fit_apple.rds")

# Read the RDS file fit
Fit_apple = readRDS(file = "Data/Fitted_RDS/Fit_apple.rds")

# Extract the quantiles from the fit and eliminate the first column
quantiles = Fit_apple$lFilter$mQ
quantiles = quantiles[,-1]
apple = apple[-1,]

# Select four random dates to see what the density plots look like
selected_columns = c(300, 1300, 2000, 2516)
mQ_days = quantiles[, selected_columns]

# Select a whole month 6/5 to 7/5 and see what the density plots look like
selected_month = c(2515, 2516)
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
       main = paste("Denity plot for apple on", apple[selected_columns[i], "Date"]))
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
       main = paste("Denity plot for apple on", apple[selected_month[i], "Date"]))
}

################################## Forecast ####################################

# One-step ahead
Forecast_apple_1 = ForecastDMQ(Fit_apple, 1)

# Twp-step ahead
Forecast_apple_2 = ForecastDMQ(Fit_apple, 2)

# Three-step ahead
Forecast_apple_3 = ForecastDMQ(Fit_apple, 3)

# Four-step ahead
Forecast_apple_4 = ForecastDMQ(Fit_apple, 4)

# Five-step ahead
Forecast_apple_5 = ForecastDMQ(Fit_apple, 5)

# 30-steps ahead
Forecast_apple_30 = ForecastDMQ(Fit_apple, 30)

########################### Forecast density plots #############################

# Plot the forecast density plots
par(mfrow = c(5,1))

for (i in 1:5) {
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(Forecast_apple_5[i, ], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(Forecast_apple_5[i, ], bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$Forecast_apple_5, Density = kde_ls$dens)
  
  # Plotting using ggplot2
  plot(kde_ls_frame, type = "l", 
       main = paste("Forecast denity plot for apple on", as.Date(apple$Date[nrow(apple)]) + i + 2))
}

############################# Kurtosis and skewness ############################

# Generate blank kurtosis and skewness for the 5 forecasts
kurtosis = rep(0, 5)
skewness = rep(0, 5)

# Calculate kurtosis and skewness
for (i in 1:5) {
  k = kurtosis(Forecast_apple_5[i, ])
  s = skewness(Forecast_apple_5[i, ])
  kurtosis[i] = k
  skewness[i] = s
}

kurtosis
skewness

################# Compare the existing result with the forecast ###############
# Load the new data set
apple_new = read.csv("Data/apple_share_prices_new.csv")

# Turn apple$Date into Date object
apple_new$Date = as.Date(apple_new$Date)

# Convert the cleaned Price column to numeric
apple_new$Close = as.numeric(apple_new$Close)

# Set up the date limits
date_limits_new = range(apple_new$Date, na.rm = TRUE)

# Plot the data using ggplot
ggplot(data = apple_new, aes(x = Date, y = Close, group = 1)) + 
  geom_line() +
  labs(title = "apple Share Price - Daily Closing Prices (2014-2024)",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = date_limits_new)

original.ts = ts(apple$Close, frequency = 260, start = c(2014,7,14))
plot(original.ts, xlab = "Time", ylab = "Close")

############################### Log-returns ####################################

# Make a new data column that contains the differences between each log entry
apple_new$log = log(apple_new$Close)
apple_diff_new = apple_new[-1, ]
apple_diff_new = diff(apple_new$log)

# Plot the time series of the log returns 
return.ts = ts(apple_diff_new, frequency = 260, start = c(2014,7,14))
plot(return.ts, xlab = "Time", ylab = "Close")

# Fit the data into model
Fit_apple_new = EstimateDMQ(vY = apple_diff_new, vTau = vTau, 
                        iTau_star = 50,  # Median as reference
                        FixReference = TRUE,
                        fn.optimizer = fn.solnp)

# Save the fit to RDS file
saveRDS(Fit_apple_new, file = "Data/Fitted_RDS/Fit_apple_new.rds")

# Read the RDS file fit
Fit_apple_new = readRDS(file = "Data/Fitted_RDS/Fit_apple_new.rds")

# Extract the quantiles from the fit and eliminate the first column
quantiles_new = Fit_apple_new$lFilter$mQ
quantiles_new = quantiles_new[,-1]
apple_new = apple_new[-1, ]

# Select the last 5 days of the data and plot density
compare_select = c(2517, 2518, 2519, 2520, 2521)
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
       main = paste("Denity plot for apple on", apple_new[compare_select[i], "Date"]))
  
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls_forecast = npudensbw(Forecast_apple_5[i, ], ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls_forecast = npudens(Forecast_apple_5[i, ], bws = bw_ls_forecast, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame_forecast = data.frame(Quantile = kde_ls_forecast$eval$Forecast_apple_5, Density = kde_ls_forecast$dens)
  
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

MSE(Forecast_apple_5, mQ_compare)

############################## Yearly density pot ###############################

# Extract the year from the Date column
apple$Year = year(apple$Date)
apple$Month = month(apple$Date)

# Iterate through each year and month
for (year in 2014:2024) {
  for (month in 1:12) {
    
    # Create the name for the variable
    var_name = paste(year, month, sep = "_")
    
    # Find the indices for the current year and month in data1
    indices = which(apple$Year == as.character(year) & apple$Month == month)
    
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
saveRDS(kde_results, file = "Data/Fitted_RDS/kde_result.rds")

# Read the kde results
kde_results = readRDS(file = "Data/Fitted_RDS/kde_result.rds")

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

kde_2019_1 = kde_all %>% filter(Year == 2019) %>% filter(Month == 1)
kde_2019_2 = kde_all %>% filter(Year == 2019) %>% filter(Month == 2)
kde_2019_3 = kde_all %>% filter(Year == 2019) %>% filter(Month == 3)
kde_2019_4 = kde_all %>% filter(Year == 2019) %>% filter(Month == 4)
kde_2019_5 = kde_all %>% filter(Year == 2019) %>% filter(Month == 5)
kde_2019_6 = kde_all %>% filter(Year == 2019) %>% filter(Month == 6)
kde_2019_7 = kde_all %>% filter(Year == 2019) %>% filter(Month == 7)
kde_2019_8 = kde_all %>% filter(Year == 2019) %>% filter(Month == 8)
kde_2019_9 = kde_all %>% filter(Year == 2019) %>% filter(Month == 9)
kde_2019_10 = kde_all %>% filter(Year == 2019) %>% filter(Month == 10)
kde_2019_11 = kde_all %>% filter(Year == 2019) %>% filter(Month == 11)
kde_2019_12 = kde_all %>% filter(Year == 2019) %>% filter(Month == 12)


kde_2020_1 = kde_all %>% filter(Year == 2020) %>% filter(Month == 1)
kde_2020_2 = kde_all %>% filter(Year == 2020) %>% filter(Month == 2)
kde_2020_3 = kde_all %>% filter(Year == 2020) %>% filter(Month == 3)
kde_2020_4 = kde_all %>% filter(Year == 2020) %>% filter(Month == 4)
kde_2020_5 = kde_all %>% filter(Year == 2020) %>% filter(Month == 5)
kde_2020_6 = kde_all %>% filter(Year == 2020) %>% filter(Month == 6)
kde_2020_7 = kde_all %>% filter(Year == 2020) %>% filter(Month == 7)
kde_2020_8 = kde_all %>% filter(Year == 2020) %>% filter(Month == 8)
kde_2020_9 = kde_all %>% filter(Year == 2020) %>% filter(Month == 9)
kde_2020_10 = kde_all %>% filter(Year == 2020) %>% filter(Month == 10)
kde_2020_11 = kde_all %>% filter(Year == 2020) %>% filter(Month == 11)
kde_2020_12 = kde_all %>% filter(Year == 2020) %>% filter(Month == 12)

plot(kde_2020_10$Quantile, kde_2020_10$Density, type = "l")

# Calculate the 95th percentile threshold
threshold_11 = quantile(kde_2020_10$Quantile, 0.95)

# Select the top 5% of data values
upper_tail_data_11 = kde_2020_10 %>% filter(kde_2020_10$Quantile > threshold_11)

# Compute optimal bandwidth using cross-validation on Maximum Likelihood
bw_ls = npudensbw(upper_tail_data_11$Quantile, ckertype = "gaussian", bwmethod = "cv.ls")
  
# Perform kernel density estimation using the optimal bandwidth
kde_ls = npudens(upper_tail_data_11$Quantile, bws = bw_ls, ckertype = "gaussian")
  
# Convert KDE result to data frame for plotting
kde_ls_frame = data.frame(Quantile = kde_ls$eval$upper_tail_data_1, Density = kde_ls$dens)
  
# Add lines to the original density plot
plot(kde_ls_frame$Quantile, kde_ls_frame$Density, type = "l")

################################### QQ plot ####################################

qqnorm(kde_ls_frame$Quantile, main = "QQ Plot")
qqline(kde_ls_frame$Quantile, col = "red")

# Do the whole data qq plot
# reference on the lecture notes about EVT/tail behaviour in finance
# Tail bahaviour/heavy tail/ finance

# student-t/ low degree of freedom/ heavy tailed
# one-step can be Guassian and h-step ahead can be heavy-tailed

########################### A month forecast plot ##############################

# Make the a month forecasts into a whole list
Forecast_30 = sort(as.vector(Forecast_apple_30))
Forecast_5 = sort(as.vector(Forecast_apple_5))

# Plot the forecasts into a density plot
bw_ls = npudensbw(Forecast_5, ckertype = "gaussian", bwmethod = "cv.ls")

# Perform kernel density estimation using the optimal bandwidth
kde_ls = npudens(Forecast_5, bws = bw_ls, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_ls_frame = data.frame(Quantile = kde_ls$eval$Forecast_5, Density = kde_ls$dens)

# Add lines to the original density plot
plot(kde_ls_frame$Quantile, kde_ls_frame$Density, type = "l")

################################# VaR and ES ###################################

# Calculate Value at Risk (VaR)
VaR(Forecast_5, p = 0.95, method = "historical")

# Calculate Expected Shortfall (ES)
ES(Forecast_5, p = 0.95, method = "historical")

# Add this


