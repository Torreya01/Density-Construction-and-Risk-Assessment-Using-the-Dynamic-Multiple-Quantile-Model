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
plot(return.ts, xlab = "Time", ylab = "Index", 
     main = "log-returns of FTSE 100 (2014-2024)")

summary_stats(FTSE_diff)

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
quantiles_FTSE = Fit_FTSE$lFilter$mQ
quantiles_FTSE = quantiles_FTSE[,-1]
FTSE = FTSE[-1, ]

# Select four random dates to see what the density plots look like
selected_columns = c(300, 1300, 2000, 2576)
mQ_days = quantiles_FTSE[, selected_columns]

# Select a whole month 6/12 to 7/12 and see what the density plots look like
selected_month = seq(2570, 2576, 1)
mQ_month = quantiles_FTSE[, selected_month]

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
quantiles_new_FTSE = Fit_FTSE_new$lFilter$mQ
quantiles_new_FTSE = quantiles_new_FTSE[,-1]
FTSE_new = FTSE_new[-1, ]

# Select the last 5 days of the data and plot density
compare_select = c(2577, 2578, 2579, 2580, 2581)
mQ_compare = quantiles_new_FTSE[, compare_select]

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
       main = paste("Forecasted and estimated denity plot for FTSE 100 Index on", FTSE_new[compare_select[i], "Date"]))
  
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

############################## Monthly density pot ###############################

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
    selected_data = sort(as.vector(quantiles_FTSE[, unlist(indices)]))
    
    # Assign the selected data to a dynamically named variable
    assign(var_name, selected_data)
  }
}

# Store dataset names in a vector
dataset_names = c("2014_6", "2014_7", "2014_8", "2014_9", "2014_10",
                  "2014_11", "2014_12", "2015_1", "2015_2", 
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

dataset = c("2020_1","2020_2", "2020_3", "2020_4", "2020_5", "2020_6", 
            "2020_7", "2020_8", "2020_9" ,"2020_10", "2020_11", "2020_12")

# Plot the yearly density plots

# Initialize a list to store the KDE results
kde_result_FTSE = list()

# Loop through the dataset names and compute KDE
for (name in dataset) {
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
  kde_result_FTSE[[paste(year, month, sep = "_")]] = kde_ls_frame
}

# Save the kde results into RDS file
saveRDS(kde_result_FTSE, file = "Data/Fitted_RDS/kde_result_FTSE.rds")

# Read the kde results
kde_result_FTSE = readRDS(file = "Data/Fitted_RDS/kde_result_FTSE.rds")

# Combine all KDE results into a single data frame
kde_all_FTSE = do.call(rbind, kde_result_FTSE)

# Define the color palette with named colors corresponding to month names
colors <- c("1" = "#FFA500",  # Orange
            "2" = "#9ACD32",  # YellowGreen
            "3" = "#008000",  # Green
            "4" = "#0000FF",  # Blue
            "5" = "#1E90FF",  # DodgerBlue
            "6" = "#00BFFF",  # DeepSkyBlue
            "7" = "#00FFFF",  # Cyan
            "8" = "#FF00FF",  # Magenta
            "9" = "#800080",  # Purple
            "10" = "#FFC0CB", # Pink
            "11" = "#FF7F50", # Coral
            "12" = "#FF0000") # Red
names(colors) = month.name  # Ensure that colors are named by month names for easy referencing

# List of months to plot
months = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
years = c("2014", "2015", "2016", "2017", "2018", 
          "2019", "2020", "2021", "2022", "2023", "2024")

# Loop through each month and create the plots
for (year in years) {
  # Subset the data for the current year
  kde_year = kde_all_FTSE %>% dplyr::filter(Year == year)
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


# Prepare the data by ensuring Month and Month_num are correctly formatted
kde_all_FTSE$Month <- factor(kde_all_FTSE$Month, levels = 1:12, labels = month.name)
kde_all_FTSE$Month_num <- as.numeric(kde_all_FTSE$Month)


kde_2020_7_apple$Quantile[which(kde_2020_7_apple$Density == max(kde_2020_7_apple$Density))]

# Add the camera eye
camera_eye <- list(x = -1, y = 2, z = 0.2) 

for (year in years) {
  # Subset the data for the current year
  kde_year <- filter(kde_all_FTSE, Year == year)
  
  # Initialize an empty plotly object
  p <- plot_ly() 
  
  # Add each month as a separate trace
  for (m in months) {
    month_data <- filter(kde_year, Month_num == m)
    p <- add_trace(p, data = month_data, x = ~Quantile, y = ~Month, z = ~Density,
                   type = 'scatter3d', mode = 'lines',
                   line = list(color = colors[m], width = 4),
                   name = m, 
                   showlegend = TRUE)  # Use month names directly for the legend
  }
  
  # Layout with camera and legend configuration
  p <- p %>% layout(#title = paste("Qauntile Density Plot in Year", year, "for Apple Inc log-returns"),
    scene = list(
      xaxis = list(title = "Quantile"),
      yaxis = list(title = "Month"),
      zaxis = list(title = "Density"), 
      camera = list(eye = list(x = -1, y = 2, z = 0.1))), 
    width = 700,  # Specify width as 1000 pixels
    height = 560)
  
  # Print the plot
  print(p)
}

############################# Extreme Value Theory #############################

# Calculate the tail value at risk
tvar = function(fit, p) {
  scale = fit$lse[1]
  shape = fit$lse[2]
  threshold = fit$threshold
  return(threshold + (scale / shape) * ((1 - p)^(-shape) - 1))
}

# Set the threshold and extract the excess values from the right tail
right_2020_10 = quantile(Forecast_5$Quantile, 0.95)
right_excess_2020_10 = kde_2020_10 %>% dplyr::filter(kde_2020_10$Quantile > right_2020_10)

# Fitting a GPD to the excesses
gpd_fit_2020_10_right = gpd.fit(right_excess_2020_10$Quantile, right_2020_10)

# Generate QQ plot to assess the fit
gpd.diag(gpd_fit_2020_10_right)

# Simulate data from the GPD
simulated_data_2020_10_right = rgpd(109, loc = gpd_fit_2020_10_right$threshold, 
                                    scale = gpd_fit_2020_10_right$lse[1], 
                                    shape = gpd_fit_2020_10_right$lse[2])

# Calculate the KS score
ks.test(right_excess_2020_10$Quantile, simulated_data_2020_10_right)

# Calculate TVaR at the 95th percentile
tvar_95_right = tvar(gpd_fit_2020_10_right, 0.95)

# Extract the extreme quantiles from the left tail
left_2020_10 = quantile(kde_2020_10$Quantile, 0.05)
left_excess_2020_10 = kde_2020_10 %>% dplyr::filter(kde_2020_10$Quantile < left_2020_10)

# Fitting a GPD to the excesses
gpd_fit_2020_10_left = gpd.fit(left_excess_2020_10$Quantile*-1, left_2020_10*-1)

# Generate QQ plot to assess the fit
gpd.diag(gpd_fit_2020_10_left)

# Simulate data from the GPD
simulated_data_2020_10_left = rgpd(109, loc = gpd_fit_2020_10_left$threshold,
                                   scale = gpd_fit_2020_10_left$lse[1], 
                                   shape = gpd_fit_2020_10_left$lse[2])

# Calculate the KS score
ks.test(left_excess_2020_10$Quantile*-1, simulated_data_2020_10_left)

# Calculate TVaR at the 95th percentile
tvar_95_right = tvar(gpd_fit_2020_10_left, 0.95)

######################## Density plot of the whole data ########################

# Sort the FTSE_diff to be ascending
FTSE_new_sorted = sort(FTSE_diff)

# Use the same kernel and bandwidth to draw the density of the full data
bw_ls_full = npudensbw(FTSE_new_sorted, ckertype = "gaussian", bwmethod = "cv.ls")

# Perform kernel density estimation using the optimal bandwidth
kde_ls_full = npudens(FTSE_new_sorted, bws = bw_ls_full, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_ls_frame_full = data.frame(Quantile = kde_ls_full$eval$FTSE_new_sorted, Density = kde_ls_full$dens)

# Add lines to the original density plot
plot(kde_ls_frame_full$Quantile, kde_ls_frame_full$Density, type = "l", 
     main = "Density plot of the whole data")

########################### Recreation of the study ############################

# Change data into absolute values
abs_changes = abs(kde_2020_10$Quantile)

# Define a sequence of thresholds
thresholds = seq(from = min(kde_2020_10$Quantile), to = quantile(kde_2020_10$Quantile, 0.95), length.out = 100)

# Initialize a vector to store mean excess values
mean_excess = numeric(length(thresholds))

# Calculate mean excess for each threshold
for (i in seq_along(thresholds)) {
  exceedances <- kde_2020_10$Quantile[kde_2020_10$Quantile > thresholds[i]] - thresholds[i]
  if (length(exceedances) > 0) {
    mean_excess[i] <- mean(exceedances)
  } else {
    mean_excess[i] <- NA  # Assign NA if no exceedances
  }
}

# Plot the empirical mean excess function
plot(thresholds, mean_excess,
     xlab = "Threshold (u)", ylab = "Mean Excess (e(u))",
     main = "Empirical Mean Excess Function")

# Add threshold 
threshold = 0
abline(v = threshold, col = "red", lty = 2)
legend("topright", "Threshold", lty = 2, col = "red", bty = "n")

################################### QQ plot ####################################

# Plot the qq plot of the quantiles generated
qqnorm(kde_2020_10$Quantile, main = "QQ Plot for quantiles generated from October 2020")
qqline(kde_2020_10$Quantile, col = "red")
datawizard::kurtosis(kde_2020_10$Quantile)

# Plot the qq plot of the quantiles generated
qqnorm(kde_2020_12$Quantile, main = "QQ Plot for quantiles generated from December 2020")
qqline(kde_2020_12$Quantile, col = "red")
datawizard::kurtosis(kde_2020_12$Quantile)

# Plot the qq plot of the original data
qqnorm(FTSE_diff, main = "QQ Plot for the original log-returns for FTSE 100 Index")
qqline(FTSE_diff, col = "red")
datawizard::kurtosis(FTSE_diff)

################################ Kurtosis plot #################################

# Calculate the krutosis for each month in 2014 to 2024
kurtosis_list = kde_all_FTSE %>%
  group_by(Year, Month_num) %>%
  summarise(Kurtosis = kurtosis(Quantile, na.rm = TRUE) + 3) %>%
  ungroup() 

# Create a new column 'Date' for plotting
kurtosis_list$Date = with(kurtosis_list, as.Date(paste(Year, Month_num, "1", sep = "-")))
kurtosis_list = kurtosis_list[order(kurtosis_list$Date), ]

# Plotting the kurtosis
ggplot(kurtosis_list, aes(x = Date, y = Kurtosis)) +
  geom_line() +  # Connect points with lines
  geom_point() +  # Show points
  labs(title = "Monthly Kurtosis over Time for FTSE 100",
       x = "Date (Year)",
       y = "Kurtosis") +
  theme_minimal() + 
  scale_y_continuous(limits = c(3,7)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year")

# Print the kurtosis results
print(kurtosis_list)

# Plot the kurtosis
plot(kurtosis_list)

################################ Skewness plot #################################

# Calculate the krutosis for each month in 2014 to 2024
skewness_list = kde_all_FTSE %>%
  group_by(Year, Month_num) %>%
  summarise(skewness = skewness(Quantile, na.rm = TRUE)) %>%
  ungroup() 

# Create a new column 'Date' for plotting
skewness_list$Date = with(skewness_list, as.Date(paste(Year, Month_num, "1", sep = "-")))
skewness_list = skewness_list[order(skewness_list$Date), ]

# Plotting the kurtosis
ggplot(skewness_list, aes(x = Date, y = skewness)) +
  geom_line() +  # Connect points with lines
  geom_point() +  # Show points
  labs(title = "Monthly Skewness over Time for FTSE 100",
       x = "Date (Year)",
       y = "skewness") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-1, 1.5)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year")

############################ Daily Kurtosis plot ###############################

# Define the months and years for the analysis
months_years <- list(c(2022, 5), c(2023, 10), c(2018, 11), c(2022, 3), c(2018, 4), c(2021, 5))
months_years = list(c(2018, 10), c(2018, 11))
# Initialize a list to store the plots
plots <- list()

# Loop through each month and year
for (my in months_years) {
  year <- my[1]
  month <- my[2]
  
  # Filter indices for the specified month and year
  indices_name <- paste("indices", year, month, sep = "_")
  indices <- which(format(as.Date(FTSE$Date), "%Y-%m") == sprintf("%04d-%02d", year, month))
  assign(indices_name, indices)
  
  # Select quantiles data based on indices
  quantiles_name <- paste("quantiles", year, month, sep = "_")
  quantiles_selected <- quantiles_FTSE[,indices]
  assign(quantiles_name, quantiles_selected)
  
  # Calculate kurtosis for each column
  kurtosis_values <- apply(quantiles_selected, 2, kurtosis)
  
  # Select dates based on indices
  days_in_month <- as.Date(FTSE$Date[indices])
  
  # Create a data frame for plotting
  kurtosis_df <- data.frame(
    Day = as.Date(format(days_in_month, "%y/%m/%d")),
    Kurtosis = kurtosis_values + 3, 
    Month = factor(month, levels = 1:12, labels = month.name)
  )
  
  # Create the plot
  p <- ggplot(kurtosis_df, aes(x = Day, y = Kurtosis)) +
    geom_line() +  # Connect points with lines
    geom_point() +  # Show points
    labs(title = paste("FTSE 100 Daily Kurtosis over Time for", kurtosis_df$Month, "in", year),
         x = "Day",
         y = "Kurtosis") +
    theme_minimal() + 
    scale_x_date(labels = date_format("%d"), breaks = "5 days")
  
  # Store the plot in the list
  plots[[paste(year, month, sep = "_")]] <- p
}

# Display the plots
for (plot_name in names(plots)) {
  print(plots[[plot_name]])
}

############################ A week forecast plot ##############################

# Make the a week forecasts into a whole list
Forecast_5 = sort(as.vector(Forecast_FTSE_5))

# Plot the forecasts into a density plot
bw_ls = npudensbw(Forecast_5, ckertype = "gaussian", bwmethod = "cv.ls")

# Perform kernel density estimation using the optimal bandwidth
kde_ls = npudens(Forecast_5, bws = bw_ls, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_ls_frame = data.frame(Quantile = kde_ls$eval$Forecast_5, Density = kde_ls$dens)

# Create density plot using ggplot2
ggplot(kde_ls_frame, aes(x = Quantile, y = Density)) +
  geom_line() +
  labs(title = "A Week of forecast density on FTSE 100 Index from 24th June, 2024 to 28th June, 2024", x = "Quantile", y = "Density") +
  theme_minimal()

################################# VaR and ES ###################################

# Calculate the empirical Value at Risk 
VaR(kde_2020_10$Quantile, p = 0.95, method = "historical")

# Calculate TVaR at the 95th percentile (Use the fit of GPD)
tvar(gpd_fit_2020_10_right, 0.95)

# Try 99%

# Calculate Expected Shortfall (ES)
ES(Forecast_5, p = 0.95, method = "historical")

# Add this
