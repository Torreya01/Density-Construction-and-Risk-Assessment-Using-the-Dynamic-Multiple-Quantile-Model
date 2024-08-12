# Import libraries
library(DMQ)
library(ggplot2)
library(dplyr)

# Source the functions in source file
source("Density/source.R")

#################### Time series for Apple and new Apple data ##################

# Import data of apple share prices
apple = read.csv("Data/Raw/apple_share_prices.csv")

# Turn apple$Date into Date object
apple$Date = as.Date(apple$Date)

# Convert the cleaned Price column to numeric
apple$Close = as.numeric(apple$Close)

# Save the cleaned data
write.csv(apple, "Data/Derived/apple_cleaned.csv")

# Set up the date limits
date_limits = range(apple$Date, na.rm = TRUE)

# Plot the data using ggplot
ggplot(data = apple, aes(x = Date, y = Close, group = 1)) + 
  geom_line() +
  labs(title = "apple Share Price - Daily Closing Prices (2014-2024)",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = date_limits)

# Load the new data set
apple_new = read.csv("Data/Raw/apple_share_prices_new.csv")

# Turn apple$Date into Date object
apple_new$Date = as.Date(apple_new$Date)

# Convert the cleaned Price column to numeric
apple_new$Close = as.numeric(apple_new$Close)

# Save the cleaned data
write.csv(apple_new, "Data/Derived/apple_new_cleaned.csv")

###################### Time series for FTSE and new FTSE data ##################

# Import data of FTSE share prices
FTSE = read.csv("Data/Raw/FTSE_100.csv")

# Apply the function to the Date column
FTSE$Date = as.Date(FTSE$Date, format = "%m/%d/%y")

# Sort the dates into the correct order
FTSE$Date = sort(FTSE$Date)

# Convert the cleaned Price column to numeric
FTSE$Close = as.numeric(FTSE$Close)

# Save the cleaned data
write.csv(FTSE, "Data/Derived/FTSE_cleaned.csv")

# Set up the date limits
date_limits = range(FTSE$Date, na.rm = TRUE)

# Plot the data using ggplot
ggplot(data = FTSE, aes(x = Date, y = Close, group = 1)) + 
  geom_line() +
  labs(title = "FTSE Share Price - Daily Closing Prices (2014-2024)",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = date_limits)

# Load the new data set
FTSE_new = read.csv("Data/Raw/FTSE_100_new.csv")

# Apply the function to the Date column
FTSE_new$Date = as.Date(FTSE_new$Date, format = "%m/%d/%y")

# Sort the dates into the correct order
FTSE_new$Date = sort(FTSE_new$Date)

# Convert the cleaned Price column to numeric
FTSE_new$Close = as.numeric(FTSE_new$Close)

# Save the cleaned data
write.csv(FTSE_new, "Data/Derived/FTSE_new_cleaned.csv")

######################### Log-returns for Apple and Apple new ##################

# Make a new data column that contains the differences between each log entry
apple$log = log(apple$Close)
apple_diff = apple[-1, ]
apple_diff = diff(apple$log)

# Make a new data column that contains the differences between each log entry
apple_new$log = log(apple_new$Close)
apple_diff_new = apple_new[-1, ]
apple_diff_new = diff(apple_new$log)

# Plot the time series of the log returns 
return.ts = ts(apple_diff, frequency = 260, start = c(2014,7,14))
plot(return.ts, xlab = "Time", ylab = "Index", 
     main = "log-returns of Apple Inc's (2014-2024)")

######################### Log-returns for FTSE and FTSE new ####################

# Make a new data column that contains the differences between each log entry
FTSE$log = log(FTSE$Close)
FTSE_diff = FTSE[-1, ]
FTSE_diff = diff(FTSE$log)

# Make a new data column that contains the differences between each log entry
FTSE_new$log = log(FTSE_new$Close)
FTSE_diff_new = FTSE_new[-1, ]
FTSE_diff_new = diff(FTSE_new$log)

# Plot the time series of the log returns 
return.ts = ts(FTSE_diff, frequency = 260, start = c(2014,7,14))
plot(return.ts, xlab = "Time", ylab = "Index", 
     main = "log-returns of FTSE 100 (2014-2024)")

############################ Summary Statistics ################################

summary_stats(apple_diff)
summary_stats(FTSE_diff)

############################# Fit Apple DMQ Model ##############################

# Set the tau values
vTau = seq(0.01, 0.99, length.out = 99)

# Fit the Apple data into model
Fit_apple = EstimateDMQ(vY = apple_diff, vTau = vTau, 
                        iTau_star = 50,  # Median as reference
                        FixReference = TRUE,
                        fn.optimizer = fn.solnp)

# Save the Apple fit to RDS file
saveRDS(Fit_apple, file = "Data/Fitted_RDS/Fit_apple.rds")

# Fit the data into model
Fit_apple_new = EstimateDMQ(vY = apple_diff_new, vTau = vTau, 
                            iTau_star = 50,  # Median as reference
                            FixReference = TRUE,
                            fn.optimizer = fn.solnp)

# Save the fit to RDS file
saveRDS(Fit_apple_new, file = "Data/Fitted_RDS/Fit_apple_new.rds")

############################## Fit FTSE DMQ Model ##############################

# Fit the FTSE data into model
Fit_FTSE = EstimateDMQ(vY = FTSE_diff, vTau = vTau, 
                       iTau_star = 50,  # Median as reference
                       FixReference = TRUE,
                       fn.optimizer = fn.solnp)

# Save the FTSE fit to RDS file
saveRDS(Fit_FTSE, file = "Data/Fitted_RDS/Fit_FTSE.rds")

# Fit the data into model
Fit_FTSE_new = EstimateDMQ(vY = FTSE_diff_new, vTau = vTau, 
                           iTau_star = 50,  # Median as reference
                           FixReference = TRUE,
                           fn.optimizer = fn.solnp)

# Save the fit to RDS file
saveRDS(Fit_FTSE_new, file = "Data/Fitted_RDS/Fit_FTSE_new.rds")
