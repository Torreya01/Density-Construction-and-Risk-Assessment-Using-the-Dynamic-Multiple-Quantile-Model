# Import libraries
library(DMQ)
library(dplyr)

# Load the year and month

########################### Monthly Kurtosis plot #################################

# Apple Inc
# Load the pre-calculated density estimation
kde_result_apple = readRDS(file = "Data/Fitted_RDS/kde_result_apple.rds")

# Combine all KDE results into a single data frame
kde_apple_all = do.call(rbind, kde_result_apple)

# Prepare the data by ensuring Month and Month_num are correctly formatted
kde_apple_all$Month = factor(kde_apple_all$Month, levels = 1:12, labels = month.name)
kde_apple_all$Month_num = as.numeric(kde_apple_all$Month) 

# Calculate the krutosis for each month in 2014 to 2024
kurtosis_list_apple = kde_apple_all %>%
  group_by(Year, Month_num) %>%
  summarise(Kurtosis = kurtosis(Quantile, na.rm = TRUE) + 3) %>%
  ungroup() 

# Create a new column 'Date' for plotting
kurtosis_list_apple$Date = with(kurtosis_list_apple, as.Date(paste(Year, Month_num, "1", sep = "-")))
kurtosis_list_apple = kurtosis_list_apple[order(kurtosis_list_apple$Date), ]

# Plotting the kurtosis
ggplot(kurtosis_list_apple, aes(x = Date, y = Kurtosis)) +
  geom_line() +  # Connect points with lines
  geom_point() +  # Show points
  labs(title = "Monthly Kurtosis over Time for Apple Inc",
       x = "Date (Year)",
       y = "Kurtosis") +
  theme_minimal() + 
  scale_y_continuous(limits = c(3,7)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year")

# FTSE
# Load the pre-calculated density estimation
kde_result_FTSE = readRDS(file = "Data/Fitted_RDS/kde_result_FTSE.rds")

# Combine all KDE results into a single data frame
kde_FTSE_all = do.call(rbind, kde_result_FTSE)

# Prepare the data by ensuring Month and Month_num are correctly formatted
kde_FTSE_all$Month = factor(kde_FTSE_all$Month, levels = 1:12, labels = month.name)
kde_FTSE_all$Month_num = as.numeric(kde_FTSE_all$Month) 

# Calculate the krutosis for each month in 2014 to 2024
kurtosis_list_FTSE = kde_FTSE_all %>%
  group_by(Year, Month_num) %>%
  summarise(Kurtosis = kurtosis(Quantile, na.rm = TRUE) + 3) %>%
  ungroup() 

# Create a new column 'Date' for plotting
kurtosis_list_FTSE$Date = with(kurtosis_list_FTSE, as.Date(paste(Year, Month_num, "1", sep = "-")))
kurtosis_list_FTSE = kurtosis_list_FTSE[order(kurtosis_list_FTSE$Date), ]

# Plotting the kurtosis
ggplot(kurtosis_list_FTSE, aes(x = Date, y = Kurtosis)) +
  geom_line() +  # Connect points with lines
  geom_point() +  # Show points
  labs(title = "Monthly Kurtosis over Time for FTSE 100",
       x = "Date (Year)",
       y = "Kurtosis") +
  theme_minimal() + 
  scale_y_continuous(limits = c(3,7)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year")

################################ Skewness plot #################################

# Apple
# Calculate the krutosis for each month in 2014 to 2024
skewness_list_apple = kde_apple_all %>%
  group_by(Year, Month_num) %>%
  summarise(skewness = skewness(Quantile, na.rm = TRUE)) %>%
  ungroup() 

# Create a new column 'Date' for plotting
skewness_list_apple$Date = with(skewness_list_apple, as.Date(paste(Year, Month_num, "1", sep = "-")))
skewness_list_apple = skewness_list_apple[order(skewness_list_apple$Date), ]

# Plotting the kurtosis
ggplot(skewness_list_apple, aes(x = Date, y = skewness)) +
  geom_line() +  # Connect points with lines
  geom_point() +  # Show points
  labs(title = "Monthly Skewness over Time for apple Inc.",
       x = "Date (Year)",
       y = "skewness") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-1, 1.5)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year")

# FTSE
# Calculate the krutosis for each month in 2014 to 2024
skewness_list_FTSE = kde_FTSE_all %>%
  group_by(Year, Month_num) %>%
  summarise(skewness = skewness(Quantile, na.rm = TRUE)) %>%
  ungroup() 

# Create a new column 'Date' for plotting
skewness_list_FTSE$Date = with(skewness_list_FTSE, as.Date(paste(Year, Month_num, "1", sep = "-")))
skewness_list_FTSE = skewness_list_FTSE[order(skewness_list_FTSE$Date), ]

# Plotting the kurtosis
ggplot(skewness_list_FTSE, aes(x = Date, y = skewness)) +
  geom_line() +  # Connect points with lines
  geom_point() +  # Show points
  labs(title = "Monthly Skewness over Time for apple Inc.",
       x = "Date (Year)",
       y = "skewness") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-1, 1.5)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year")

############################ Daily Kurtosis plot ###############################

# Load the previous data
FTSE = read.csv("Data/Derived/FTSE_cleaned.csv")
Fit_FTSE = readRDS(file = "Data/Fitted_RDS/Fit_FTSE.rds")
quantiles_FTSE = Fit_FTSE$lFilter$mQ
quantiles_FTSE = quantiles_FTSE[,-1]
FTSE = FTSE[-1, ]

# Define the months and years for the analysis
months_years = list(c(2018, 10), c(2018, 11))
# Initialize a list to store the plots
plots = list()

# Loop through each month and year
for (my in months_years) {
  year = my[1]
  month = my[2]
  
  # Filter indices for the specified month and year
  indices_name = paste("indices", year, month, sep = "_")
  indices = which(format(as.Date(FTSE$Date), "%Y-%m") == sprintf("%04d-%02d", year, month))
  assign(indices_name, indices)
  
  # Select quantiles data based on indices
  quantiles_name = paste("quantiles", year, month, sep = "_")
  quantiles_selected = quantiles_FTSE[,indices]
  assign(quantiles_name, quantiles_selected)
  
  # Calculate kurtosis for each column
  kurtosis_values = apply(quantiles_selected, 2, kurtosis)
  
  # Select dates based on indices
  days_in_month = as.Date(FTSE$Date[indices])
  
  # Create a data frame for plotting
  kurtosis_df = data.frame(
    Day = as.Date(format(days_in_month, "%y/%m/%d")),
    Kurtosis = kurtosis_values + 3, 
    Month = factor(month, levels = 1:12, labels = month.name)
  )
  
  # Create the plot
  p = ggplot(kurtosis_df, aes(x = Day, y = Kurtosis)) +
    geom_line() +  # Connect points with lines
    geom_point() +  # Show points
    labs(title = paste("Daily Kurtosis over Time for", kurtosis_df$Month, "in", year),
         x = "Day",
         y = "Kurtosis") +
    theme_minimal() + 
    scale_x_date(labels = date_format("%d"), breaks = "5 days")
  
  # Store the plot in the list
  plots[[paste(year, month, sep = "_")]] = p
}

# Display the plots
for (plot_name in names(plots)) {
  print(plots[[plot_name]])
}
