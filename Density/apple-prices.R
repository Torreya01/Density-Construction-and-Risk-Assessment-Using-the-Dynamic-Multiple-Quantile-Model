# Import libraries
library(DMQ)
library(np)
library(ks)
library(ggplot2)
library(moments)
library(dplyr)
library(lubridate)
library(scales)
library(PerformanceAnalytics)
library(ismev)
library(evir)
library(evd)
library(plotly)
library(dplyr)


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

# Define the color palette with named colors corresponding to month names
colors = c("black", "blue", "red", "green", "purple", "yellow", "orange", "pink", "cyan", "brown", "magenta", "turquoise")
names(colors) = month.name  # Ensure that colors are named by month names for easy referencing

# List of months to plot
months = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
years = c("2014", "2015", "2016", "2017", "2018", 
          "2019", "2020", "2021", "2022", "2023", "2024")

# Loop through each month and create the plots
for (year in years) {
  # Subset the data for the current year
  kde_year = kde_all %>% dplyr::filter(Year == year)
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
kde_all$Month <- factor(kde_all$Month, levels = 1:12, labels = month.name)
kde_all$Month_num <- as.numeric(kde_all$Month) 

# Add the camera eye
camera_eye <- list(x = -1, y = 2, z = 0.2) 

for (year in years) {
  # Subset the data for the current year
  kde_year <- filter(kde_all, Year == year)
    
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
  
  # Save the plot using orca
  plotly::orca(p, "plot.pdf")
}

# Set color palette
colors <- RColorBrewer::brewer.pal(12, "Set3")

# Set camera eye
camera_eye <- list(x = -1.5, y = 1.5, z = 0.3) 

# Assuming 'years' is a vector of years to plot
years <- unique(kde_all$Year)

# Create a 3D surface plot
for (year in years) {
  # Subset the data for the current year
  kde_year <- filter(kde_all, Year == year)
  
  # Aggregate data to ensure unique quantile values within each month
  kde_year <- kde_year %>%
    group_by(Month_num, Quantile) %>%
    summarize(Density = mean(Density), .groups = 'drop')
  
  # Add jitter to avoid collinearity issues
  kde_year$Quantile <- kde_year$Quantile + rnorm(nrow(kde_year), sd = 1e-6)
  kde_year$Month_num <- kde_year$Month_num + rnorm(nrow(kde_year), sd = 1e-6)
  
  # Interpolate to fill the grid
  interp_data <- akima::interp(x = kde_year$Quantile, y = kde_year$Month_num, z = kde_year$Density,
                               xo = quantiles, yo = months, duplicate = "mean")
  
  # Plot the surface
  p <- plot_ly(
    x = interp_data$x, 
    y = month.name[interp_data$y], 
    z = interp_data$z, 
    type = "surface", 
  )
  
  # Layout with camera and legend configuration
  p <- p %>% layout(
    title = paste("Quantile Density Plot in Year", year, "for Apple Inc log-returns"),
    scene = list(
      xaxis = list(title = "Quantile"),
      yaxis = list(title = "Month"),
      zaxis = list(title = "Density"), 
      camera = list(eye = list(x = -1.5, y = 1.5, z = 0.3))
    ),
    legend = list(title = list(text = 'Months'), orientation = "v")
  )
  
  print(p)
  
  # Use orca to save as SVG if available
  plotly::export(p, file = "plot.svg")
  ggsave("Density/apple_2022.pdf", plot = g)
}



plot(kde_2020_10$Quantile, kde_2020_10$Density, type = "l")

# Turn the forecast results into a data frame
Forecast_week = data.frame(quantiles = c(Forecast_apple_5))

# Calculate the 95th percentile threshold
threshold_forecast = quantile(Forecast_week$quantiles, 0.95)

# Select the top 5% of data values
upper_tail_data_forecast = Forecast_week %>% dplyr::filter(Forecast_week$quantile > threshold_11)

# Compute optimal bandwidth using cross-validation on Maximum Likelihood
bw_ls = npudensbw(upper_tail_data_forecast$quantile, ckertype = "gaussian", bwmethod = "cv.ls")
  
# Perform kernel density estimation using the optimal bandwidth
kde_ls = npudens(upper_tail_data_forecast$quantile, bws = bw_ls, ckertype = "gaussian")
  
# Convert KDE result to data frame for plotting
kde_ls_frame = data.frame(Quantile = kde_ls$eval$upper_tail_data_forecast, Density = kde_ls$dens)
  
# Add lines to the original density plot
plot(kde_ls_frame$Quantile, kde_ls_frame$Density, type = "l")

################################ Fit GARCH(1, 1) ###############################

# Fit the data into GARCH(1,1)
spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "norm")

fit = ugarchfit(spec = spec, data = kde_2020_10$Quantile)

# Calculate the standardised GARCH residuals
std_resid = fit@fit$z

# Plot the time series
std.ts = ts(std_resid)
plot(std.ts, xlab = "Time", ylab = "Value")

# Write function about the indicator function
indicator = function(x, threshold) {
  as.numeric(x > threshold)
}

# Write function about the mean excess function
mean_excess = function(X, v){
  numerator = sum((X - v) * indicator(X, v))
  denominator = sum(indicator(X, v))
  value = numerator / denominator
  value = ifelse(is.nan(value), 0, value)
  value
}

# Initialise mean excess values
en_i = rep(0, length(std_resid))

# Use for loop 
for (i in 1:length(std_resid)){
  value = mean_excess(std_resid, std_resid[i])
  en_i[i] = value
}

# Plot the points
plot(std_resid, en_i,
     ylab = "sample mean excess", xlab = "standardised residuals")

############################# Extreme Value Theory #############################

# Calculate the tail value at risk
tvar = function(fit, p) {
  scale = fit$lse[1]
  shape = fit$lse[2]
  threshold = fit$threshold
  return(threshold + (scale / shape) * ((1 - p)^(-shape) - 1))
}

# Set the threshold and extract the excess values from the right tail
right_2020_10 = quantile(kde_2020_10$Quantile, 0.95)
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

# Sort the apple_diff to be ascending
apple_new_sorted = sort(apple_diff)

# Use the same kernel and bandwidth to draw the density of the full data
bw_ls_full = npudensbw(apple_new_sorted, ckertype = "gaussian", bwmethod = "cv.ls")

# Perform kernel density estimation using the optimal bandwidth
kde_ls_full = npudens(apple_new_sorted, bws = bw_ls_full, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_ls_frame_full = data.frame(Quantile = kde_ls_full$eval$apple_new_sorted, Density = kde_ls_full$dens)

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
qqnorm(apple_diff, main = "QQ Plot for the original log-returns for Apple Inc.")
qqline(apple_diff, col = "red")
datawizard::kurtosis(apple_diff)

########################### Monthly Kurtosis plot #################################

# Calculate the krutosis for each month in 2014 to 2024
kurtosis_list = kde_all %>%
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
  labs(title = "Monthly Kurtosis over Time for Apple Inc",
       x = "Date (Year)",
       y = "Kurtosis") +
  theme_minimal() + 
  scale_y_continuous(limits = c(3,7)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year")

# Print the kurtosis results
print(kurtosis_list)

################################ Skewness plot #################################

# Calculate the krutosis for each month in 2014 to 2024
skewness_list = kde_all %>%
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
  labs(title = "Monthly Skewness over Time for Apple Inc.",
       x = "Date (Year)",
       y = "skewness") +
  theme_minimal() + 
  scale_y_continuous(limits = c(-1, 1.5)) +
  scale_x_date(labels = date_format("%Y"), breaks = "1 year")


############################ Daily Kurtosis plot ###############################

# Define the months and years for the analysis
months_years <- list(c(2020, 3), c(2017, 6), c(2017, 2), c(2022, 7), c(2022, 4), c(2022, 3))
# Initialize a list to store the plots
plots <- list()

# Loop through each month and year
for (my in months_years) {
  year <- my[1]
  month <- my[2]
  
  # Filter indices for the specified month and year
  indices_name <- paste("indices", year, month, sep = "_")
  indices <- which(format(as.Date(apple$Date), "%Y-%m") == sprintf("%04d-%02d", year, month))
  assign(indices_name, indices)
  
  # Select quantiles data based on indices
  quantiles_name <- paste("quantiles", year, month, sep = "_")
  quantiles_selected <- quantiles[,indices]
  assign(quantiles_name, quantiles_selected)
  
  # Calculate kurtosis for each column
  kurtosis_values <- apply(quantiles_selected, 2, kurtosis)

  # Select dates based on indices
  days_in_month <- as.Date(apple$Date[indices])
  
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
    labs(title = paste("Daily Kurtosis over Time for", kurtosis_df$Month, "in", year),
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
Forecast_5 = sort(as.vector(Forecast_apple_5))

# Plot the forecasts into a density plot
bw_ls = npudensbw(Forecast_5, ckertype = "gaussian", bwmethod = "cv.ls")

# Perform kernel density estimation using the optimal bandwidth
kde_ls = npudens(Forecast_5, bws = bw_ls, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_ls_frame = data.frame(Quantile = kde_ls$eval$Forecast_5, Density = kde_ls$dens)

# Create density plot using ggplot2
ggplot(kde_ls_frame, aes(x = Quantile, y = Density)) +
  geom_line() +
  labs(title = "A Week of forecast density on Apple Inc. from 24th June, 2024 to 28th June, 2024", x = "Quantile", y = "Density") +
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


