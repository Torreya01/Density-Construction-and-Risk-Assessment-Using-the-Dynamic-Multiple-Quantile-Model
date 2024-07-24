# Load necessary libraries
library(DMQ)
library(dplyr)
library(ggplot2)
library(np)

################################## Define Smoothing Function Using Histogram ###############################

# Define a function to draw the density based on different year
density_plot_histogram = function(data, year, Tau, iTau_star){
  # Select year data from the full data set
  filtered_data = filter(data, format(DATE, "%Y") == year)
  
  # Set Tau values
  vTau = seq(0.01, 0.99, length.out = Tau)
  
  # Fit the DMQ model in the data
  Fit = EstimateDMQ(vY = filtered_data[,2], vTau = vTau, 
                    iTau_star = iTau_star,  # Median as reference
                    FixReference = TRUE,
                    fn.optimizer = fn.solnp)
  
  # Extract the generated quantiles
  generated_quantiles = Fit$lFilter$mQ
  
  # Transform the quantiles into data frame for plotting
  df = data.frame(
    Quantile = as.vector(generated_quantiles),
    Year = rep(year, length(as.vector(generated_quantiles))))
  
  # Compute density
  density_data = density(df$Quantile)
  
  # Convert density to a data frame
  density_df = data.frame(x = density_data$x, y = density_data$y)
  
  # Add points at the start and end to bring the density to zero
  density_df = rbind(data.frame(x = min(density_df$x), y = 0), density_df, data.frame(x = max(density_df$x), y = 0))
  
  # Plot the density
  ggplot(density_df, aes(x = x, y = y)) +
    geom_line() +
    geom_area(fill = "blue", alpha = 0.5) +
    labs(x = "Values", y = "Density") +
    ggtitle(paste("Density plot in Year", year))
}

################################## Define Smoothing Function Via Epanechnikov ###############################

# Define a function to draw the density based on different years with Epanechnikov kernel
density_plot_epanechnikov <- function(data, year, Tau, iTau_star) {
  # Select year data from the full dataset
  filtered_data <- filter(data, format(DATE, "%Y") == year)
  
  # Set Tau values
  vTau = seq(0.01, 0.99, length.out = Tau)
  
  # Fit the DMQ model in the data
  Fit = EstimateDMQ(vY = filtered_data[,2], vTau = vTau, 
                    iTau_star = iTau_star,  # Median as reference
                    FixReference = TRUE,
                    fn.optimizer = fn.solnp)
  
  # Extract the generated quantiles
  generated_quantiles = Fit$lFilter$mQ
  
  # Transform the quantiles into data frame for plotting
  df = data.frame(
    Quantile = as.vector(generated_quantiles),
    Year = rep(year, length(as.vector(generated_quantiles))))
  
  # Compute density using the Epanechnikov kernel
  density_data <- density(df$Quantile, kernel = "epanechnikov")
  
  # Convert density to a data frame
  density_df <- data.frame(x = density_data$x, y = density_data$y)
  
  # Add points at the start and end to bring the density to zero
  density_df <- rbind(data.frame(x = min(density_df$x), y = 0), density_df, data.frame(x = max(density_df$x), y = 0))
  
  # Plot the density
  ggplot(density_df, aes(x = x, y = y)) +
    geom_line() +
    geom_area(fill = "blue", alpha = 0.5) +
    labs(x = "Values", y = "Density") +
    ggtitle(paste("Density plot in Year", year, "using Epanechnikov kernel"))
}

########################################## US Unemployment ################################################ 

# Import data
data_unemployment = read.csv("Data/US_unemployment.csv") 

# Convert Date column to Date type
data_unemployment$DATE = as.Date(data_unemployment$DATE)

# Generate density plots for some significant year
density_plot_histogram(data_unemployment, 1995, 99, 50)
density_plot_histogram(data_unemployment, 2003, 99, 50)
density_plot_histogram(data_unemployment, 2008, 99, 50)
density_plot_histogram(data_unemployment, 2017, 99, 50)
density_plot_histogram(data_unemployment, 2020, 99, 50)

################################################# US GDP ###################################################

# Import data
data_GDP = read.csv("Data/US_GDP.csv") 

# Convert Date column to Date type
data_GDP$DATE = as.Date(data_GDP$DATE)

# Generate density plots for some significant year
density_plot_histogram(data_GDP, 1995, 99, 50)
density_plot_histogram(data_GDP, 2003, 99, 50)
density_plot_histogram(data_GDP, 2008, 99, 50)
density_plot_histogram(data_GDP, 2017, 99, 50)
density_plot_histogram(data_GDP, 2020, 99, 50)

density_plot_epanechnikov(data_GDP, 2020, 99, 50)

#########################################################################################################

# Convert Date column to Date type
data_unemployment$DATE = as.Date(data_unemployment$DATE)

filtered_data = filter(data_unemployment, format(DATE, "%Y") == 2008)
vTau = seq(0.01, 0.99, 0.01)
Fit = EstimateDMQ(vY = filtered_data$UNRATE, vTau = vTau, 
                  iTau_star = 50,  # Median as reference
                  FixReference = TRUE,
                  fn.optimizer = fn.solnp)

# Extract the generated quantiles
generated_quantiles = Fit$lFilter$mQ

# Iterate over each time point (month) to compute and plot densities
for (month in 1:13) {
  quantiles_month = generated_quantiles[, month]  # Adjust indexing if needed
  density_month = calculate_density(vTau, quantiles_month)
  
  # Create data frame for plotting
  density_df = data.frame(
    x = quantiles_month[-1],  # Remove the first quantile because we have n-1 densities
    y = density_month
  )
  
  # Plot the density
  p = ggplot(density_df, aes(x = x, y = y)) +
    geom_line() +
    geom_area(fill = "blue", alpha = 0.5) +
    labs(x = "Values", y = "Density") +
    ggtitle(paste("Density plot for month", month, "of 2008"))
  
  print(p)
}

##########################################################################################################

filtered_GDP = filter(data_GDP, format(DATE, "%Y") == 2008)
Fit_GDP = EstimateDMQ(vY = filtered_GDP$GDP, vTau = vTau, 
                      iTau_star = 50,  # Median as reference
                      FixReference = TRUE,
                      fn.optimizer = fn.solnp)

# Extract the generated quantiles
generated_quantiles_GDP = Fit_GDP$lFilter$mQ
generated_quantiles_GDP = mQ

# Iterate over each time point (month) to compute and plot densities
for (quarter in 1:4) {
  quantiles_quarter = generated_quantiles_GDP[, quarter]  # Adjust indexing if needed
  
  # Transform the quantiles into data frame for plotting
  df_GDP = data.frame(
    Quantile = as.vector(quantiles_quarter),
    Year = rep(quarter, length(as.vector(quantiles_quarter))))
  
  # Compute density
  density_data = density(df_GDP$Quantile)
  
  # Convert density to a data frame
  density_df_GDP = data.frame(x = density_data$x, y = density_data$y)
  
  # Add points at the start and end to bring the density to zero
  density_df_GDP = rbind(data.frame(x = min(density_df_GDP$x), y = 0), density_df_GDP, data.frame(x = max(density_df_GDP$x), y = 0))
  
  # Plot the density
  ggplot(density_df_GDP, aes(x = x, y = y)) +
    geom_line() +
    geom_area(fill = "blue", alpha = 0.5) +
    labs(x = "Values", y = "Density") +
    ggtitle(paste("Density plot for quarter", quarter, "of 2008"))
}

