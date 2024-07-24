# Load necessary libraries
library(DMQ)
library(dplyr)
library(ggplot2)
library(np)

#########################################################################################################
# Construct density estimates based on the formula
calculate_density = function(tau_levels, quantile_estimates, epsilon = 1e-8) {
  
  density_estimates = numeric(length(tau_levels) - 1)
  
  for (i in 2:length(tau_levels)) {
    numerator = tau_levels[i] - tau_levels[i - 1]
    denominator = quantile_estimates[i] - quantile_estimates[i - 1]
    
    if (denominator == 0) {
      density_estimates[i - 1] = numerator / denominator + epsilon
    } else {
      density_estimates[i - 1] = numerator / denominator
    }
  }
  
  # Interpolate NAs if any
  na_indices = which(is.na(density_estimates))
  if (length(na_indices) > 0) {
    valid_indices = which(!is.na(density_estimates))
    density_estimates = approx(valid_indices, density_estimates[valid_indices], xout = 1:(length(tau_levels)-1))$y
  }
  
  return(density_estimates)
  
}

#########################################################################################################

# Import data
data_unemployment = read.csv("Data/US_unemployment.csv") 

# Convert Date column to Date type
data_unemployment$DATE = as.Date(data_unemployment$DATE)

filtered_data = filter(data_unemployment, format(DATE, "%Y") == 2008)
vTau = seq(0.01, 0.99, length.out = 99)
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
# Import data of GDP growth
data_GDP = read.csv("Data/US_GDP.csv")

# Convert Date column to Date type
data_GDP$DATE = as.Date(data_GDP$DATE)

filtered_GDP = filter(data_GDP, format(DATE, "%Y") == 2008)
Fit_GDP = EstimateDMQ(vY = filtered_GDP$GDP, vTau = vTau, 
                  iTau_star = 50,  # Median as reference
                  FixReference = TRUE,
                  fn.optimizer = fn.solnp)

# Extract the generated quantiles
generated_quantiles_GDP = Fit_GDP$lFilter$mQ


for (quarter in 2:5) {
  quantiles_quarter = generated_quantiles_GDP[, quarter]  # Adjust indexing if needed
  density_quarter = calculate_density(vTau, quantiles_quarter)
  
  # Create data frame for plotting
  density_df_GDP <- data.frame(
    x = quantiles_quarter[-1],  # Remove the first quantile because we have n-1 densities
    y = density_quarter
  )
  
  # Plot the density
  p = ggplot(density_df_GDP, aes(x = x, y = y)) +
    geom_line() +
    geom_area(fill = "blue", alpha = 0.5) +
    labs(x = "Values", y = "Density") +
    ggtitle(paste("Density plot for quarter", quarter, "of 2008"))
  
  print(p)
  
}

#########################################################################################################
# Treat the data as a whole year
generated_quantiles_GDP = Fit_GDP$lFilter$mQ

df = data.frame(
  Quantile = as.vector(generated_quantiles_GDP),
  Year = rep(2008, length(as.vector(generated_quantiles_GDP))))

density_GDP = calculate_density(vTau, df$Quantile)

# Create data frame for plotting
density_df_GDP = data.frame(
  x = quantiles_quarter[-1],  # Remove the first quantile because we have n-1 densities
  y = density_quarter
)
