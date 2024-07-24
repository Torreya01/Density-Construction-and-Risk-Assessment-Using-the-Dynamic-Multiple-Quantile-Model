# Load necessary libraries
library(DMQ)
library(xts)
library(ggplot2)
library(ggridges)

# Import data of GDP growth
data_SP500 = read.csv("Data/SP500.csv")

# Convert Date column to Date type
data_SP500$DATE = as.Date(data_SP500$DATE)

# Convert SP500 column to numeric type
data_SP500$SP500 = as.numeric(data_SP500$SP500)

# Replace NA data
for (i in 1:nrow(data_SP500)){
  if (is.na(data_SP500[i, 2])) data_SP500[i, 2] = mean(data_SP500[(i - 1), 2], data_SP500[(i + 1), 2])
}

# Set the tau values
vTau = seq(0.01, 0.99, length.out = 99)

Fit = EstimateDMQ(vY = data_SP500$SP500, vTau = vTau, 
                  iTau_star = 50,  # Median as reference
                  FixReference = TRUE,
                  fn.optimizer = fn.solnp)

# Extract quantiles from the result
quantiles = Fit$lFilter$mQ # Transpose for appropriate structure

mQ = quantiles[, c(1000, 1001, 1002, 1003, 1305, 1306)]

kernel_density <- function(data) {
  par(mfrow = c(2, 3))  # Layout for 4 plots
  for (i in 1:ncol(data)) {
      
    # Calculate the density of these differences
    density_data = density(data)
    
    # Plot the density
    plot(density_data, main = paste("Density for Quarter", i), xlab = "Differences", ylab = "Density")
  }
}

# Call the function with the data
kernel_density(mQ)

calculate_density = function(tau_levels, quantile_estimates, epsilon = 1e-8) {
  
  density_estimates = numeric(length(tau_levels) - 1)
  
  for (i in 2:length(tau_levels)) {
    numerator = tau_levels[i] - tau_levels[i - 1]
    denominator = quantile_estimates[i] - quantile_estimates[i - 1]
    
    if (denominator == 0) {
      density_estimates[i - 1] = numerator / denominator
    } else {
      density_estimates[i - 1] = numerator / denominator
    }
  }
  return(density_estimates)
}


curve_density <- function(data) {
    quantiles_quarter = data # Adjust indexing if needed
    density_quarter = calculate_density(vTau, quantiles_quarter)
  
    # Create data frame for plotting
    density_df_GDP <- data.frame(
      x = quantiles_quarter[-1],  # Remove the first quantile because we have n-1 densities
      y = density_quarter
    )
    # Plot the density
    plot(density_df_GDP, type = "l")
}

curve_density(mQ)

###############################################################################################

# Plot for 1003
quantiles_20230404 = quantiles[, 1003] # Adjust indexing if needed
density_quarter = calculate_density(vTau, quantiles_20230404)

# Create data frame for plotting
density_df_GDP = data.frame(
  x = quantiles_20230404[-1],  # Remove the first quantile because we have n-1 densities
  y = density_quarter
)

# Plot the density
ggplot(density_df_GDP, aes(x = x, y = y)) + 
  geom_line() +
  geom_area(fill = "blue", alpha = 0.5) +
  labs(x = "Values", y = "Density") +
  ggtitle("Direct density plot for SP500 on 2023/4/4")

##############################################################################################
data_20230404 = data.frame(
  Quantile = as.vector(quantiles_20230404))

# Compute density using the Epanechnikov kernel
density_data <- density(data_20230404$Quantile)

# Convert density to a data frame
density_df <- data.frame(x = density_data$x, y = density_data$y)

# Add points at the start and end to bring the density to zero
density_df <- rbind(data.frame(x = min(density_df$x), y = 0), density_df, data.frame(x = max(density_df$x), y = 0))

# Plot the density
ggplot(density_df, aes(x = x, y = y)) + 
  geom_line() +
  geom_area(fill = "blue", alpha = 0.5) +
  labs(x = "Values", y = "Density") +
  ggtitle("Smoothed density plot for SP500 on 2023/4/4")


saveRDS(Fit, file = "Data/fit_DMQ_SP500.rds")
