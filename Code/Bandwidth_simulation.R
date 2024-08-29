# Import libraries
library(ggplot2)
library(gridExtra)
library(MASS)

# Function to generate data from a normal mixture distribution
generate_mixture = function(n) {
  # Mixture of two normal distributions
  mixture = c(rnorm(n * 0.75, mean = 0, sd = 1), rnorm(n * 0.25, mean = 2, sd = 0.5))
  return(mixture)
}

# Set seed for reproducibility
set.seed(123)

# Generate the data
n = 1000
data = generate_mixture(n)

# Define bandwidths
bandwidths = c(0.05, 0.68, 0.19)

# True density function for the mixture distribution
true_density = function(x) {
  0.75 * dnorm(x, mean = 0, sd = 1) + 0.25 * dnorm(x, mean = 2, sd = 0.5)
}

# Plotting the densities with different bandwidths
plot_density_with_bandwidth = function(data, bandwidth, true_density) {
  ggplot(data = data.frame(x = data), aes(x = x)) +
    geom_density(aes(y = ..density..), bw = bandwidth, color = "black", linewidth = 1) +
    stat_function(fun = true_density, color = "red", linetype = "dashed", linewidth = 1) +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 0.4), xlim = c(-3, 3)) +
    xlab("") + ylab("")
}

# Plot each bandwidth
p1 = plot_density_with_bandwidth(data, bandwidths[1], true_density)
p2 = plot_density_with_bandwidth(data, bandwidths[2], true_density)
p3 = plot_density_with_bandwidth(data, bandwidths[3], true_density)

# Show the plots
p1
p2
p3
