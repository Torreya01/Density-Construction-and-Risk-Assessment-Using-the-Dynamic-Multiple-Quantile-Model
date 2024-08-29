# Import libraries
library(ggplot2)
library(gridExtra)

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

# Define kernel functions
gaussian_kernel = function(u) {
  (1 / sqrt(2 * pi)) * exp(-0.5 * u^2)
}

epanechnikov_kernel = function(u) {
  0.75 * (1 - u^2) * (abs(u) <= 1)
}

biweight_kernel = function(u) {
  (15 / 16) * (1 - u^2)^2 * (abs(u) <= 1)
}

triweight_kernel = function(u) {
  (35 / 32) * (1 - u^2)^3 * (abs(u) <= 1)
}

# Function to calculate kernel density estimate manually
manual_kde = function(data, eval_points, kernel_function, bandwidth) {
  n = length(data)
  kde_values = sapply(eval_points, function(x) {
    sum(kernel_function((x - data) / bandwidth)) / (n * bandwidth)
  })
  return(kde_values)
}

# True density function for the mixture distribution
true_density = function(x) {
  0.75 * dnorm(x, mean = 0, sd = 1) + 0.25 * dnorm(x, mean = 2, sd = 0.5)
}

# Bandwidth to be used (you can adjust this as needed)
bandwidth = 0.3

# Points where we want to evaluate the KDE
eval_points = seq(-3, 3, length.out = 500)

# Calculate KDE for each kernel type
kde_gaussian = manual_kde(data, eval_points, gaussian_kernel, bandwidth)
kde_epanechnikov = manual_kde(data, eval_points, epanechnikov_kernel, bandwidth)
kde_biweight = manual_kde(data, eval_points, biweight_kernel, bandwidth)
kde_triweight = manual_kde(data, eval_points, triweight_kernel, bandwidth)

# Function to create the kernel mass at the bottom with correct colors
add_kernel_mass = function(kernel_type, bandwidth, color) {
  kernel_x = seq(-2, 2, length.out = 500)
  if (kernel_type == "gaussian") {
    kernel_y = dnorm(kernel_x, mean = 0, sd = bandwidth)
  } else if (kernel_type == "epanechnikov") {
    kernel_y = 0.75 * (1 - (kernel_x / bandwidth) ^ 2) * (abs(kernel_x) <= bandwidth)
  } else if (kernel_type == "biweight") {
    kernel_y = (15 / 16) * (1 - (kernel_x / bandwidth) ^ 2)^2 * (abs(kernel_x) <= bandwidth)
  } else if (kernel_type == "triweight") {
    kernel_y = (35 / 32) * (1 - (kernel_x / bandwidth) ^ 2)^3 * (abs(kernel_x) <= bandwidth)
  } 
  
  # Scale the kernel to make it visible at the bottom of the plot
  kernel_y_scaled = kernel_y * 0.1 # Adjust the scaling for visibility
  geom_line(data = data.frame(x = kernel_x, y = kernel_y_scaled), aes(x = x, y = y), color = color, size = 1)
}

# Function to plot the results with the kernel mass at the bottom
plot_manual_kde_with_mass = function(eval_points, kde1, kde2, kernel_type1, kernel_type2, bandwidth) {
  ggplot() +
    geom_line(aes(x = eval_points, y = kde1), color = "black", size = 1) + # First kernel
    geom_line(aes(x = eval_points, y = kde2), color = "red", linetype = "dashed", size = 1) + # Second kernel
    add_kernel_mass(kernel_type1, bandwidth, "black") + # Kernel mass for the first kernel
    add_kernel_mass(kernel_type2, bandwidth, "red") + # Kernel mass for the second kernel
    theme_minimal() +
    coord_cartesian(ylim = c(0, 0.4), xlim = c(-3, 3)) +
    xlab("") + ylab("Density")
}

# Plotting the results for the first subfigure (Gaussian vs. Epanechnikov)
p1 = plot_manual_kde_with_mass(eval_points, kde_gaussian, kde_epanechnikov, "gaussian", "epanechnikov", bandwidth)

# Plotting the results for the second subfigure (Biweight vs. Triweight)
p2 = plot_manual_kde_with_mass(eval_points, kde_biweight, kde_triweight, "biweight", "triweight", bandwidth)

# Show the plots
p1
p2

