# Load library
library(stats)

# Define the Gaussian kernel function
gaussian_kernel = function(u) {
  (1 / sqrt(2 * pi)) * exp(-0.5 * u^2)
}

# Kernel Quantile Estimator function with weights
kernel_quantile_estimator_with_weights = function(x, p, h) {
  n = length(x)
  kq = numeric(length(p))
  weights_list = list()
  
  for (j in 1:length(p)) {
    weights = sapply((1:n)/n, function(t) gaussian_kernel((t - p[j]) / h))
    weights = weights / sum(weights)
    kq[j] = sum(weights * x)
    weights_list[[j]] = weights
  }
  
  return(list(quantile_estimates = kq, weights = weights_list))
}

quantile_density = function(p, x, bandwidth) {
  eps = .Machine$double.eps^0.5  # A small value for finite difference approximation
  q_plus = kernel_quantile_estimator_with_weights(x, p + eps, bandwidth)$quantile_estimates
  q_minus = kernel_quantile_estimator_with_weights(x, p - eps, bandwidth)$quantile_estimates
  return((q_plus - q_minus) / (2 * eps))  # Approximate derivative
}

vTau = seq(0.01, 0.99, 0.01)
bwd = 68.81351

# Estimate quantiles using the kernel quantile estimator with weights
quantile_result = kernel_quantile_estimator_with_weights(mQ, vTau, bwd)
quantile_estimates = quantile_result$quantile_estimates
weights_list = quantile_result$weights

# Estimate the quantile density at the given quantiles
quantile_density_estimates = sapply(vTau, quantile_density, x = data, bandwidth = bwd)

# Calculate the density of quantiles
density_of_quantiles = 1 / quantile_density_estimates

# Print the results
result = data.frame(Quantiles = quantiles, Density_of_Quantiles = density_of_quantiles)
print(result)

# Plot the results
ggplot(result, aes(x = Quantiles, y = Density_of_Quantiles)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Density of Quantiles using Direct Kernel Quantile Density Estimation",
       x = "Quantile",
       y = "Density") +
  theme_minimal()
