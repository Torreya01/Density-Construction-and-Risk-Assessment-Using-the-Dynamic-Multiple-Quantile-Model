# Create a function that can summarise all statistics from log-returns
summary_stats = function(log_returns) {
mean_lr = mean(log_returns, na.rm = TRUE)
std_dev = sd(log_returns, na.rm = TRUE)
skewness = moments::skewness(log_returns, na.rm = TRUE)
kurtosis = moments::kurtosis(log_returns, na.rm = TRUE)
min_lr = min(log_returns, na.rm = TRUE)
max_lr = max(log_returns, na.rm = TRUE)
median_lr = median(log_returns, na.rm = TRUE)
quantiles = quantile(log_returns, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)

# Create a list to hold the summary statistics
list(
  Mean = mean_lr,
  Standard_Deviation = std_dev,
  Skewness = skewness,
  Kurtosis = kurtosis,
  Minimum = min_lr,
  Maximum = max_lr,
  Median = median_lr,
  Quantiles = quantiles
)
}

# Function to sample from a mixed Gaussian distribution
sample_mixed_gaussian = function(n, means, vars, probs) {
  # Determine which mixture component each sample comes from
  components = sample(length(probs), size = n, replace = TRUE, prob = probs)
  rnorm(n, mean = means[components], sd = sqrt(vars[components]))
}


# Function to calculate the mse between densities for Gaussians in simulation study 
MSE_Gaussian = function(data, kernel, bwd, mean, var, size){
  MSE = rep(0, size)
  for (i in seq(1:size)){
    bw = npudensbw(data[, i], ckertype = kernel, bwmethod = bwd) 
    kde = npudens(data[, i], bws = bw, ckertype = kernel)
    kde_frame = data.frame(Quantile = kde$eval$data, Density = kde$dens)
    estimated_density = kde_frame$Density
    true_density = dnorm(data[, i], mean = mean1, sd = sqrt(var1))
    MSE[i] = mean((true_density - estimated_density)^2)
  }
  MSE = mean(MSE)
}

# Function to calculate the rescaled density for student-t distribution in simulation study
f_Y_vector = function(y, mean_t, sd_t, t) {
  # Compute the density values for each quantile
  t_density = dt((y - mean_t) / (sd_t / sqrt(2)), t)
  scale_factor = 1 / (sd_t / sqrt(2))
  return(t_density * scale_factor)
}

# Function to calculate the mse between densities for student-t in simulation study
MSE_t = function(data, kernel, bwd, freedom, size){
  MSE = rep(0, size)
  for (i in seq(1:size)){
    bw = npudensbw(data[, i], ckertype = kernel, bwmethod = bwd) 
    kde = npudens(data[, i], bws = bw, ckertype = kernel)
    kde_frame = data.frame(Quantile = kde$eval$data, Density = kde$dens)
    estimated_density = kde_frame$Density
    true_density = f_Y_vector(data[,i], mean_t, sd_t, t)
    MSE[i] = mean((true_density - estimated_density)^2)
  }
  MSE = mean(MSE)
}

# Function of calculating MSE in forecast density
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
