# Load necessary library
library(MASS)
library(DMQ)

# Set the Gaussian samples
# Parameters for two normal Gaussians
mean1 = 3
var1 = 100
mean2 = 10
var2 = 5

# Parameters for mixed Gaussian (mixture of two normal distributions)
mean_mix1 = 3
var_mix1 = 100
mean_mix2 = 10
var_mix2 = 5
prob_mix = c(0.6, 0.4)  # Probabilities for each mixture component

# Function to sample from a mixed Gaussian distribution
sample_mixed_gaussian = function(n, means, vars, probs) {
  # Determine which mixture component each sample comes from
  components = sample(length(probs), size = n, replace = TRUE, prob = probs)
  rnorm(n, mean = means[components], sd = sqrt(vars[components]))
}

# Generate samples
set.seed(123)  # Set seed for reproducibility
samples_normal1 = rnorm(2000, mean1, sqrt(var1))
samples_normal2 = rnorm(2000, mean2, sqrt(var2))
samples_mixed = sample_mixed_gaussian(2000, c(mean_mix1, mean_mix2), c(var_mix1, var_mix2), prob_mix)

# Create a sequence of values to cover the relevant range
x_values_1 = seq(mean1 - 4*sqrt(var1), mean1 + 4*sqrt(var1), length.out = 300)
density_values_1 = dnorm(x_values_1, mean = mean1, sd = sqrt(var1))

x_values_2 = seq(mean2 - 4*sqrt(var2), mean2 + 4*sqrt(var2), length.out = 300)
density_values_2 = dnorm(x_values_2, mean = mean2, sd = sqrt(var2))

gmm_pdf = function(x) {
  density1 <- prob_mix[1] * (1 / (sqrt(2 * pi) * sqrt(var1))) * exp(-((x - mean1)^2) / (2 * sqrt(var1)^2))
  density2 <- prob_mix[2] * (1 / (sqrt(2 * pi) * sqrt(var2))) * exp(-((x - mean2)^2) / (2 * sqrt(var2)^2))
  # Return the sum of the densities
  return(density1 + density2)
}
x_values_mix = seq(-20, 40, length.out = 300)
density_values_mix <- sapply(x_values_mix, gmm_pdf)

###############################################################################################

# Generate quantiles using DMQ model
# Set the parameters
vTau = seq(0.01, 0.99, length.out = 99)

# Generate fit for these three samples
Fit_1 = EstimateDMQ(vY = samples_normal1, vTau = vTau, 
                  iTau_star = 50,  # Median as reference
                  FixReference = TRUE,
                  fn.optimizer = fn.solnp)

Fit_2 = EstimateDMQ(vY = samples_normal2, vTau = vTau, 
                    iTau_star = 50,  # Median as reference
                    FixReference = TRUE,
                    fn.optimizer = fn.solnp)

Fit_3 = EstimateDMQ(vY = samples_mixed, vTau = vTau, 
                    iTau_star = 50,  # Median as reference
                    FixReference = TRUE,
                    fn.optimizer = fn.solnp)

# Extract quantiles from the fit
quantiles_1 = Fit_1$lFilter$mQ
quantiles_2 = Fit_2$lFilter$mQ
quantiles_mix = Fit_3$lFilter$mQ

##############################################################################################

# Select one of the columns and see
mQ_1 = quantiles_1[,1000]
mQ_2 = quantiles_2[,1000]
mQ_mix = quantiles_mix[,1000]

# What should look like if I calcualte the straight density
density_1 = calculate_density(vTau, mQ_1)
density_1 = data.frame(Quantiles = mQ_1[-1], density = density_1)
density_2 = calculate_density(vTau, mQ_2)
density_2 = data.frame(Quantiles = mQ_2[-1], density = density_2)
density_mix = calculate_density(vTau, mQ_mix)
density_mix = data.frame(Quantiles = mQ_mix[-1], density = density_mix)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_1_ls = npudensbw(mQ_1, ckertype = "epanechnikov", bwmethod = "cv.ls") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_1_ls = npudens(mQ_1, bws = bw_1_ls, ckertype = "epanechnikov")

# Convert KDE result to data frame for plotting
kde_1_ls_frame = data.frame(Quantile = kde_1_ls$eval$mQ, Density = kde_1_ls$dens)

# Plot both kernel smoothing and the density
plot(density_1, type = "l", main = "Epanechnikov with ls")
lines(kde_1_ls_frame, col = "red", lwd = 2)
lines(x_values_1, density_values_1, col = 'blue', lwd = 2)


############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_1_ml = npudensbw(mQ_1, ckertype = "epanechnikov", bwmethod = "cv.ml") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_1_ml = npudens(mQ_1, bws = bw_1_ml, ckertype = "epanechnikov")

# Convert KDE result to data frame for plotting
kde_1_ml_frame = data.frame(Quantile = kde_1_ml$eval$mQ, Density = kde_1_ml$dens)

# Plot both kernel smoothing and the density
plot(density_1, type = "l", main = "Epanechnikov with ml")
lines(kde_1_ml_frame, col = 'red', lwd = 2)
lines(x_values_1, density_values_1, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_2_ls = npudensbw(mQ_2, ckertype = "epanechnikov", bwmethod = "cv.ls") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_2_ls = npudens(mQ_2, bws = bw_2_ls, ckertype = "epanechnikov")

# Convert KDE result to data frame for plotting
kde_2_ls_frame = data.frame(Quantile = kde_2_ls$eval$mQ, Density = kde_2_ls$dens)

# Plot both kernel smoothing and the density
plot(density_2, type = "l", main = "Epanechnikov with ls")
lines(kde_2_ls_frame, col = 'red', lwd = 2)
lines(x_values_2, density_values_2, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_2_ml = npudensbw(mQ_2, ckertype = "epanechnikov", bwmethod = "cv.ml") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_2_ml = npudens(mQ_2, bws = bw_2_ml, ckertype = "epanechnikov")

# Convert KDE result to data frame for plotting
kde_2_ml_frame = data.frame(Quantile = kde_2_ml$eval$mQ, Density = kde_2_ml$dens)

# Plot both kernel smoothing and the density
plot(density_2, type = "l", main = "Epanechnikov with ml")
lines(kde_2_ml_frame, col = 'red', lwd = 2)
lines(x_values_2, density_values_2, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_mix_ls = npudensbw(mQ_mix, ckertype = "epanechnikov", bwmethod = "cv.ls") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_mix_ls = npudens(mQ_mix, bws = bw_mix_ls, ckertype = "epanechnikov")

# Convert KDE result to data frame for plotting
kde_mix_ls_frame = data.frame(Quantile = kde_mix_ls$eval$mQ, Density = kde_mix_ls$dens)

# Plot both kernel smoothing and the density
plot(density_mix, type = "l", main = "Epanechnikov with ls")
lines(kde_mix_ls_frame, col = 'red', lwd = 2)
lines(x_values_mix, density_values_mix, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_mix_ml = npudensbw(mQ_mix, ckertype = "epanechnikov", bwmethod = "cv.ml") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_mix_ml = npudens(mQ_mix, bws = bw_mix_ml, ckertype = "epanechnikov")

# Convert KDE result to data frame for plotting
kde_mix_ml_frame = data.frame(Quantile = kde_mix_ml$eval$mQ, Density = kde_mix_ml$dens)

# Plot both kernel smoothing and the density
plot(density_mix, type = "l", main = "Epanechnikov with ml")
lines(kde_mix_ml_frame, col = 'red', lwd = 2)
lines(x_values_mix, density_values_mix, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_1_ls = npudensbw(mQ_1, ckertype = "gaussian", bwmethod = "cv.ls") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_1_ls = npudens(mQ_1, bws = bw_1_ls, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_1_ls_frame = data.frame(Quantile = kde_1_ls$eval$mQ, Density = kde_1_ls$dens)

# Plot both kernel smoothing and the density
plot(density_1, type = "l", main = "Gaussian with ls")
lines(kde_1_ls_frame, col = 'red', lwd = 2)
lines(x_values_1, density_values_1, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_1_ml = npudensbw(mQ_1, ckertype = "gaussian", bwmethod = "cv.ml") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_1_ml = npudens(mQ_1, bws = bw_1_ml, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_1_ml_frame = data.frame(Quantile = kde_1_ml$eval$mQ, Density = kde_1_ml$dens)

# Plot both kernel smoothing and the density
plot(density_1, type = "l", main = "Gaussian with ml")
lines(kde_1_ml_frame, col = 'red', lwd = 2)
lines(x_values_1, density_values_1, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_2_ls = npudensbw(mQ_2, ckertype = "gaussian", bwmethod = "cv.ls") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_2_ls = npudens(mQ_2, bws = bw_2_ls, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_2_ls_frame = data.frame(Quantile = kde_2_ls$eval$mQ, Density = kde_2_ls$dens)

# Plot both kernel smoothing and the density
plot(density_2, type = "l", main = "Gaussian with ls")
lines(kde_2_ls_frame, col = 'red', lwd = 2)
lines(x_values_2, density_values_2, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_2_ml = npudensbw(mQ_2, ckertype = "gaussian", bwmethod = "cv.ml") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_2_ml = npudens(mQ_2, bws = bw_2_ml, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_2_ml_frame = data.frame(Quantile = kde_2_ml$eval$mQ, Density = kde_2_ml$dens)

# Plot both kernel smoothing and the density
plot(density_2, type = "l", main = "Gaussian with ml")
lines(kde_2_ml_frame, col = 'red', lwd = 2)
lines(x_values_2, density_values_2, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_mix_ls = npudensbw(mQ_mix, ckertype = "gaussian", bwmethod = "cv.ls") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_mix_ls = npudens(mQ_mix, bws = bw_mix_ls, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_mix_ls_frame = data.frame(Quantile = kde_mix_ls$eval$mQ, Density = kde_mix_ls$dens)

# Plot both kernel smoothing and the density
plot(density_mix, type = "l", main = "Gaussian with ls")
lines(kde_mix_ls_frame, col = 'red', lwd = 2)
lines(x_values_mix, density_values_mix, col = 'blue', lwd = 2)

#############################################################################################

# Plot the smoothed density with optimal kernel
# Compute optimal bandwidth using cross-validation on Least Square
bw_mix_ml = npudensbw(mQ_mix, ckertype = "gaussian", bwmethod = "cv.ml") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_mix_ml = npudens(mQ_mix, bws = bw_mix_ml, ckertype = "gaussian")

# Convert KDE result to data frame for plotting
kde_mix_ml_frame = data.frame(Quantile = kde_mix_ml$eval$mQ, Density = kde_mix_ml$dens)

# Plot both kernel smoothing and the density
plot(density_mix, type = "l", main = "Gaussian with ml")
lines(kde_mix_ml_frame, col = 'red', lwd = 2)
lines(x_values_mix, density_values_mix, col = 'blue', lwd = 2)

