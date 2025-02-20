# Load necessary library
library(MASS)
library(DMQ)
library(np)

# Source the functions in source file
source("Density/source.R")

# Set the Gaussian samples
# Parameters for two normal Gaussians
mean1 = 0.0009
var1 = 0.0003
mean2 = 0.0005
var2 = 0.0005

# Parameters for mixed Gaussian (mixture of two normal distributions)
mean_mix1 = 0.0009
var_mix1 = 0.0003
mean_mix2 = 0.0005
var_mix2 = 0.0005
prob_mix = c(0.6, 0.4)
mean_mix = prob_mix[1] * mean_mix1 + prob_mix[2] * mean_mix2
var_mix = prob_mix[1] * var_mix1 + prob_mix[2] * var_mix2

# Parameters for student-t distribution with degree of freedom 4
t = 4
desired_variance = 0.0003
mean_t = 0.0009
standard_variance = t / (t - 2)  # This is 2 for nu = 4
tau = sqrt(desired_variance / standard_variance)

# Set the number of the distribution
small = 200
medium = 500
large = 1000

# Generate samples
set.seed(123)
samples_normal_small_1 = rnorm(small, mean1, sqrt(var1))
samples_normal_medium_1 = rnorm(medium, mean1, sqrt(var1))
samples_normal_large_1 = rnorm(large, mean1, sqrt(var1))
samples_normal_small_2 = rnorm(small, mean2, sqrt(var2))
samples_normal_medium_2 = rnorm(medium, mean2, sqrt(var2))
samples_normal_large_2 = rnorm(large, mean2, sqrt(var2))
samples_small_mixed = sample_mixed_gaussian(small, c(mean_mix1, mean_mix2), c(var_mix1, var_mix2), prob_mix)
samples_medium_mixed = sample_mixed_gaussian(medium, c(mean_mix1, mean_mix2), c(var_mix1, var_mix2), prob_mix)
samples_large_mixed = sample_mixed_gaussian(large, c(mean_mix1, mean_mix2), c(var_mix1, var_mix2), prob_mix)
samples_t_small = rt(small, 4)
samples_t_medium = rt(medium, 4)
samples_t_large = rt(large, 4)

# Transform the samples with specific mean and variance
samples_t_small_transformed = mean_t + tau * samples_t_small
samples_t_medium_transformed = mean_t + tau * samples_t_medium
samples_t_large_transformed = mean_t + tau * samples_t_large

# Create a sequence of values to cover the relevant range (true density)
x_values_1 = seq(mean1 - 4*sqrt(var1), mean1 + 4*sqrt(var1), length.out = 300)
density_values_1 = dnorm(x_values_1, mean = mean1, sd = sqrt(var1))

x_values_2 = seq(mean2 - 4*sqrt(var2), mean2 + 4*sqrt(var2), length.out = 300)
density_values_2 = dnorm(x_values_2, mean = mean2, sd = sqrt(var2))

gmm_pdf = function(x) {
  density1 = prob_mix[1] * (1 / (sqrt(2 * pi) * sqrt(var1))) * exp(-((x - mean1)^2) / (2 * sqrt(var1)^2))
  density2 = prob_mix[2] * (1 / (sqrt(2 * pi) * sqrt(var2))) * exp(-((x - mean2)^2) / (2 * sqrt(var2)^2))
  # Return the sum of the densities
  return(density1 + density2)
}
x_values_mix = seq(-20, 40, length.out = 300)
density_values_mix = sapply(x_values_mix, gmm_pdf)

############################## Set up the Gaussian distributions ###################################

# Generate quantiles using DMQ model
# Set the parameters
vTau = seq(0.01, 0.99, length.out = 99)

# Generate fit for these three samples
Fit_small_1 = EstimateDMQ(vY = samples_normal_small_1, vTau = vTau, 
                          iTau_star = 50,  # Median as reference
                          FixReference = TRUE,
                          fn.optimizer = fn.solnp)

Fit_medium_1 = EstimateDMQ(vY = samples_normal_medium_1, vTau = vTau, 
                           iTau_star = 50,  # Median as reference
                           FixReference = TRUE,
                           fn.optimizer = fn.solnp)

Fit_large_1 = EstimateDMQ(vY = samples_normal_large_1, vTau = vTau, 
                          iTau_star = 50,  # Median as reference
                          FixReference = TRUE,
                          fn.optimizer = fn.solnp)

Fit_small_2 = EstimateDMQ(vY = samples_normal_small_2, vTau = vTau, 
                          iTau_star = 50,  # Median as reference
                          FixReference = TRUE,
                          fn.optimizer = fn.solnp)

Fit_medium_2 = EstimateDMQ(vY = samples_normal_medium_2, vTau = vTau, 
                           iTau_star = 50,  # Median as reference
                           FixReference = TRUE,
                           fn.optimizer = fn.solnp)

Fit_large_2 = EstimateDMQ(vY = samples_normal_large_2, vTau = vTau, 
                          iTau_star = 50,  # Median as reference
                          FixReference = TRUE,
                          fn.optimizer = fn.solnp)

Fit_small_3 = EstimateDMQ(vY = samples_small_mixed, vTau = vTau, 
                          iTau_star = 50,  # Median as reference
                          FixReference = TRUE,
                          fn.optimizer = fn.solnp)

Fit_medium_3 = EstimateDMQ(vY = samples_medium_mixed, vTau = vTau, 
                           iTau_star = 50,  # Median as reference
                           FixReference = TRUE,
                           fn.optimizer = fn.solnp)

Fit_large_3 = EstimateDMQ(vY = samples_large_mixed, vTau = vTau, 
                          iTau_star = 50,  # Median as reference
                          FixReference = TRUE,
                          fn.optimizer = fn.solnp)

Fit_small_4 = EstimateDMQ(vY = samples_t_small_transformed, vTau = vTau, 
                          iTau_star = 50,  # Median as reference
                          FixReference = TRUE,
                          fn.optimizer = fn.solnp)

Fit_medium_4 = EstimateDMQ(vY = samples_t_medium_transformed, vTau = vTau, 
                           iTau_star = 50,  # Median as reference
                           FixReference = TRUE,
                           fn.optimizer = fn.solnp)

Fit_large_4 = EstimateDMQ(vY = samples_t_large_transformed, vTau = vTau, 
                          iTau_star = 50,  # Median as reference
                          FixReference = TRUE,
                          fn.optimizer = fn.solnp)

# Save the fit I generated
saveRDS(Fit_small_1, file = "Data/Fitted_RDS/Simulation_Fit_small_1.rds")
saveRDS(Fit_medium_1, file = "Data/Fitted_RDS/Simulation_Fit_medium_1.rds")
saveRDS(Fit_large_1, file = "Data/Fitted_RDS/Simulation_Fit_large_1.rds")
saveRDS(Fit_small_2, file = "Data/Fitted_RDS/Simulation_Fit_small_2.rds")
saveRDS(Fit_medium_2, file = "Data/Fitted_RDS/Simulation_Fit_medium_2.rds")
saveRDS(Fit_large_2, file = "Data/Fitted_RDS/Simulation_Fit_large_2.rds")
saveRDS(Fit_small_3, file = "Data/Fitted_RDS/Simulation_Fit_small_3.rds")
saveRDS(Fit_medium_3, file = "Data/Fitted_RDS/Simulation_Fit_medium_3.rds")
saveRDS(Fit_large_3, file = "Data/Fitted_RDS/Simulation_Fit_large_3.rds")
saveRDS(Fit_small_4, file = "Data/Fitted_RDS/Simulation_Fit_small_4.rds")
saveRDS(Fit_medium_4, file = "Data/Fitted_RDS/Simulation_Fit_medium_4.rds")
saveRDS(Fit_large_4, file = "Data/Fitted_RDS/Simulation_Fit_large_4.rds")

# Read the saved R files
Fit_small_1 = readRDS("Data/Fitted_RDS/Simulation_Fit_small_1.rds")
Fit_medium_1 = readRDS("Data/Fitted_RDS/Simulation_Fit_medium_1.rds")
Fit_large_1 = readRDS("Data/Fitted_RDS/Simulation_Fit_large_1.rds")
Fit_small_2 = readRDS("Data/Fitted_RDS/Simulation_Fit_small_2.rds")
Fit_medium_2 = readRDS("Data/Fitted_RDS/Simulation_Fit_medium_2.rds")
Fit_large_2 = readRDS("Data/Fitted_RDS/Simulation_Fit_large_2.rds")
Fit_small_3 = readRDS("Data/Fitted_RDS/Simulation_Fit_small_3.rds")
Fit_medium_3 = readRDS("Data/Fitted_RDS/Simulation_Fit_medium_3.rds")
Fit_large_3 = readRDS("Data/Fitted_RDS/Simulation_Fit_large_3.rds")
Fit_small_4 = readRDS("Data/Fitted_RDS/Simulation_Fit_small_4.rds")
Fit_medium_4 = readRDS("Data/Fitted_RDS/Simulation_Fit_medium_4.rds")
Fit_large_4 = readRDS("Data/Fitted_RDS/Simulation_Fit_large_4.rds")

# Extract quantiles from the fit
quantiles_small_1 = Fit_small_1$lFilter$mQ[,-1]
quantiles_medium_1 = Fit_medium_1$lFilter$mQ[,-1]
quantiles_large_1 = Fit_large_1$lFilter$mQ[,-1]
quantiles_small_2 = Fit_small_2$lFilter$mQ[,-1]
quantiles_medium_2 = Fit_medium_2$lFilter$mQ[,-1]
quantiles_large_2 = Fit_large_2$lFilter$mQ[,-1]
quantiles_small_3 = Fit_small_3$lFilter$mQ[,-1]
quantiles_medium_3 = Fit_medium_3$lFilter$mQ[,-1]
quantiles_large_3 = Fit_large_3$lFilter$mQ[,-1]
quantiles_small_4 = Fit_small_4$lFilter$mQ[,-1]
quantiles_medium_4 = Fit_medium_4$lFilter$mQ[,-1]
quantiles_large_4 = Fit_large_4$lFilter$mQ[,-1]

###################### Generate quantiles using DMQ model ######################

# Results for the first Gaussian distribution
## Small samples
optimal_1_ls_small = MSE_Gaussian(quantiles_small_1, "epanechnikov", "cv.ls", mean1, var1, small)
optimal_1_ml_small = MSE_Gaussian(quantiles_small_1, "epanechnikov", "cv.ml", mean1, var1, small)
gaussian_1_ls_small = MSE_Gaussian(quantiles_small_1, "gaussian", "cv.ls", mean1, var1, small)
gaussian_1_ml_small = MSE_Gaussian(quantiles_small_1, "gaussian", "cv.ml", mean1, var1, small)

## Medium samples
optimal_1_ls_medium = MSE_Gaussian(quantiles_medium_1, "epanechnikov", "cv.ls", mean1, var1, medium)
optimal_1_ml_medium = MSE_Gaussian(quantiles_medium_1, "epanechnikov", "cv.ml", mean1, var1, medium)
gaussian_1_ls_medium = MSE_Gaussian(quantiles_medium_1, "gaussian", "cv.ls", mean1, var1, medium)
gaussian_1_ml_medium = MSE_Gaussian(quantiles_medium_1, "gaussian", "cv.ml", mean1, var1, medium)

## Large samples
optimal_1_ls_large = MSE_Gaussian(quantiles_large_1, "epanechnikov", "cv.ls", mean1, var1, large)
optimal_1_ml_large = MSE_Gaussian(quantiles_large_1, "epanechnikov", "cv.ml", mean1, var1, large)
gaussian_1_ls_large = MSE_Gaussian(quantiles_large_1, "gaussian", "cv.ls", mean1, var1, large)
gaussian_1_ml_large = MSE_Gaussian(quantiles_large_1, "gaussian", "cv.ml", mean1, var1, large)

# Results for the second Gaussian distribution
## Small samples
optimal_2_ls_small = MSE_Gaussian(quantiles_small_2, "epanechnikov", "cv.ls", mean2, var2, small)
optimal_2_ml_small = MSE_Gaussian(quantiles_small_2, "epanechnikov", "cv.ml", mean2, var2, small)
gaussian_2_ls_small = MSE_Gaussian(quantiles_small_2, "gaussian", "cv.ls", mean2, var2, small)
gaussian_2_ml_small = MSE_Gaussian(quantiles_small_2, "gaussian", "cv.ml", mean2, var2, small)

## Medium samples
optimal_2_ls_medium = MSE_Gaussian(quantiles_medium_2, "epanechnikov", "cv.ls", mean2, var2, medium)
optimal_2_ml_medium = MSE_Gaussian(quantiles_medium_2, "epanechnikov", "cv.ml", mean2, var2, medium)
gaussian_2_ls_medium = MSE_Gaussian(quantiles_medium_2, "gaussian", "cv.ls", mean2, var2, medium)
gaussian_2_ml_medium = MSE_Gaussian(quantiles_medium_2, "gaussian", "cv.ml", mean2, var2, medium)

## Large samples
optimal_2_ls_large = MSE_Gaussian(quantiles_large_2, "epanechnikov", "cv.ls", mean2, var2, large)
optimal_2_ml_large = MSE_Gaussian(quantiles_large_2, "epanechnikov", "cv.ml", mean2, var2, large)
gaussian_2_ls_large = MSE_Gaussian(quantiles_large_2, "gaussian", "cv.ls", mean2, var2, large)
gaussian_2_ml_large = MSE_Gaussian(quantiles_large_2, "gaussian", "cv.ml", mean2, var2, large)

# Results for the third Gaussian mix distribution
## Small samples
optimal_3_ls_small = MSE_Gaussian(quantiles_small_3, "epanechnikov", "cv.ls", mean_mix, var_mix, small)
optimal_3_ml_small = MSE_Gaussian(quantiles_small_3, "epanechnikov", "cv.ml", mean_mix, var_mix, small)
gaussian_3_ls_small = MSE_Gaussian(quantiles_small_3, "gaussian", "cv.ls", mean_mix, var_mix, small)
gaussian_3_ml_small = MSE_Gaussian(quantiles_small_3, "gaussian", "cv.ml", mean_mix, var_mix, small)

## Medium samples
optimal_3_ls_medium = MSE_Gaussian(quantiles_medium_3, "epanechnikov", "cv.ls", mean_mix, var_mix, medium)
optimal_3_ml_medium = MSE_Gaussian(quantiles_medium_3, "epanechnikov", "cv.ml", mean_mix, var_mix, medium)
gaussian_3_ls_medium = MSE_Gaussian(quantiles_medium_3, "gaussian", "cv.ls", mean_mix, var_mix, medium)
gaussian_3_ml_medium = MSE_Gaussian(quantiles_medium_3, "gaussian", "cv.ml", mean_mix, var_mix, medium)

## Large samples
optimal_3_ls_large = MSE_Gaussian(quantiles_large_3, "epanechnikov", "cv.ls", mean_mix, var_mix, large)
optimal_3_ml_large = MSE_Gaussian(quantiles_large_3, "epanechnikov", "cv.ml", mean_mix, var_mix, large)
gaussian_3_ls_large = MSE_Gaussian(quantiles_large_3, "gaussian", "cv.ls", mean_mix, var_mix, large)
gaussian_3_ml_large = MSE_Gaussian(quantiles_large_3, "gaussian", "cv.ml", mean_mix, var_mix, large)

# Results for the student-t distribution
## Small samples
optimal_4_ls_small = MSE_t(quantiles_small_4, "epanechnikov", "cv.ls", t, small)
optimal_4_ml_small = MSE_t(quantiles_small_4, "epanechnikov", "cv.ml", t, small)
gaussian_4_ls_small = MSE_t(quantiles_small_4, "gaussian", "cv.ls", t, small)
gaussian_4_ml_small = MSE_t(quantiles_small_4, "gaussian", "cv.ml", t, small)

## Medium samples
optimal_4_ls_medium = MSE_t(quantiles_medium_4, "epanechnikov", "cv.ls", t, medium)
optimal_4_ml_medium = MSE_t(quantiles_medium_4, "epanechnikov", "cv.ml", t, medium)
gaussian_4_ls_medium = MSE_t(quantiles_medium_4, "gaussian", "cv.ls", t, medium)
gaussian_4_ml_medium = MSE_t(quantiles_medium_4, "gaussian", "cv.ml", t, medium)

## Large samples
optimal_4_ls_large = MSE_t(quantiles_large_4, "epanechnikov", "cv.ls", t, large)
optimal_4_ml_large = MSE_t(quantiles_large_4, "epanechnikov", "cv.ml", t, large)
gaussian_4_ls_large = MSE_t(quantiles_large_4, "gaussian", "cv.ls", t, large)
gaussian_4_ml_large = MSE_t(quantiles_large_4, "gaussian", "cv.ml", t, large)

