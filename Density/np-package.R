# Load libraries
library(np)
library(ks)
library(ggplot2)
library(gridExtra)

################################################################################
# Try the triangular kernel

# Load quantiles from the saved fit
Fit = readRDS("Data/fit_DMQ_SP500.rds")
quantiles = Fit$lFilter$mQ
quantiles = quantiles[,-1]
selected_columns = c(1300, 1301, 1302, 1303, 1304, 1305, 1306)
mQ = quantiles[, selected_columns]

################################################################################
# Try the epanechnikov kernel

# Compute optimal bandwidth using cross-validation on Least Square
bw_ls = npudensbw(mQ[,1], ckertype = "epanechnikov", bwmethod = "cv.ls") 

# Perform kernel density estimation using Epanechnikov kernel and optimal bandwidth
kde_ls = npudens(mQ[,1], bws = bw_ls, ckertype = "epanechnikov")

# Convert KDE result to data frame for plotting
kde_ls_frame = data.frame(Quantile = kde_ls$eval$mQ, Density = kde_ls$dens)

# Plotting using ggplot2
ggplot(kde_ls_frame, aes(x = Quantile, y = Density)) +
  geom_line() + 
  ggtitle("KDE with Optimal Bandwidth selected from ls using Epanechnikov kernel") +
  xlab("Quantile") + 
  ylab("Density") + theme_minimal()

###############################################################################

# Compute optimal bandwidth using cross-validation on Maximum Likelihood
bw_ml = npudensbw(mQ, ckertype = "epanechnikov", bwmethod = "cv.ml")

# Perform kernel density estimation using the optimal bandwidth
kde_ml = npudens(mQ, bws = bw_ml, ckertype = "epanechnikov")

# Convert KDE result to data frame for plotting
kde_ml_frame = data.frame(Quantile = kde_ml$eval$mQ, Density = kde_ml$dens)

# Plotting using ggplot2
ggplot(kde_ml_frame, aes(x = Quantile, y = Density)) +
  geom_line() + 
  ggtitle("KDE with Optimal Bandwidth selected from ml using Epanechnikov kernel") +
  xlab("Quantile") + 
  ylab("Density") + theme_minimal()

###############################################################################

par(mfrow = c(5,1))

for (i in 1:length(selected_columns)) {
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ml = npudensbw(mQ[, i], ckertype = "gaussian", bwmethod = "cv.ml")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ml = npudens(mQ[, i], bws = bw_ml, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ml_frame = data.frame(Quantile = kde_ml$eval$mQ, Density = kde_ml$dens)
  
  # Plotting using ggplot2
  plot(kde_ml_frame, type = "l")
}

