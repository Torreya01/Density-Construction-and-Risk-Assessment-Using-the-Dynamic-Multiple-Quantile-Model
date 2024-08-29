# Import libraries
library(DMQ)
library(np)
library(ks)
library(extRemes)
library(evd)
library(ggplot2)
library(boot)
library(ismev)

# Source functions in source.R
source("Density/source.R")

# Import returns and forecasts for apple and FTSE
apple_diff = read.csv("Data/Derived/apple_return.csv")
FTSE_diff = read.csv("Data/Derived/FTSE_return.csv")
Forecast_apple = read.csv("Data/Derived/forecast_apple.csv")
Forecast_FTSE = read.csv("Data/Derived/forecast_FTSE.csv")

################################## QQ plot #####################################

# QQ plot for full Apple data
qqnorm(apple_diff$x, main = "QQ Plot for the original log-returns for Apple Inc.")
qqline(apple_diff$x, col = "red")

# QQ plot for full FTSE data
qqnorm(FTSE_diff$x, main = "QQ Plot for the original log-returns for FSTE 100 Index")
qqline(FTSE_diff$x, col = "red")

# QQ plot for the forecast Apple data
qqnorm(Forecast_apple$x, main = "QQ Plot for the week forecast log-returns for Apple Inc.")
qqline(Forecast_apple$x, col = "red")

# QQ plot for the forecast FTSE data
qqnorm(Forecast_FTSE$x, main = "QQ Plot for the week forecast log-returns for FTSE 100 Index")
qqline(Forecast_FTSE$x, col = "red")

########################## Tail Extreme Value Theory ###########################

# Take the Modulus of the sample
Forecast_apple_modulus = -1 * Forecast_apple$x
Forecast_apple_modulus = sort(Forecast_apple_modulus)

# Set the threshold and extract the excess values from the right tail
right_95 = quantile(Forecast_apple_modulus, 0.95)
right_excess = Forecast_apple_modulus[Forecast_apple_modulus > right_95]

# Fitting a GPD to the excesses
gpd_fit_right = ismev::gpd.fit(right_excess, right_95)

# Generate QQ plot to assess the fit
gpd.diag(gpd_fit_right)

# Simulate data from the GPD
simulated_data_right = rgpd(25, loc = gpd_fit_right$threshold, 
                                    scale = gpd_fit_right$mle[1], 
                                    shape = gpd_fit_right$mle[2])

# Calculate the KS score
ks.test(right_excess, simulated_data_right)

scale = gpd_fit_right$mle[1]
shape = gpd_fit_right$mle[2]
exceedances = length(right_excess)
n = length(data)
VaR = right_95 + (scale / shape) * (((n / exceedances) * (1 - 0.95))^(-shape) - 1)

############################### First plot #####################################
# Assuming you have a vector 'data' of your sample returns
data = Forecast_apple_modulus  # Replace with your actual data

exceedances = seq(30, 450, by = 5)

# Define thresholds to evaluate
thresholds = data[length(data) - exceedances + 1] # Adjust the number of thresholds as needed

# Initialize vectors to store results
shape_params = numeric(length(thresholds))
shape_std_errors = numeric(length(thresholds))
num_exceedances = numeric(length(thresholds))

# Fit GPD and calculate shape parameters and confidence intervals
for (i in 1:length(thresholds)) {
  u = thresholds[i]
  excesses = data[data > u]  # Excesses over the threshold
  
  if (length(excesses) > 0) {
    fit = gpd.fit(excesses, u)  # Fit GPD with threshold 0 to excesses
    shape_params[i] = fit$mle[2]
    shape_std_errors[i] = fit$cov[2,2]  # Standard error of the shape parameter
    num_exceedances[i] = length(excesses)  # Number of exceedances
  }
}

# Calculate confidence intervals
lower_ci = shape_params - 1.96 * shape_std_errors
upper_ci = shape_params + 1.96 * shape_std_errors

# Create a data frame for plotting
plot_data = data.frame(
  NumExceedances = num_exceedances,
  ShapeParameter = shape_params,
  LowerCI = lower_ci,
  UpperCI = upper_ci
)

# Generate the plot
ggplot(plot_data, aes(x = NumExceedances)) +
  geom_line(aes(y = ShapeParameter), color = "black") +  # Main line for shape parameter
  geom_line(aes(y = LowerCI), linetype = "dashed", color = "red") +  # Lower bound
  geom_line(aes(y = UpperCI), linetype = "dashed", color = "red") +  # Upper bound
  scale_x_reverse() +  # Reverse the x-axis
  labs(title = "Estimated Shape Parameter with 0.95 Confidence Intervals for Apple Inc.",
       x = "Number of Exceedances",
       y = "Estimated Shape Parameter (CI, p = 0.95)") +
  theme_minimal() + 
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0)) 

############################ second plot #####################################

# Function to calculate the quantile at 0.95 level
quantile_95 <- function(data, indices) {
  return(quantile(data[indices], probs = 0.95))
}

# Prepare vectors to store results
thresholds <- seq(1, length(data), by = 25)
quantiles <- numeric(length(thresholds))
lower_ci <- numeric(length(thresholds))
upper_ci <- numeric(length(thresholds))

# Calculate the quantile and confidence intervals for each threshold
for (i in 1:length(thresholds)) {
  threshold_data <- data[thresholds[i]:length(data)]
  
  # Estimate the 95th quantile
  quantiles[i] <- quantile(threshold_data, probs = 0.95)
  
  # Bootstrap to get confidence intervals
  boot_results <- boot(threshold_data, quantile_95, R = 1000)
  ci <- boot.ci(boot_results, type = "perc", conf = 0.95)
  
  lower_ci[i] <- ci$percent[4]
  upper_ci[i] <- ci$percent[5]
}

# Create a data frame for plotting
plot_data <- data.frame(
  thresholds = thresholds,
  quantiles = quantiles,
  lower_ci = lower_ci,
  upper_ci = upper_ci
)

# Plotting with ggplot2
ggplot(plot_data, aes(x = thresholds, y = quantiles)) +
  geom_line(color = "black") +
  geom_line(aes(y = lower_ci), color = "red", linetype = "dashed") +
  geom_line(aes(y = upper_ci), color = "red", linetype = "dashed") +
  labs(
    x = "Number of Exceedances",
    y = "Estimated 0.95 Quantile",
    title = "Estimated 0.95 Quantile with Confidence Intervals"
  ) + scale_x_reverse() +
  theme_minimal()
