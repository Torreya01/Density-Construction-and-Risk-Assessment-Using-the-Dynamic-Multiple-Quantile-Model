# Density Construction and Risk Assessment Using the Dynamic Multiple Quantile Model
## Thesis description
The thesis utilised the Dynamic Multiple Quantile (DMQ) model to estimate 99 quantiles for Apple Inc. and FTSE 100 Index returns for each day from June 2014, to June, 2024. 
Then applied the Kernel Density Estimation (KDE) method to construct smooth density out of these quantiles. The Extreme Value Theory (EVT) was then employed to analyse the tail behaviour of the density plots.
This thesis provides a fundamental framework for future research and practical applications in financial market analysis, paving the way for more sophisticated risk management tools and strategies in the future.

## Directories
### Data/

This directory contains all the raw, derived and fitted RDS data.

- Raw:
  - apple_share_prices.csv is the closing share prices for Apple Inc. from June 23rd, 2014, to June 21st, 2024.
  - apple_share_prices_new.csv is the closing share prices for Apple Inc. from June 23rd, 2014, to June 28th, 2024.
  - FTSE_100.csv is the closing share prices for FTSE 100 Index from June 23rd, 2014, to June 21st, 2024.
  - FTSE_100_new.csv is the closing share prices for FTSE 100 Index from June 23rd, 2014, to June 28th, 2024.

The relevant Apple Inc. data is available through [here](https://uk.finance.yahoo.com/quote/AAPL/history/) and the FTSE 100 Index data is available through [here](https://www.wsj.com/market-data/quotes/index/UK/FTSE%20UK/UKX/historical-prices).

- Derived:
  - apple_cleaned.csv and apple_new_cleaned.csv are the cleaned data obatained from the raw Apple Inc. data.
  - FTSE_cleaned.csv and FTSE_new_cleaned.csv are the cleaned data obtained from the raw FTSE 100 Index data.
  - apple_return.csv and apple_return_new.csv are the financial returns calculated from apple_cleaned.csv and apple_new_cleaned.csv.
  - FTSE_return.csv and FTSE_return_new.csv are the financial returns calculated from FTSE_cleaned.csv and FTSE_new_cleaned.csv.
  - forecast_apple.csv and forecast_FTSE.csv are the week forecast of Apple Inc. and FTSE 100 Index from June 24th, 2024 to June 28th, 2024 estimated from the DMQ model fit.

- Fitted_RDS:
  - Fit_apple.rds and Fit_FTSE.rds are the fitted DMQ model results from apple_return.csv and FTSE_return.csv.
  - Fit_apple_new.rds and Fit_FTSE_new.rds are the fitted DMQ model results from apple_return_new.csv and FTSE_return_new.csv.
  - Simulation_*.rds are the fitted DMQ model results for the simulation study discussed in the dissertation.
  - kde_result_apple.rds and kde_result_FTSE.rds are the estimated density using KDE from Fit_apple.rds and Fit_FTSE.rds
    
### Code/

This directory contains all the code used in the dissertation.

- source.R contains all the helper function needed in this dissertation.
- Kernel_simulation.R and Bandwidth_simulation.R generate figures needed in the Methods section in dissertation.
- Simulation_study.R contains code that find the optimal kernel and bandwidth applied in the KDE method.
- Fit_the_DMQ_model.R contains code that fit Apple Inc. and FTSE 100 Index into the DMQ model.
- Density_plots.R contains code that applies results reported in Simulation_study.R to estimate densities for estiamted quantiles from DMQ model.
- Forecast_density.R contains code that applies results reported in Simulation_study.R to estimate densities for estiamted forecast quantiles from DMQ model.
- Kurtosis_skewness.R contains code that calculates kurtosis, variances and skewness of the estiamted quantiles from DMQ model.
- Tail_behaviour.R contains code that applies EVT to the forecast density.

The order mentioned above is the recommended sequence for running the code.
  
### Plot/

This directory contains all the figures used in the dissertation.

- Time series: This folder contains the time series figrues for both closing prices and financial returns of Apple Inc. and FTSE 100 Index.
- Methods: This folder contains the results from running the Kernel_simulation.R and Bandwidth_simulation.R, about what how density estiamtes would alter when choosing different kernel function and bandwidth.
- Density: This folder contains the monthly density estimate figrues for Apple Inc. and FTSE 100 Index from 2014 to 2024.
- Kurtosis and skewness: This folder contains the monthly kurtosis, variances and skewness figrues for Apple Inc. and FTSE 100 Index.
- GPD: This folder contains QQ plots and figures showing the parameter estimates and 95% confidence interval from the Generalised Pareto Distriution.
- Forecast: This folder contains all the figures using the forecast quantiles estimated from DMQ model, QQ plot, daily and monthly density figures.

