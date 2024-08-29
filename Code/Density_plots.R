# Import libraries
library(DMQ)
library(np)
library(ks)
library(ggplot)
library(plotly)

# Source functions in source.R
source("Density/source.R")

############################# Read necessary data ##############################

# Read the cleaned data
apple = read.csv("Data/Derived/apple_cleaned.csv")
FTSE = read.csv("Data/Derived/FTSE_cleaned.csv")

# Read the RDS file fit
Fit_apple = readRDS(file = "Data/Fitted_RDS/Fit_apple.rds")
Fit_FTSE = readRDS(file = "Data/Fitted_RDS/Fit_FTSE.rds")

# Extract the quantiles from the fit and eliminate the first column
quantiles_apple = Fit_apple$lFilter$mQ
quantiles_apple = quantiles_apple[,-1]
apple = apple[-1, ]

# Extract the quantiles from the fit and eliminate the first column
quantiles_FTSE = Fit_FTSE$lFilter$mQ
quantiles_FTSE = quantiles_FTSE[,-1]
FTSE = FTSE[-1, ]

######################### Careful, took long time to run #######################

# Extract the year from the Date column
apple$Year = year(apple$Date)
apple$Month = month(apple$Date)

# Iterate through each year and month
for (year in 2014:2024) {
  for (month in 1:12) {
    
    # Create the name for the variable
    var_name = paste(year, month, sep = "_")
    
    # Find the indices for the current year and month in data1
    indices = which(apple$Year == as.character(year) & apple$Month == month)
    
    # Select data from data2 using the indices
    selected_data = sort(as.vector(quantiles_apple[, unlist(indices)]))
    
    # Assign the selected data to a dynamically named variable
    assign(var_name, selected_data)
  }
}

# Store dataset names in a vector
dataset_names = c("2014_6", "2014_7", "2014_8", "2014_9", "2014_10",
                  "2014_11", "2014_12", "2015_1", "2015_2", 
                  "2015_3", "2015_4", "2015_5", "2015_6", "2015_7", 
                  "2015_8", "2015_9", "2015_10", "2015_11", "2015_12",
                  "2016_1", "2016_2", "2016_3", "2016_4", "2016_5",
                  "2016_6", "2016_7", "2016_8", "2016_9", "2016_10", 
                  "2016_11", "2016_12", "2017_1", "2017_2", "2017_3", 
                  "2017_4", "2017_5", "2017_6", "2017_7", "2017_8", 
                  "2017_9", "2017_10", "2017_11", "2017_12", "2018_1",
                  "2018_2", "2018_3", "2018_4", "2018_5", "2018_6",
                  "2018_7", "2018_8", "2018_9", "2018_10", "2018_11", 
                  "2018_12", "2019_1", "2019_2", "2019_3",
                  "2019_4", "2019_5", "2019_6", "2019_7", "2019_8",
                  "2019_9", "2019_10", "2019_11", "2019_12", "2020_1",
                  "2020_2", "2020_3", "2020_4", "2020_5", "2020_6", 
                  "2020_7", "2020_8", "2020_9" ,"2020_10", "2020_11",
                  "2020_12", "2021_1", "2021_2", "2021_3", "2021_4",
                  "2021_5", "2021_6", "2021_7", "2021_8", "2021_9", 
                  "2021_10", "2021_11", "2021_12", "2022_1", "2022_2",
                  "2022_3", "2022_4", "2022_5", "2022_6", "2022_7", 
                  "2022_8", "2022_9", "2022_10", "2022_11", "2022_12",
                  "2023_1", "2023_2", "2023_3", "2023_4", "2023_5",
                  "2023_6", "2023_7", "2023_8", "2023_9", "2023_10", 
                  "2023_11", "2023_12", "2024_1", "2024_2", "2024_3",
                  "2024_4", "2024_5", "2024_6") 

# Initialize a list to store the KDE results
kde_result_apple = list()

# Loop through the dataset names and compute KDE
for (name in dataset_names) {
  # Retrieve the dataset using the name
  data = get(name)
  
  # Extract the year and month from the dataset name
  year = substr(name, 1, 4)
  month = substr(name, 6, 7)
  
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(data, ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(data, bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$data, Density = kde_ls$dens, Year = year, Month = month)
  
  # Store the KDE result in the list
  kde_result_apple[[paste(year, month, sep = "_")]] = kde_ls_frame
}

# Save the kde results into RDS file
saveRDS(kde_result_apple, file = "Data/Fitted_RDS/kde_result_apple.rds")

# Extract the year from the Date column
FTSE$Year = year(FTSE$Date)
FTSE$Month = month(FTSE$Date)

# Iterate through each year and month
for (year in 2014:2024) {
  for (month in 1:12) {
    
    # Create the name for the variable
    var_name = paste(year, month, sep = "_")
    
    # Find the indices for the current year and month in data1
    indices = which(FTSE$Year == as.character(year) & FTSE$Month == month)
    
    # Select data from data2 using the indices
    selected_data = sort(as.vector(quantiles_FTSE[, unlist(indices)]))
    
    # Assign the selected data to a dynamically named variable
    assign(var_name, selected_data)
  }
}

# Store dataset names in a vector
dataset_names = c("2014_6", "2014_7", "2014_8", "2014_9", "2014_10",
                  "2014_11", "2014_12", "2015_1", "2015_2", 
                  "2015_3", "2015_4", "2015_5", "2015_6", "2015_7", 
                  "2015_8", "2015_9", "2015_10", "2015_11", "2015_12",
                  "2016_1", "2016_2", "2016_3", "2016_4", "2016_5",
                  "2016_6", "2016_7", "2016_8", "2016_9", "2016_10", 
                  "2016_11", "2016_12", "2017_1", "2017_2", "2017_3", 
                  "2017_4", "2017_5", "2017_6", "2017_7", "2017_8", 
                  "2017_9", "2017_10", "2017_11", "2017_12", "2018_1",
                  "2018_2", "2018_3", "2018_4", "2018_5", "2018_6",
                  "2018_7", "2018_8", "2018_9", "2018_10", "2018_11", 
                  "2018_12", "2019_1", "2019_2", "2019_3",
                  "2019_4", "2019_5", "2019_6", "2019_7", "2019_8",
                  "2019_9", "2019_10", "2019_11", "2019_12", "2020_1",
                  "2020_2", "2020_3", "2020_4", "2020_5", "2020_6", 
                  "2020_7", "2020_8", "2020_9" ,"2020_10", "2020_11",
                  "2020_12", "2021_1", "2021_2", "2021_3", "2021_4",
                  "2021_5", "2021_6", "2021_7", "2021_8", "2021_9", 
                  "2021_10", "2021_11", "2021_12", "2022_1", "2022_2",
                  "2022_3", "2022_4", "2022_5", "2022_6", "2022_7", 
                  "2022_8", "2022_9", "2022_10", "2022_11", "2022_12",
                  "2023_1", "2023_2", "2023_3", "2023_4", "2023_5",
                  "2023_6", "2023_7", "2023_8", "2023_9", "2023_10", 
                  "2023_11", "2023_12", "2024_1", "2024_2", "2024_3",
                  "2024_4", "2024_5", "2024_6") 

# Initialize a list to store the KDE results
kde_result_FTSE = list()

# Loop through the dataset names and compute KDE
for (name in dataset_names) {
  # Retrieve the dataset using the name
  data = get(name)
  
  # Extract the year and month from the dataset name
  year = substr(name, 1, 4)
  month = substr(name, 6, 7)
  
  # Compute optimal bandwidth using cross-validation on Maximum Likelihood
  bw_ls = npudensbw(data, ckertype = "gaussian", bwmethod = "cv.ls")
  
  # Perform kernel density estimation using the optimal bandwidth
  kde_ls = npudens(data, bws = bw_ls, ckertype = "gaussian")
  
  # Convert KDE result to data frame for plotting
  kde_ls_frame = data.frame(Quantile = kde_ls$eval$data, Density = kde_ls$dens, Year = year, Month = month)
  
  # Store the KDE result in the list
  kde_result_FTSE[[paste(year, month, sep = "_")]] = kde_ls_frame
}

# Save the kde results into RDS file
saveRDS(kde_result_FTSE, file = "Data/Fitted_RDS/kde_result_FTSE.rds")

################################ Apple Inc #####################################

# Read the kde results
kde_result_apple = readRDS(file = "Data/Fitted_RDS/kde_result_apple.rds")

# Combine all KDE results into a single data frame
kde_all_apple = do.call(rbind, kde_result_apple)

# Prepare the data by ensuring Month and Month_num are correctly formatted
kde_all_apple$Month = factor(kde_all_apple$Month, levels = 1:12, labels = month.name)
kde_all_apple$Month_num = as.numeric(kde_all_apple$Month) 

# Set color palette
colours = RColorBrewer::brewer.pal(12, "Set3")

# Assuming 'years' is a vector of years to plot
years = unique(kde_all_apple$Year)

for (year in years) {
  # Subset the data for the current year
  kde_year = filter(kde_all_apple, Year == year)
  
  # Initialize an empty plotly object
  p = plot_ly() 
  
  # Add each month as a separate trace
  for (m in unique(kde_year$Month)) {
    month_data = filter(kde_year, Month == m)
    p = add_trace(p, data = month_data, x = ~Quantile, y = ~Month, z = ~Density,
                   type = 'scatter3d', mode = 'lines',
                   line = list(color = colors[m], width = 3),
                   name = m)  # Use month names directly for the legend
  }
  
  # Layout with camera and legend configuration
  p = p %>% layout(title = paste("3D Density Plot for Year", year),
                    scene = list(
                      xaxis = list(title = "Quantile"),
                      yaxis = list(title = "Month"),
                      zaxis = list(title = "Density"), 
                      camera = list(eye = list(x = -1.5, y = 2.5, z = 0.3))),
                    legend = list(title = list(text = 'Months'), orientation = "v"))
  
  # Print the plot
  print(p)
}


################################ FTSE 100 Index ################################

# Read the kde results
kde_result_FTSE = readRDS(file = "Data/Fitted_RDS/kde_result_FTSE.rds")

# Combine all KDE results into a single data frame
kde_all_FTSE = do.call(rbind, kde_result_FTSE)

# Prepare the data by ensuring Month and Month_num are correctly formatted
kde_all_FTSE$Month = factor(kde_all_FTSE$Month, levels = 1:12, labels = month.name)
kde_all_FTSE$Month_num = as.numeric(kde_all_FTSE$Month) 

# Set color palette
colours = RColorBrewer::brewer.pal(12, "Set3")

# Assuming 'years' is a vector of years to plot
years = unique(kde_all_FTSE$Year)

for (year in years) {
  # Subset the data for the current year
  kde_year = filter(kde_all_FTSE, Year == year)
  
  # Initialize an empty plotly object
  p = plot_ly() 
  
  # Add each month as a separate trace
  for (m in unique(kde_year$Month)) {
    month_data = filter(kde_year, Month == m)
    p = add_trace(p, data = month_data, x = ~Quantile, y = ~Month, z = ~Density,
                  type = 'scatter3d', mode = 'lines',
                  line = list(color = colors[m], width = 3),
                  name = m)  # Use month names directly for the legend
  }
  
  # Layout with camera and legend configuration
  p = p %>% layout(title = paste("3D Density Plot for Year", year),
                   scene = list(
                     xaxis = list(title = "Quantile"),
                     yaxis = list(title = "Month"),
                     zaxis = list(title = "Density"), 
                     camera = list(eye = list(x = -1.5, y = 2.5, z = 0.3))),
                   legend = list(title = list(text = 'Months'), orientation = "v"))
  
  # Print the plot
  print(p)
}
