install.packages("readr")
install.packages("DataExplorer")
install.packages("inspectdf")
install.packages("corrplot")
install.packages("ggpubr")
library(readr)
library(dplyr) 
library(tidyverse)
library(tidyr)
library(hash)
library(DataExplorer) 
library(inspectdf)
library(ggplot2)
library(corrplot)
library(broom)
library(ggpubr)
library(hrbrthemes)
library(viridis)
library(plotly)
library(nnet)



 
#setwd("C:\\Users\\s4215274\\OneDrive - University of Gloucestershire\\R-Assignment")

load_files_from_dataset <- function () {
  csv_files <- list()
  csv_files[["2014"]] <- list.files('Dataset - Assignment/2014', pattern = '\\.csv$') 
  csv_files[["2015"]] <- list.files('Dataset - Assignment/2015', pattern = '\\.csv$') 
  csv_files[["2016"]] <- list.files('Dataset - Assignment/2016', pattern = '\\.csv$') 
  csv_files[["2017"]] <- list.files('Dataset - Assignment/2017', pattern = '\\.csv$') 
  csv_files[["2018"]] <- list.files('Dataset - Assignment/2018', pattern = '\\.csv$') 
  
  return(csv_files)
}

merge_files <- function() {
  # defining the directory which contains all the data files
  data_dir <- "Dataset - Assignment"
  
  # Loop through each year folder and read in the csv files for every month
  dfs <- list()
  for (year in dir(data_dir)) {
    year_dir <- file.path(data_dir, year)
    for (csv_file in dir(year_dir, pattern = ".csv")) {
      # extract the month and year information from file name
      month <- strsplit(csv_file, "_")[[1]][4]
      year <- as.character(strsplit(csv_file, "_")[[1]][5])
      year <- sub(".csv$", "", year)
      df <- read_csv(file.path(year_dir, csv_file))
      # create columns of year and month and populate with correct year and month
      df <- df %>% mutate(Year = year, Month = month)
      dfs[[length(dfs) + 1]] <- df
    }
  }
  
  # Concatenate all the dataframes into one
  merged_df <- bind_rows(dfs)
  # Save the merged dataframe to a new csv file
  return (merged_df)
}

merged_df <- merge_files()

## DATA CLEANING ##

remove_percentage_columns <- function(merged_df){
  # remove the percentage columns 
  merged_df <- merged_df[, !grepl("^Percentage", colnames(merged_df))]
  
  return(merged_df)
}

rename_columns <- function(merged_df) {
  # rename the columns to lower case for making it use friendly
  colnames(merged_df) <- tolower(colnames(merged_df))
  # convert the column to shorter ones for ease of use
  name_mapping <- c('number of homicide convictions' = 'successful_homicide',
                    'number of homicide unsuccessful' = 'unsuccessful_homicide',
                    'number of offences against the person convictions' = 'successful_against_person',
                    'number of offences against the person unsuccessful' = 'unsuccessful_against_person',
                    'number of sexual offences convictions' = 'successful_sexual_offences',
                    'number of sexual offences unsuccessful' = 'unsuccessful_sexual_offences',
                    'number of burglary convictions' = 'successful_burglary', 'number of burglary unsuccessful' = 'unsuccessful_burglary',
                    'number of robbery convictions'= 'successful_robbery', 'number of robbery unsuccessful' = 'unsuccessful_robbery',
                    'number of theft and handling convictions' = 'successful_theft',
                    'number of theft and handling unsuccessful' = 'unsuccessful_theft',
                    'number of fraud and forgery convictions' = 'successful_fraud',
                    'number of fraud and forgery unsuccessful' = 'unsuccessful_fraud',
                    'number of criminal damage convictions' = 'successful_criminal_damage',
                    'number of criminal damage unsuccessful' = 'unsuccessful_criminal_damage',
                    'number of drugs offences convictions' = 'successful_drugs',
                    'number of drugs offences unsuccessful' = 'unsuccessful_drugs',
                    'number of public order offences convictions' = 'successful_public_order',
                    'number of public order offences unsuccessful' = 'unsuccessful_public_order',
                    'number of all other offences (excluding motoring) convictions' = 'successful_other',
                    'number of all other offences (excluding motoring) unsuccessful' = 'unsuccessful_other',
                    'number of motoring offences convictions' = 'successful_motoring',
                    'number of motoring offences unsuccessful' = 'unsuccessful_motoring',
                    'number of admin finalised unsuccessful' = 'unsuccessful_admin',
                    'year' = 'year', 'month' = 'month')
  
  
  colnames(merged_df) <- name_mapping[colnames(merged_df)]
  colnames(merged_df)[1] <- "county"
  return(merged_df)
}

label_counties <- function(merged_df){
  # Define the list of rural counties
  rural_counties <- c("Cumbria", "Dorset", "Durham", "Dyfed Powys", "Gloucestershire",
                      "Lincolnshire", "Norfolk", "North Wales", "North Yorkshire",
                      "South Wales", "Suffolk", "Warwickshire", "Wiltshire")
  
  # Adding a column indicating rural or urban based on the county
  merged_df$region <- ifelse(merged_df$county %in% rural_counties, "Rural", "Urban")
  
  return(merged_df)
}

remove_national_data <- function(merged_df) {
  merged_df <- merged_df[merged_df$county != 'National', ]
  return(merged_df)
}

remove_na <- function(merged_df) {
  merged_df[is.na(merged_df)] <- 0
  return(merged_df)
}

merged_df <- remove_percentage_columns(merged_df)
merged_df <- rename_columns(merged_df)
merged_df <- label_counties(merged_df)
merged_df <- remove_national_data(merged_df)
merged_df <- remove_na(merged_df)
print(colnames(merged_df))
view(merged_df)





## DATA DESCRIPTION ##

select_columns_of_interest <- function(merged_df) {
  data_exploration_df <- merged_df[, !(names(merged_df) %in% c("year", "month", "county", "region"))]
  return(data_exploration_df)
} 

describe_data <- function(merged_df){
  DataExplorer::create_report(merged_df, report_title = "Data Exploration Report") 
  inspect_num(merged_df) %>% show_plot()
  inspect_types(merged_df) %>% show_plot()
}

describe_data(merged_df)
data_exploration_df <- select_columns_of_interest(merged_df)


data_correlation <- function(data_exploration_df){
  
  cor_matrix <- cor(data_exploration_df)
  
  corrplot(cor_matrix, type = "upper", order = "hclust", tl.cex = 0.7, tl.col = "black", is.corr = TRUE, mar = c(0, 0, 0, 0)) 
  heatmap(cor_matrix, 
          col = colorRampPalette(c("blue", "white", "red"))(100),
          main = "Correlation Heatmap")
}

data_correlation(data_exploration_df)


## Linear Regression Analysis for Successful Drug and Fraud Cases Convictions ##

linear_regression_analysis <- function(merged_df){
  successful_fraud_drugs_lm <- lm(successful_drugs ~ successful_fraud, data = merged_df[c('successful_fraud', 'successful_drugs')])
  summary(successful_fraud_drugs_lm)
  
  plot(successful_drugs ~ successful_fraud, data = merged_df[c('successful_fraud', 'successful_drugs')])
  
  
  successful_fraud_drugs <- ggplot(merged_df[c('successful_fraud', 'successful_drugs')], aes(x=successful_drugs, y=successful_fraud)) + geom_point() + geom_smooth(method="lm", col="black") +
    theme_minimal()
  
  
  successful_fraud_drugs<- successful_fraud_drugs + stat_regline_equation(label.x = 200, label.y = 500)
  
  successful_fraud_drugs + theme_bw() + 
    labs(title = "Reported Successful drug convictions as a function of successful fraud convictions", x = "Successful Drugs Convictions", y = "Successful Fraud Convictions")
}

data_predictions <- function(df_fraud_drug){
  set.seed(123)
  
  # Generating a vector of indices for the training data
  train_index <- sample(nrow(df_fraud_drug), floor(0.7 * nrow(df_fraud_drug)))
  
  # creating training and test datasets
  train_data <- df_fraud_drug[train_index, ]
  test_data <- df_fraud_drug[-train_index, ]
  
  model <- lm(successful_drugs ~ successful_fraud, data = train_data)
  
  # Predict the 'successful_drugs' values for the test data using the trained model
  predictions <- predict(model, newdata = test_data, type= "response")
  
  mse <- mean((predictions - test_data$successful_drugs)^2)
  print(mse)
  summary(model)
}


linear_regression_analysis(merged_df)

data_predictions(merged_df[c('successful_fraud', 'successful_drugs')])

# total number of successful_drugs and successful_fraud per year

calculate_total_drugs_frauds <- function(merged_df){
  totals_drugs <- merged_df %>%
    group_by(year) %>%
    summarize(total_successful_drugs = sum(successful_drugs))
  
  # total number of successful_drugs per year
  totals_fraud <- merged_df %>%
    group_by(year) %>%
    summarize(total_successful_fraud = sum(successful_fraud))
  
  
  merged_data_fraud_drugs <- merge(totals_drugs, totals_fraud, by = "year")
  
  merged_data_fraud_drugs <- tidyr::pivot_longer(merged_data_fraud_drugs, cols=c('total_successful_drugs', 'total_successful_fraud'), names_to='Result', 
                             values_to="Count")
  
  ggplot(merged_data_fraud_drugs, aes(x=year, y=Count, fill=Result)) +
    geom_bar(stat='identity', position='dodge')
  

}

calculate_total_drugs_frauds(merged_df)



# Hypothesis 2: The number of burglary convictions is higher in urban regions compared to rural regions

divide_as_regions <- function(merged_df) {
  rural_drugs_fruad <- merged_df[merged_df$region == "Rural", ]
  urban_drugs_fraud <- merged_df[merged_df$region == "Urban", ]
  
  return(list(rural_drugs_fruad,urban_drugs_fraud))
}

get_means_regions <- function(rural_df, urban_df){
  
  mean_rural_drugs <- mean(rural_df$successful_drugs)
  mean_rural_frauds <- mean(rural_df$successful_fraud)
  mean_urban_drugs <- mean(urban_df$successful_drugs)
  mean_urban_frauds <- mean(urban_df$successful_fraud)
  
  # Perform statistical test (e.g., t-test) to determine if the means are significantly different
  # You can use a suitable statistical test based on your assumptions and data
  
  # Print the means for comparison
  cat("Mean rural Drug convictions (Rural):", mean_rural_drugs, "\n")
  cat("Mean rural Fraud convictions (Rural):", mean_rural_frauds, "\n")
  cat("Mean urban drug convictions (Urban):", mean_urban_drugs, "\n")
  cat("Mean urban Fraud convictions (Urban):", mean_urban_frauds, "\n")
}

region_df <- divide_as_regions(merged_df)
get_means_regions(region_df[[1]], region_df[[2]])



######## CLUSTERING ################

data_clustering <- function(merged_df) {
  # the variables of interest for clustering
  cluster_data <- merged_df[, c("successful_fraud", "successful_drugs")]
  
  
  k <- 3  # the number of clusters
  kmeans_model <- kmeans(cluster_data, centers = k)
  
  # cluster assignments for each data point
  cluster_assignments <- kmeans_model$cluster
  
  # cluster assignments to the original dataset
  merged_df$cluster <- cluster_assignments
  
  
  #  plot with color-coded clusters
  ggplot(merged_df, aes(x = successful_drugs, y = successful_fraud, color = factor(cluster))) +
    geom_point() +
    labs(x = "Successful Drugs", y = "Successful Fraud", color = "Cluster") +
    scale_color_discrete(name = "Cluster") +
    theme_minimal()
  
}

data_clustering(merged_df)


data_classification <- function(merged_df) {
  # variables of interest for classification
  classification_data <- merged_df[c("successful_fraud", "successful_drugs")]
  
  # Preparing the target variable
  target_variable <- merged_df$region
  
  # dividing the data into training and testing sets
  set.seed(123)  # Setting a seed for reproducibility
  train_indices <- sample(nrow(classification_data), 0.7 * nrow(classification_data))  # 70% for training
  train_data <- classification_data[train_indices, ]
  train_target <- target_variable[train_indices]
  test_data <- classification_data[-train_indices, ]
  test_target <- target_variable[-train_indices]
  
  
  multinom_model <- multinom(train_target ~ ., data = train_data)
  
  # predictions on the test data
  predictions <- predict(multinom_model, newdata = test_data, type = "class")
  
  # model performamce evaluation
  accuracy <- sum(predictions == test_target) / length(test_target)
  print(paste("Accuracy:", accuracy))
}

data_classification(merged_df)



