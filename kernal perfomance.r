# Load the necessary R libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(reshape2)

sgemm_df <- read.csv('C:/Users/SANJANA/OneDrive/Desktop/sgemm_product.csv')

# First look at the data
head(sgemm_df)

# Summary of the data frame structure
str(sgemm_df)

# Summary statistics of the data frame
summary(sgemm_df)

# Define the dependent variables
dependent_var <- c('Run1..ms.', 'Run2..ms.', 'Run3..ms.', 'Run4..ms.')

# Plot histograms with density plots for each dependent variable
for (depend in dependent_var) {
  p <- ggplot(sgemm_df, aes(x = !!sym(depend))) +
    geom_histogram(bins = 30, fill = "green", color = "black") +
    geom_density(alpha = 0.3, fill = "green") +
    theme_minimal() +
    labs(title = paste("Distribution of", depend), x = depend, y = "Density")
  print(p)
}

# Plot boxplots for each dependent variable
for (depend in dependent_var) {
  p <- ggplot(sgemm_df, aes(x = factor(1), y = !!sym(depend))) +
    geom_boxplot(fill = 'lightblue', color = 'black') +
    labs(title = paste("Boxplot of", depend), x = NULL, y = depend) +
    theme_minimal()
  print(p)
}


# Compute and melt the absolute correlation matrix
cor_matrix_abs <- abs(cor(sgemm_df[dependent_var]))
melted_cor_matrix <- melt(cor_matrix_abs)

# Plot heatmap
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  theme_minimal() +
  labs(title = "Heatmap of Absolute Correlations", x = NULL, y = NULL)

# Define the dependent variables
dependent_var <- c('Run1 (ms)', 'Run2 (ms)', 'Run3 (ms)', 'Run4 (ms)')

# Identify columns that are not in dependent_var
non_dependent_vars <- setdiff(names(sgemm_df), dependent_var)

# Plot count plots for non-dependent variables
for (column in non_dependent_vars) {
  if (is.factor(sgemm_df[[column]]) || is.character(sgemm_df[[column]])) {
    p <- ggplot(sgemm_df, aes_string(x = column)) +
      geom_bar(fill = 'skyblue', color = 'black') +
      labs(title = paste("Count Plot of", column), x = column, y = "Count") +
      theme_minimal()
    
    print(p)
  }
}

# Define the dependent variables
dependent_var <- c('Run1 (ms)', 'Run2 (ms)', 'Run3 (ms)', 'Run4 (ms)')

# Identify columns that are not in dependent_var
non_dependent_vars <- setdiff(names(sgemm_df), dependent_var)

# Compute and melt the absolute correlation matrix for non-dependent variables
cor_matrix_abs <- abs(cor(sgemm_df[non_dependent_vars], use = "complete.obs"))
melted_cor_matrix <- melt(round(cor_matrix_abs, 3))

# Plot heatmap
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.3f", value)), vjust = 1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5) +
  theme_minimal() +
  labs(title = "Heatmap of Absolute Correlations (Non-Dependent Variables)", x = NULL, y = NULL)

# Define the dependent variables
dependent_var <- c('Run1 (ms)', 'Run2 (ms)', 'Run3 (ms)', 'Run4 (ms)')

# Drop columns listed in dependent_var
X <- sgemm_df[ , !(names(sgemm_df) %in% dependent_var)]

# Select the target variable
y <- sgemm_df[['Run2..ms.']]


# Plot histogram with density plot
ggplot(data = data.frame(y = y), aes(x = y)) +
  geom_histogram(aes(y = ..density..), fill = 'red', color = 'black', bins = 30) +
  geom_density(alpha = 0.3, fill = 'red') +
  theme_minimal() +
  labs(title = "Histogram with Density Plot of Target Variable", x = "Run2 (ms)", y = "Density")


# Apply log transformation
log_y <- log(y)

# Plot histogram with density plot of log-transformed target variable
ggplot(data = data.frame(log_y = log_y), aes(x = log_y)) +
  geom_histogram(aes(y = ..density..), fill = 'red', color = 'black', bins = 30) +
  geom_density(alpha = 0.3, fill = 'red') +
  theme_minimal() +
  labs(title = "Histogram with Density Plot of Log-Transformed Target Variable", x = "Log(Run2 (ms))", y = "Density")

# Apply log transformation
y_final <- log(y)

# Import libraries
library(caret)  # For train/test split
library(MASS)   # For linear regression

# Set seed for reproducibility
set.seed(0)

# Create a partition for the training and test sets
splitIndex <- createDataPartition(y_final, p = 0.75, list = FALSE)

# Split the data
X_train <- X[splitIndex, ]
X_test <- X[-splitIndex, ]
y_train <- y_final[splitIndex]
y_test <- y_final[-splitIndex]

# Get dimensions of the training and test sets
dim(X_train)  # Returns the number of rows and columns in X_train
dim(X_test)   # Returns the number of rows and columns in X_test

# Get lengths of the training and test target variables
length(y_train)  # Returns the number of elements in y_train
length(y_test)   # Returns the number of elements in y_test

# Fit a linear regression model
model <- lm(y_train ~ ., data = X_train)

# Get the intercept of the linear regression model
intercept <- coef(model)[1]

# Get all coefficients, including the intercept
all_coefficients <- coef(model)

# Exclude the intercept to get only the coefficients
coefficients <- all_coefficients[-1]  # Remove the intercept

# Make predictions on the training set
y_pred_train <- predict(model, newdata = X_train)

# Make predictions on the test set
y_pred <- predict(model, newdata = X_test)

# Calculate R-squared
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - y_pred)^2)
r2 <- 1 - (ss_residual / ss_total)

#Alternate way of calculating of R-squared
#Install the rsq package if you haven't already
#install.packages("rsq")

# Load the package
# library(rsq)

# Calculate R-squared
# r2 <- rsq(model, newdata = X_test, y = y_test)


# Calculate Mean Absolute Error
mae <- mean(abs(y_test - y_pred))

# Calculate Mean Squared Error
mse <- mean((y_test - y_pred)^2)

# Calculate Root Mean Squared Error
rmse <- sqrt(mse)

# Calculate the number of observations and predictors
n <- nrow(X_test)
p <- ncol(X_test)

# Calculate Adjusted R-squared
adj_r2 <- 1 - (1 - r2) * ((n - 1) / (n - p - 1))

# Print Adjusted R-squared
print(paste("Adjusted R2:", adj_r2))

# Create a data frame for plotting
plot_data <- data.frame(Actual = y_test, Predicted = y_pred)


# Scatter plot of actual vs predicted values
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue', size = 2) +              # Data points
  geom_abline(slope = 1, intercept = 0, color = 'red') + # Regression line (45-degree line)
  labs(x = 'Actual', y = 'Predicted', title = 'Linear Regression: Actual vs Predicted') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +     # Center title
  scale_x_continuous(limits = c(min(y_test), max(y_test))) +
  scale_y_continuous(limits = c(min(y_test), max(y_test)))


# Create a data frame for plotting
plot_data <- data.frame(Index = seq_along(y_test), Actual = y_test, Predicted = y_pred)

# Plot the predicted vs actual values
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = Actual, color = 'Actual')) +
  geom_line(aes(y = Predicted, color = 'Predicted')) +
  labs(x = 'Number of Test Data', y = 'Value', title = 'Predicted vs. Actual Values') +
  scale_color_manual(values = c('Actual' = 'blue', 'Predicted' = 'red')) +
  theme_minimal() +
  theme(legend.title = element_blank())