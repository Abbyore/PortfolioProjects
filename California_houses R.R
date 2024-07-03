# Load the necessary packages
library(ggplot2)
library(randomForest)
library(caret)
library(psych)
library(corrplot)
library(gridExtra)
library(dplyr)
library(scales)
library(DataExplorer)
library(psych)
library(sqldf)
library(formattable)
library(leaps)
library(pROC)
library(RColorBrewer)
library(car)
library(grid)
library(tidyverse)
library(Metrics)
library(glmnet)
library(readr)
library(MASS)

#import the dataset
California_Houses <- read_csv("~/Desktop/California_Houses.csv")

#change the dataset name to housing data
housing_data <- California_Houses

#view the housing dataset
View(housing_data)

# View the structure of the data
str(housing_data)

# View the summary statistics of the data
summary(housing_data)

# Check for missing values
sum(is.na(housing_data))

#convert the median income variable to round figures 
housing_data$Median_Income <- round(housing_data$Median_Income)

# Convert the housing data table dataframe to a regular dataframe
housing_df <- as.data.frame(housing_data)

# View the structure of the data
str(housing_data)

# View the summary statistics of the data
summary(housing_data)

# Plot histogram with outline color and whole number x-axis values
ggplot(housing_df, aes(x = Median_House_Value)) +
  geom_histogram(fill = "blue", color = "black", bins = 20) +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 800000, by = 100000)) +
  labs(title = "Histogram of Median House Value", x = "Median House Value", y = "Count") +
  theme_bw()



# Plot histogram with outline color and whole number x-axis values
ggplot(housing_df, aes(x = `Median_Age`)) +
  geom_histogram(fill = "yellow", color = "black", bins = 10) +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 800000, by = 100000)) +
  labs(title = "Histogram of Median Age", x = "Median Age", y = "Count") +
  theme_bw()



ggplot(housing_df, aes(x = Median_Income, y = Median_House_Value)) +
  geom_point(color = "steelblue", alpha = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Scatter Plot of Median Income vs. Median House Value",
       x = "Median Income",
       y = "Median House Value") +
  theme_bw()

ggplot(housing_df, aes(x = Median_Income, y = Median_House_Value)) +
  geom_point(color = "black", alpha = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Median Income vs. Median House Value",
       x = "Median Income",
       y = "Median House Value") +
  theme_bw() 


ggplot(housing_df, aes(x = Median_Age, y = Median_House_Value)) +
  geom_point(color = "black", alpha = 0.05) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Median Income vs. Median House Value",
       x = "Median Income",
       y = "Median House Value") +
  theme_bw() 

# Running a PCA approach to perform a feature reduction
# Standardizing the data
mthousing_std <- scale(housing_data)
pca <- prcomp(mthousing_std)

# Deciding on the best number of components based on the PCA visualization
# Creating a scree plot
plot(pca, type="lines")

# Load the necessary libraries
library(caret)

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(housing_data$Median_House_Value, p = 0.7, list = FALSE)
train_data <- housing_data[train_indices, ]
test_data <- housing_data[-train_indices, ]

# Train a linear regression model
linear_model <- train(
  Median_House_Value ~ .,
  data = train_data,
  method = "lm"
)

# Train a ridge regression model
ridge_model <- train(
  Median_House_Value ~ .,
  data = train_data,
  method = "ridge"
)

# Train a decision tree regression model
tree_model <- train(
  Median_House_Value ~ .,
  data = train_data,
  method = "rpart"
)


#Predict with the linear regression model
linear_pred <- predict(linear_model, newdata = test_data)
# View the predicted values
head(linear_pred)
# Predict with the ridge regression model
ridge_pred <- predict(ridge_model, newdata = test_data)
# View the predicted values
head(ridge_pred)
# Predict with the decision tree regression model
tree_pred <- predict(tree_model, newdata = test_data)
head(tree_pred)

# Calculate the evaluation metrics
####linear metrics
linear_metrics <- postResample(pred = linear_pred, obs = test_data$Median_House_Value)
# Print the evaluation metrics
linear_metrics

#####ridge metrics
ridge_metrics <- list()
ridge_metrics$RMSE <- sqrt(mean((test_data$Median_House_Value - ridge_pred)^2))
ridge_metrics$R_squared <- cor(test_data$Median_House_Value, ridge_pred)^2
ridge_metrics$MAE <- mean(abs(test_data$Median_House_Value - ridge_pred))

# Print the evaluation metrics
ridge_metrics

# Compare the evaluation metrics
linear_metrics
ridge_metrics



# Create a data frame for the evaluation metrics
metrics <- data.frame(
  Model = c("Ridge Regression", "Linear Regression"),
  RMSE = c(ridge_metrics$RMSE, linear_metrics[1]),
  R_squared = c(ridge_metrics$R_squared, linear_metrics[2]),
  MAE = c(ridge_metrics$MAE, linear_metrics[3])
)

# Reshape the data frame to long format
metrics_long <- tidyr::gather(metrics, Metric, Value, -Model)


# Create the graph plot
ggplot(data = metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Model", y = "Value") +
  ggtitle("Comparison of Evaluation Metrics") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +
  theme_minimal() +
  geom_text(aes(label = round(Value, 4)), position = position_dodge(width = 0.9), vjust = -0.5)


