Define your data
```
# Import the data
data <- read.csv("data.csv")

# Create a new variable that is the product of two existing variables
data$new_variable <- data$variable1 * data$variable2

# Create a subset of the data
subset_data <- data[data$variable3 > 100, ]
```
Create your model
```
# Fit a linear regression model
model <- lm(new_variable ~ variable4 + variable5, data = subset_data)

# Calculate the predicted values
predicted_values <- predict(model, newdata = subset_data)

# Calculate the residuals
residuals <- subset_data$new_variable - predicted_values
```
Evaluate your model
```
# Calculate the root mean squared error
rmse <- sqrt(mean((residuals)^2))

# Calculate the adjusted R-squared value
adjusted_r_squared <- 1 - (1 - r_squared) * ((nrow(subset_data) - 1) / (nrow(subset_data) - 2))

# Print the model summary
summary(model)
```
Visualize your results
```
# Create a scatterplot of the predicted values vs. the actual values
plot(predicted_values, subset_data$new_variable)
abline(0, 1)

# Create a histogram of the residuals
hist(residuals)
```
Save your results
```
# Save the model
save(model, file = "model.rds")

# Save the predicted values
save(predicted_values, file = "predicted_values.rds")

# Save the residuals
save(residuals, file = "residuals.rds")
```