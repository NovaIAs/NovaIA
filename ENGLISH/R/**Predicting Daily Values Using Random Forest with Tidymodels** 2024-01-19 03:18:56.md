```r
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(tidymodels)

# Simulate some data
set.seed(123)
df <- tibble(
  id = 1:100,
  date = seq(as_date("2020-01-01"), as_date("2020-12-31"), by = "day"),
  value = rnorm(100 * 365, mean = 0, sd = 1)
)

# Create a tidymodels recipe
recipe <- recipe(value ~ ., data = df) %>%
  step_date(date, label = "day") %>%
  step_scale(value, to_range = c(0, 1))

# Split the data into training and test sets
training <- initial_split(df, prop = 0.75)
testing <- training %>% training_set() %>% testing_set()

# Create a random forest model
model <- rand_forest(value ~ ., data = training, recipe = recipe)

# Train the model
trained_model <- model %>% fit(resamples = training)

# Evaluate the model on the test set
model_results <- trained_model %>% predict(new_data = testing) %>%
  bind_cols(testing) %>%
  rmse(truth = value, estimate = .pred)

# Print the model results
print(model_results)

# Visualize the model predictions
ggplot(model_results, aes(x = date, y = value, color = .pred)) +
  geom_line() +
  geom_point(alpha = 0.2) +
  labs(title = "Actual vs. Predicted Values",
       x = "Date",
       y = "Value")
```

Explanation:
- A tidymodels recipe is created to preprocess the data, including date transformation and value scaling.
- The data is split into training and test sets using the initial_split() function.
- A random forest model is specified using the rand_forest() function with the recipe.
- The model is trained using the training data with the fit() function.
- The trained model is used to make predictions on the test set using the predict() function.
- The predicted values are bound with the actual values in the test set and evaluated using the rmse() function to calculate the root mean squared error (RMSE).
- The model results are printed to the console.
- A ggplot2 plot visualizes the actual and predicted values along with the dates.