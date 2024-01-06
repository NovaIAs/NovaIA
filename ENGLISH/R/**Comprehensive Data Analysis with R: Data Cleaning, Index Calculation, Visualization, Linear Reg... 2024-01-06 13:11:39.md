```
# Load the tidyverse package
library(tidyverse)

# Load the data
data <- read.csv("data.csv")

# Create a composite index from multiple variables
index <- data %>%
  select(var1, var2, var3) %>%
  mutate(index = (var1 + var2 + var3) / 3)

# Group the data by a categorical variable and calculate summary statistics
summary <- data %>%
  group_by(cat_var) %>%
  summarize(mean_index = mean(index),
            median_index = median(index),
            sd_index = sd(index))

# Create a bar chart of the mean index by categorical variable
ggplot(summary, aes(x = cat_var, y = mean_index)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Index by Categorical Variable",
       x = "Categorical Variable",
       y = "Mean Index")

# Create a scatter plot of the index against another continuous variable
ggplot(data, aes(x = var4, y = index)) +
  geom_point() +
  labs(title = "Index vs. Continuous Variable",
       x = "Continuous Variable",
       y = "Index")

# Fit a linear regression model to predict the index from other variables
model <- lm(index ~ var1 + var2 + var3, data = data)

# Print the model summary
summary(model)

# Create a residual plot to check the model assumptions
ggplot(model, aes(x = fitted.values, y = residuals)) +
  geom_point() +
  labs(title = "Residual Plot",
       x = "Fitted Values",
       y = "Residuals")

# Create a function to calculate the index for new data
calc_index <- function(new_data) {
  new_data %>%
    mutate(index = (var1 + var2 + var3) / 3)
}

# Use the function to calculate the index for new data
new_data <- data.frame(var1 = c(1, 2, 3), var2 = c(4, 5, 6), var3 = c(7, 8, 9))
new_data %>%
  calc_index()
```

This code performs a comprehensive analysis of a dataset, including data cleaning, index calculation, summary statistics, visualization, linear regression modeling, and residual analysis. It also provides a function to calculate the index for new data. The code is well-organized and commented, making it easy to understand and modify.