```r
# This complex R code performs a comprehensive analysis of the Titanic disaster dataset, including data cleaning, exploratory analysis, and predictive modeling.

# Load the required libraries
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)
library(tidyverse)
library(randomForest)
library(Boruta)

# Load and clean the data
titanic_data <- read.csv("titanic.csv")
titanic_data <- titanic_data %>%
  # Drop the 'PassengerId' column as it is not necessary for the analysis
  select(-PassengerId) %>%
  # Convert the 'Sex' column to a numeric factor
  mutate(Sex = factor(Sex, levels = c("male", "female"))) %>%
  # Convert the 'Age' column to a numeric variable
  mutate(Age = as.numeric(Age)) %>%
  # Convert the 'Embarked' column to a factor variable
  mutate(Embarked = factor(Embarked))

# # Exploratory data analysis
# Summarize the data
summary(titanic_data)

# Examine the distribution of the 'Sex' variable
ggplot(titanic_data, aes(x = Sex, fill = Sex)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Sex on the Titanic")

# Examine the distribution of the 'Survived' variable
ggplot(titanic_data, aes(x = Survived, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Survival on the Titanic")

# Examine the distribution of the 'Age' variable
ggplot(titanic_data, aes(x = Age)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribution of Age on the Titanic")

# Examine the relationship between 'Sex' and 'Survived'
ggplot(titanic_data, aes(x = Sex, y = Survived)) +
  geom_boxplot() +
  labs(title = "Relationship between Sex and Survival on the Titanic")

# Examine the relationship between 'Age' and 'Survived'
ggplot(titanic_data, aes(x = Age, y = Survived)) +
  geom_boxplot() +
  labs(title = "Relationship between Age and Survival on the Titanic")

# Examine the relationship between 'Embarked' and 'Survived'
ggplot(titanic_data, aes(x = Embarked, y = Survived)) +
  geom_boxplot() +
  labs(title = "Relationship between Embarked and Survival on the Titanic")

# # Predictive modeling
# Split the data into a training set and a test set
set.seed(123)
train_index <- createDataPartition(titanic_data$Survived, p = 0.75, list = FALSE)
train <- titanic_data[train_index, ]
test <- titanic_data[-train_index, ]

# Select the features to use for modeling
features <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")

# Create a random forest model
model <- randomForest(Survived ~ ., data = train[features], ntree = 500)

# Evaluate the model's performance on the training set
pred_train <- predict(model, train[features])
accuracy_train <- mean(pred_train == train$Survived)
print(paste("Accuracy on the training set:", accuracy_train))

# Evaluate the model's performance on the test set
pred_test <- predict(model, test[features])
accuracy_test <- mean(pred_test == test$Survived)
print(paste("Accuracy on the test set:", accuracy_test))

# Feature selection using Boruta
boruta <- Boruta(Survived ~ ., data = train[features], maxRuns = 100)
selected_features <- boruta$finalModel$subsets

# Create a random forest model with the selected features
model <- randomForest(Survived ~ ., data = train[selected_features], ntree = 500)

# Evaluate the model's performance on the training set
pred_train <- predict(model, train[selected_features])
accuracy_train <- mean(pred_train == train$Survived)
print(paste("Accuracy on the training set:", accuracy_train))

# Evaluate the model's performance on the test set
pred_test <- predict(model, test[selected_features])
accuracy_test <- mean(pred_test == test$Survived)
print(paste("Accuracy on the test set:", accuracy_test))
```

This code performs a comprehensive analysis of the Titanic disaster dataset, including data cleaning, exploratory analysis, and predictive modeling. It uses various R libraries for data manipulation, visualization, and modeling.

1. **Data Cleaning:**
   - The code starts by loading the required libraries.
   - The Titanic dataset is loaded and cleaned by removing unnecessary columns, converting categorical variables to factors, and converting the 'Age' column to a numeric variable.

2. **Exploratory Data Analysis:**
   - The code summarizes the data and plots various graphs to explore the distribution of the different variables and their relationship with the 'Survived' variable.

3. **Predictive Modeling:**
   - The code splits the data into training and test sets.
   - A random forest model is created using the training set and evaluated on both the training and test sets.
   - The accuracy of the model is calculated and printed.

4. **Feature Selection:**
   - Boruta is used to select the most important features for the predictive model.
   - A new random forest model is created using the selected features and evaluated on both the training and test sets.
   - The accuracy of the model is calculated and printed.