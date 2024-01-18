```r
# This R code performs a comprehensive analysis of the Titanic dataset, including data cleaning, exploration, and modeling.

# Load the necessary libraries
library(tidyverse)
library(ggplot2)
library(caret)

# Read the Titanic dataset
titanic <- read.csv("titanic.csv")

# Clean the data
titanic <- titanic %>%
  drop_na() %>%
  mutate(Sex = factor(Sex),
         Embarked = factor(Embarked))

# Explore the data
ggplot(titanic, aes(x = Sex, y = Survived)) +
  geom_bar(stat = "count") +
  labs(title = "Survival Rates by Sex",
       x = "Sex",
       y = "Number of Passengers")

ggplot(titanic, aes(x = Pclass, y = Survived)) +
  geom_bar(stat = "count") +
  labs(title = "Survival Rates by Passenger Class",
       x = "Passenger Class",
       y = "Number of Passengers")

ggplot(titanic, aes(x = Age, y = Survived)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Age of Passengers",
       x = "Age",
       y = "Number of Passengers")

# Create a training and test set
train_index <- createDataPartition(titanic$Survived, p = 0.75, list = FALSE)
train <- titanic[train_index, ]
test <- titanic[-train_index, ]

# Train a random forest model
model <- train(Survived ~ Sex + Pclass + Age, data = train, method = "rf")

# Evaluate the model on the test set
pred <- predict(model, test)
accuracy <- mean(pred == test$Survived)
print(accuracy)

# Feature importance
importance <- varImp(model)
plot(importance, top = 10)

# ROC curve
roc <- roc(test$Survived, pred)
plot(roc, print.auc = TRUE)

# Calculate the confusion matrix
conf_mat <- confusionMatrix(test$Survived, pred)
print(conf_mat)
```

This code performs the following steps:

1. **Data Cleaning:** It removes any rows with missing values and converts the categorical variables `Sex` and `Embarked` into factors.
2. **Data Exploration:** It creates several plots to visualize the distribution of the variables and the relationship between them.
3. **Train-Test Split:** It randomly splits the data into a training set and a test set.
4. **Model Training:** It trains a random forest model using the training data to predict the survival of passengers.
5. **Model Evaluation:** It evaluates the performance of the model on the test set by calculating the accuracy.
6. **Feature Importance:** It calculates the importance of each feature in the model.
7. **ROC Curve:** It plots the ROC curve to assess the model's ability to distinguish between survivors and non-survivors.
8. **Confusion Matrix:** It generates a confusion matrix to visualize the model's predictions.

Overall, this code provides a comprehensive analysis of the Titanic dataset and demonstrates the use of various machine learning techniques for data modeling and evaluation.