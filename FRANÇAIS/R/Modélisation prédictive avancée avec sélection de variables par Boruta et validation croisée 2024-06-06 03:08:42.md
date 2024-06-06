**Fonction de modélisation prédictive complexe avec sélection de variables et validation croisée**

```r
# Chargement des bibliothèques nécessaires
library(tidyverse)
library(caret)
library(rsample)

# Chargement des données
data <- read.csv("data.csv")

# Prétraitement des données
data <- data %>%
  mutate(species = as.factor(species)) %>%
  scale()

# Création d'un jeu de données de test et d'entraînement
set.seed(123)
train_index <- initial_split(species ~ ., data, prop = 0.75)
train <- training(train_index)
test <- testing(train_index)

# Sélection des variables à l'aide de l'algorithme Boruta
control <- borutaControl(maxRuns = 100)
boruta_result <- boruta(species ~ ., data = data[train, ], control = control)

# Sélection des variables importantes
selected_variables <- colnames(data)[boruta_result$finalDecision == "Retained"]

# Création d'un modèle de forêt aléatoire
model <- randomForest(species ~ ., data = data[train, selected_variables], ntree = 500, importance = TRUE)

# Validation croisée du modèle
cv_control <- trainControl(method = "cv", number = 10)
cv_results <- train(species ~ ., data = data[train, selected_variables], method = "rf", trControl = cv_control)

# Évaluation des performances du modèle
accuracy <- mean(cv_results$resample %>% pull(Accuracy))
print(accuracy)

# Prédiction sur le jeu de données de test
predictions <- predict(model, data[test, selected_variables])

# Évaluation des performances du modèle sur le jeu de données de test
accuracy_test <- mean(predictions == data$species[test])
print(accuracy_test)
```

**Explication du code:**

* Le prétraitement des données comprend la création d'une variable de facteur pour la variable **espèce**, la mise à l'échelle des données et la séparation des données en ensembles d'entraînement et de test.
* La sélection des variables à l'aide de Boruta permet d'identifier les variables les plus importantes pour la prédiction.
* Un modèle de forêt aléatoire est ensuite créé en utilisant les variables sélectionnées.
* La validation croisée est utilisée pour évaluer les performances du modèle sur plusieurs sous-ensembles de données d'entraînement.
* Les performances du modèle sont évaluées en termes de précision sur le jeu de données d'entraînement et le jeu de données de test.