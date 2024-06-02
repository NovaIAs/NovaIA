```r
# Création d'un modèle de forêt aléatoire complexe pour la prédiction de valeurs de régression continue

# Importation des bibliothèques nécessaires
library(randomForest)

# Chargement des données d'entraînement
data <- read.csv("data.csv")

# Séparation des données en ensembles d'entraînement et de test
set.seed(123)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train <- data[train_index, ]
test <- data[-train_index, ]

# Création d'une grille de paramètres pour l'optimisation
param_grid <- expand.grid(
  mtry = c(2, 3, 4),
  ntree = c(500, 1000, 1500),
  nodesize = c(50, 100, 150)
)

# Optimisation des paramètres du modèle par validation croisée
best_params <- tuneRF(train$y ~ ., train, param_grid, ntree = 10, cv = 10)

# Création du modèle de forêt aléatoire avec les paramètres optimisés
model <- randomForest(train$y ~ ., train, ntree = 1000, mtry = 3, nodesize = 100)

# Évaluation des performances du modèle sur l'ensemble de test
pred <- predict(model, test)
rmse <- sqrt(mean((test$y - pred)^2))
print(paste("Erreur quadratique moyenne sur l'ensemble de test :", rmse))

# Calcul de l'importance des variables
importance <- importance(model)
plot(importance, top = 5)

# Enregistrement du modèle pour une utilisation ultérieure
saveRDS(model, "model.rds")
```

**Explications du code :**

* **Importation des bibliothèques nécessaires** :

```r
library(randomForest)
```

* **Chargement des données d'entraînement** :

```r
data <- read.csv("data.csv")
```

* **Séparation des données en ensembles d'entraînement et de test** :

```r
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train <- data[train_index, ]
test <- data[-train_index, ]
```

* **Création d'une grille de paramètres pour l'optimisation** :

```r
param_grid <- expand.grid(
  mtry = c(2, 3, 4),
  ntree = c(500, 1000, 1500),
  nodesize = c(50, 100, 150)
)
```

* **Optimisation des paramètres du modèle par validation croisée** :

```r
best_params <- tuneRF(train$y ~ ., train, param_grid, ntree = 10, cv = 10)
```

* **Création du modèle de forêt aléatoire avec les paramètres optimisés** :

```r
model <- randomForest(train$y ~ ., train, ntree = 1000, mtry = 3, nodesize = 100)
```

* **Évaluation des performances du modèle sur l'ensemble de test** :

```r
pred <- predict(model, test)
rmse <- sqrt(mean((test$y - pred)^2))
print(paste("Erreur quadratique moyenne sur l'ensemble de test :", rmse))
```

* **Calcul de l'importance des variables** :

```r
importance <- importance(model)
plot(importance, top = 5)
```

* **Enregistrement du modèle pour une utilisation ultérieure** :

```r
saveRDS(model, "model.rds")
```