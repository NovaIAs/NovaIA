**Création d'un modèle de forêt aléatoire avec optimisation hyperparamétrique dans R**

```r
# Charger les données
donnees <- read.csv("donnees.csv")

# Séparer les données en ensembles d'entraînement et de test
set.seed(123)
partition <- createDataPartition(y = donnees$y, p = 0,75, list = FALSE)
donnees_train <- donnees[partition,]
donnees_test <- donnees[-partition,]

# Définition de la fonction de réglage hyperparamétrique
reglage_hyperparam <- function(grid) {
  modele <- randomForest(y ~ ., data = donnees_train, mtry = grid$mtry, ntree = grid$ntree)
  erreur <- mean(abs(predict(modele, donnees_test$x) - donnees_test$y))

  return(erreur)
}

# Paramètres hyperparamétriques à optimiser
param_grid <- expand.grid(mtry = c(2, 4, 6), ntree = c(500, 1000, 1500))

# Optimisation hyperparamétrique
res_opt <- optim(par = param_grid, fn = reglage_hyperparam, control = list(method = "L-BFGS-B"))

# Affichage des hyperparamètres optimaux
print(res_opt$par)

# Création du modèle de forêt aléatoire optimisé
modele_opti <- randomForest(y ~ ., data = donnees_train, mtry = res_opt$par[["mtry"]], ntree = res_opt$par[["ntree"]])

# Évaluation des performances du modèle
erreur_test <- mean(abs(predict(modele_opti, donnees_test$x) - donnees_test$y))
print(erreur_test)

# Génération d'une matrice de confusion pour l'évaluation de la classification
mat_confusion <- table(pred = predict(modele_opti, donnees_test$x), vrai = donnees_test$y)
print(mat_confusion)

# Traçage de la courbe ROC pour l'évaluation de la classification
data_roc <- data.frame(
  vrai = donnees_test$y,
  pred = predict(modele_opti, donnees_test$x, type = "prob")[,1]
)
p <- ggplot(data_roc, aes(x = 1 - specifite, y = sensibilite)) +
  geom_area(fill = "steelblue", alpha = 0.5) +
  geom_line(color = "darkblue") +
  ggtitle("Courbe ROC") +
  xlab("1 - Spécificité") +
  ylab("Sensibilité")

print(p)
```

**Explication du code :**

1. **Chargement des données** : Le code charge un ensemble de données à partir d'un fichier CSV.

2. **Séparation des données** : Il divise les données en ensembles d'entraînement et de test.

3. **Définition de la fonction de réglage hyperparamétrique** : Cette fonction calcule l'erreur d'un modèle de forêt aléatoire pour une combinaison donnée d'hyperparamètres.

4. **Définition des paramètres hyperparamétriques à optimiser** : Le code définit une grille de valeurs pour les hyperparamètres mtry (nombre de variables à prendre en compte à chaque nœud) et ntree (nombre d'arbres).

5. **Optimisation hyperparamétrique** : Le code utilise la fonction `optim` pour optimiser les hyperparamètres en minimisant l'erreur sur l'ensemble d'entraînement.

6. **Création du modèle optimisé** : Il crée un modèle de forêt aléatoire avec les hyperparamètres optimaux.

7. **Évaluation des performances** : Le code évalue les performances du modèle sur l'ensemble de test en calculant l'erreur moyenne absolue.

8. **Création d'une matrice de confusion** : Il calcule une matrice de confusion pour évaluer les performances de classification du modèle.

9. **Traçage de la courbe ROC** : Le code crée une courbe ROC pour visualiser les performances de classification du modèle.