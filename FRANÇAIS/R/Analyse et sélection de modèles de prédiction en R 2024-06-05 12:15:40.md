```r
# Chargement des données
donnees <- read.csv("donnees.csv")

# Prétraitement des données
donnees$variable_numerique <- as.numeric(donnees$variable_numerique)
donnees$variable_categorique <- as.factor(donnees$variable_categorique)

# Création d'un modèle de régression linéaire
modele <- lm(variable_numerique ~ ., data = donnees)

# Évaluation du modèle
print(summary(modele))

# Calcul de l'AIC du modèle
aic <- AIC(modele)

# Création d'un modèle de forêt aléatoire
modele_rf <- randomForest(variable_numerique ~ ., data = donnees)

# Évaluation du modèle
print(summary(modele_rf))

# Calcul de l'AIC du modèle
aic_rf <- AIC(modele_rf)

# Sélection du meilleur modèle
if (aic < aic_rf) {
  meilleur_modele <- modele
} else {
  meilleur_modele <- modele_rf
}

# Prédictions avec le meilleur modèle
predictions <- predict(meilleur_modele, newdata = donnees)

# Visualisation des prédictions
plot(donnees$variable_numerique, predictions)
```

**Explication du code :**

Ce code effectue une analyse de régression en R. Il charge les données à partir d'un fichier CSV, effectue un prétraitement des données pour les convertir aux types de données appropriés. Ensuite, il crée un modèle de régression linéaire et un modèle de forêt aléatoire, et évalue leurs performances à l'aide de l'AIC (critère d'information d'Akaike). En fonction de l'AIC, il sélectionne le meilleur modèle et effectue des prédictions sur les données. Enfin, il visualise les prédictions sous forme de graphique.