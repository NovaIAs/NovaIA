**Code complexe en R :**

```r
# Chargement des bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(tidyverse)

# Création d'un ensemble de données
donnees <- tibble(
  id = seq(1, 100),
  nom = sample(c("Alice", "Bob", "Charlie", "Diana", "Edward"), 100, replace = TRUE),
  age = sample(20:60, 100, replace = TRUE),
  salaire = sample(c(1000, 2000, 3000, 4000, 5000), 100, replace = TRUE)
)

# Nettoyage et transformation des données
donnees_propres <- donnees %>%
  # Supprimer les doublons
  distinct() %>%
  # Convertir la colonne "salaire" en facteur
  mutate(salaire = as.factor(salaire)) %>%
  # Créer une nouvelle colonne "tranche_age"
  mutate(tranche_age = case_when(
    age < 30 ~ "20-29 ans",
    age >= 30 & age < 40 ~ "30-39 ans",
    age >= 40 & age < 50 ~ "40-49 ans",
    age >= 50 & age < 60 ~ "50-59 ans",
    TRUE ~ "60 ans ou plus"
  ))

# Exploration des données
ggplot(donnees_propres, aes(x = nom, y = salaire)) +
  geom_bar(stat = "count") +
  labs(title = "Nombre d'employés par nom",
       x = "Nom",
       y = "Nombre d'employés")

ggplot(donnees_propres, aes(x = tranche_age, y = salaire)) +
  geom_boxplot() +
  labs(title = "Répartition des salaires par tranche d'âge",
       x = "Tranche d'âge",
       y = "Salaire")

# Analyse statistique
test_t <- t.test(salaire ~ nom, data = donnees_propres)
print(test_t)

test_anova <- aov(salaire ~ nom, data = donnees_propres)
print(test_anova)

# Modélisation prédictive
modele_lm <- lm(salaire ~ nom, data = donnees_propres)
print(modele_lm)

# Évaluation du modèle
pred_lm <- predict(modele_lm, newdata = donnees_propres)
rmse_lm <- sqrt(mean((pred_lm - donnees_propres$salaire)^2))
print(rmse_lm)
```

**Explication du code :**

* Le code importe les bibliothèques nécessaires pour la manipulation et l'analyse des données.
* Il crée un ensemble de données avec des informations sur les employés, notamment leur nom, leur âge et leur salaire.
* Les données sont ensuite nettoyées et transformées pour supprimer les doublons, convertir le salaire en facteur et créer une colonne de tranches d'âge.
* Le code explore les données en créant des graphiques à barres et à boîtes pour visualiser la distribution des noms et des salaires.
* Il effectue des analyses statistiques, notamment un test t pour comparer les salaires en fonction des noms et une analyse de variance (ANOVA) pour évaluer l'effet du nom sur les salaires.
* Enfin, il crée un modèle de régression linéaire pour prédire les salaires en fonction des noms et évalue les performances du modèle en calculant l'erreur quadratique moyenne (RMSE).