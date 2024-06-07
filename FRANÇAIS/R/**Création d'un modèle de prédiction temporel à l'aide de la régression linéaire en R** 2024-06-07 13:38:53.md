```r
## Chargement des bibliothèques nécessaires
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidymodels)
library(rsample)
library(caret)

## Chargement des données
donnees <- read.csv("donnees.csv")

## Préparation des données
donnees <- donnees %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(valeur)) %>%
  select(date, valeur)

## Création des caractéristiques temporelles
donnees_tempo <- donnees %>%
  mutate(jour_semaine = wday(date),
         mois = month(date),
         trimestre = quarter(date),
         annee = year(date))

## Création du modèle de prédiction
modele <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

## Entrainement du modèle
modele_fit <- modele %>%
  fit(valeur ~ ., data = donnees_tempo)

## Évaluation du modèle
resultats_entrainement <- predict(modele_fit, donnees_tempo)
print(rmse(resultats_entrainement, donnees_tempo$valeur))

## Validation croisée
resultats_validation <- vfold_cv(modele, donnees_tempo, v = 10, outcome = valeur)
print(rmse(resultats_validation))

## Prédiction sur de nouvelles données
donnees_nouvelles <- read.csv("donnees_nouvelles.csv")
donnees_nouvelles <- donnees_nouvelles %>%
  mutate(date = as.Date(date))

resultats_prediction <- predict(modele_fit, donnees_nouvelles)
```

**Explication du code :**

Ce code R exécute une analyse de régression linéaire pour prédire la valeur d'une variable continue (valeur) à partir d'un ensemble de caractéristiques temporelles (date, jour de la semaine, mois, trimestre, année).

1. Chargement des bibliothèques nécessaires pour manipuler les données, créer des modèles et évaluer les performances.

2. Chargement des données d'apprentissage depuis un fichier CSV.

3. Préparation des données en supprimant les lignes avec des valeurs manquantes, en convertissant la date en format Date et en sélectionnant uniquement les variables nécessaires (date et valeur).

4. Création des caractéristiques temporelles à partir de la date.

5. Création du modèle de prédiction en utilisant la régression linéaire avec l'engine lm.

6. Entrainement du modèle sur les données préparées.

7. Évaluation des performances du modèle sur les données d'entraînement en calculant l'erreur quadratique moyenne (RMSE).

8. Validation croisée du modèle pour estimer ses performances sur des données non vues.

9. Prédiction des valeurs pour de nouvelles données en utilisant le modèle entraîné.