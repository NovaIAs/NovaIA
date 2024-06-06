**Code Python complexe et avancé**

```python
# Importation des bibliothèques nécessaires
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Chargement et préparation des données
données = pd.read_csv("donnees.csv")
données["date"] = pd.to_datetime(données["date"])
données["année"] = données["date"].dt.year

# Division des données en train et test
X_train, X_test, y_train, y_test = train_test_split(données[["année"]], données["prix"], test_size=0.2)

# Création et entraînement du modèle de régression linéaire
modèle = LinearRegression()
modèle.fit(X_train, y_train)

# Prédiction des prix pour les données de test
y_pred = modèle.predict(X_test)

# Évaluation du modèle
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print("Erreur quadratique moyenne : ", mse)
print("Coefficient de détermination : ", r2)

# Visualisation des résultats
plt.scatter(X_test["année"], y_test, color="blue", label="Données réelles")
plt.plot(X_test["année"], y_pred, color="red", label="Prédictions")
plt.xlabel("Année")
plt.ylabel("Prix")
plt.legend()
plt.show()

# Création d'une fonction pour prédire les prix de l'année en cours et des années suivantes
def prédire_prix(annees):
    """
    Prédit les prix pour les années spécifiées.

    Args:
        années (list): Liste d'années pour lesquelles prédire les prix.

    Returns:
        list: Liste des prix prédits.
    """
    X_nouvelles = pd.DataFrame({"année": annees})
    return modèle.predict(X_nouvelles)

# Prédiction des prix pour les 5 prochaines années
annees_futur = list(range(données["année"].max() + 1, données["année"].max() + 6))
prix_futurs = prédire_prix(annees_futur)

# Affichage des prédictions
print("Prix prédits pour les 5 prochaines années : ", prix_futurs)
```

**Explication du code**

Ce code Python effectue :

* Le chargement, la préparation et la division des données en ensembles d'entraînement et de test.
* La création et l'entraînement d'un modèle de régression linéaire pour prédire les prix en fonction de l'année.
* L'évaluation du modèle sur l'ensemble de test à l'aide des métriques erreur quadratique moyenne (MSE) et coefficient de détermination (R2).
* La visualisation des résultats réels et prédits sur un graphique.
* La création d'une fonction pour prédire les prix pour les années futures.
* L'utilisation de la fonction pour prédire les prix pour les 5 prochaines années et l'affichage des résultats.