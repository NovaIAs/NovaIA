```python
# Importation des bibliothèques nécessaires
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, classification_report

# Chargement des données
données = pd.read_csv('données.csv')
données.head()

# Prétraitement des données
# Suppression des colonnes manquantes
données = données.dropna()

# Encodage des variables catégorielles
données = pd.get_dummies(données, columns=['sexe'])

# Création des variables d'entrée (X) et de sortie (y)
X = données.drop(['classe'], axis=1)
y = données['classe']

# Division des données en train et test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Standardisation des données
scaler = StandardScaler()
X_train = scaler.fit_transform(X_train)
X_test = scaler.transform(X_test)

# Création de l'arbre de décision
classifieur_arbre = DecisionTreeClassifier(max_depth=5)
classifieur_arbre.fit(X_train, y_train)

# Création de la forêt aléatoire
classifieur_forêt = RandomForestClassifier(n_estimators=100, max_depth=5)
classifieur_forêt.fit(X_train, y_train)

# Prédiction sur les données de test
prédictions_arbre = classifieur_arbre.predict(X_test)
prédictions_forêt = classifieur_forêt.predict(X_test)

# Évaluation des modèles
print('Arbre de décision')
print(classification_report(y_test, prédictions_arbre))
print('\nForêt aléatoire')
print(classification_report(y_test, prédictions_forêt))

```

**Explication du code :**

**Importation des bibliothèques**

* **numpy** : pour les opérations mathématiques.
* **pandas** : pour la manipulation de données.
* **sklearn.model_selection** : pour diviser les données en train et test.
* **sklearn.preprocessing** : pour standardiser les données.
* **sklearn.tree** : pour créer l'arbre de décision.
* **sklearn.ensemble** : pour créer la forêt aléatoire.
* **sklearn.metrics** : pour évaluer les modèles.

**Chargement et prétraitement des données**

* Chargement des données à partir d'un fichier CSV.
* Suppression des lignes manquantes.
* Encodage des variables catégorielles à l'aide d'une variable fictive.
* Création des variables d'entrée (X) et de sortie (y).

**Division des données en train et test**

* Division des données en deux ensembles : un ensemble de train pour ajuster les modèles et un ensemble de test pour les évaluer.

**Standardisation des données**

* Mise à l'échelle des données pour que toutes les variables aient une moyenne de 0 et un écart-type de 1. Cela améliore la convergence des modèles.

**Création de l'arbre de décision et de la forêt aléatoire**

* Création d'un arbre de décision avec une profondeur maximale de 5.
* Création d'une forêt aléatoire avec 100 arbres de décision et une profondeur maximale de 5.

**Entraînement des modèles**

* Ajustement des modèles aux données de train.

**Prédiction sur les données de test**

* Utilisation des modèles pour prédire les classes des données de test.

**Évaluation des modèles**

* Calcul et affichage des métriques d'évaluation du modèle (précision, rappel, F1-score).