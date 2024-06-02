Voici un code Python complexe et varié :

```python
import numpy as np
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import StandardScaler

# Générer un ensemble de données
X = np.random.rand(1000, 3)
y = np.dot(X, np.array([1, 2, 3])) + np.random.randn(1000)

# Standardiser les données
scaler = StandardScaler()
X = scaler.fit_transform(X)
y = scaler.fit_transform(y.reshape(-1, 1)).ravel()

# Diviser les données en train et test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2)

# Créer un modèle de régression linéaire
model = LinearRegression()

# Entraîner le modèle
model.fit(X_train, y_train)

# Évaluer le modèle
score = model.score(X_test, y_test)
print(f"Score du modèle : {score}")

# Tracer les données et la droite de régression
plt.scatter(X[:, 0], y)
plt.plot(X[:, 0], model.predict(X), color='red')
plt.show()

# Utiliser le modèle pour prédire les valeurs d'une nouvelle observation
new_data = np.array([[1.5, 2.5, 3.5]])
prediction = model.predict(new_data)
print(f"Prédiction pour la nouvelle observation : {prediction}")

# Exemple d'utilisation de la bibliothèque Scikit-Learn pour entraîner un classifieur d'arbres de décision
from sklearn.tree import DecisionTreeClassifier

# Créer un classifieur d'arbres de décision
clf = DecisionTreeClassifier()

# Entraîner le classifieur
clf.fit(X_train, y_train)

# Évaluer le classifieur
score = clf.score(X_test, y_test)
print(f"Score du classifieur : {score}")

# Exemple d'utilisation de la bibliothèque Pandas pour manipuler des données sous forme de DataFrame
import pandas as pd

# Créer un DataFrame
df = pd.DataFrame({
    "Nom": ["Alice", "Bob", "Charlie"],
    "Âge": [25, 30, 35],
    "Salaire": [100000, 120000, 150000]
})

# Afficher le DataFrame
print(df)

# Filtrer le DataFrame pour les personnes âgées de plus de 30 ans
df_filtered = df[df["Âge"] > 30]

# Tri du DataFrame par salaire
df_sorted = df.sort_values("Salaire", ascending=False)

# Grouper le DataFrame par nom et calculer la moyenne du salaire
df_grouped = df.groupby("Nom")["Salaire"].mean()

# Exemple d'utilisation de la bibliothèque NetworkX pour créer et manipuler des graphes
import networkx as nx

# Créer un graphe
G = nx.Graph()

# Ajouter des nœuds et des arêtes au graphe
G.add_nodes_from(["A", "B", "C", "D", "E"])
G.add_edges_from([("A", "B"), ("B", "C"), ("C", "D"), ("D", "E"), ("E", "A")])

# Afficher le graphe
nx.draw(G, with_labels=True)
plt.show()
```

**Explication du code :**

Ce code illustre différentes fonctionnalités et bibliothèques de Python, notamment :

* NumPy pour la manipulation de matrices et de vecteurs.
* Matplotlib pour la visualisation de données.
* Scikit-Learn pour l'apprentissage automatique, comprenant la régression linéaire et la classification par arbre de décision.
* Pandas pour la manipulation de données dans des DataFrames.
* NetworkX pour la création et la manipulation de graphes.

Le code effectue les opérations suivantes :

* Génère des données de régression linéaire.
* Standardise les données à l'aide d'un Scaler standard.
* Divise les données en ensembles d'entraînement et de test.
* Entraîne un modèle de régression linéaire.
* Évalue le modèle de régression linéaire et trace les données avec la droite de régression.
* Prédit des valeurs pour de nouvelles observations.
* Entraîne un classifieur d'arbres de décision.
* Évalue le classifieur d'arbres de décision.
* Crée un DataFrame Pandas et le manipule en filtrant, triant et regroupant les données.
* Crée un graphe NetworkX et affiche le graphe.