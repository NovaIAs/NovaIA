**Programme de Réduction de Dimension**

Ce programme vise à réduire la dimensionnalité des données en utilisant l'Analyse en Composantes Principales (ACP). Il utilise des techniques avancées pour gérer les données volumineuses et les calculs complexes.

```julia
using LinearAlgebra, Statistics, CSV

# Chargement des données
data = CSV.read("data.csv")
X = data[:, 1:end-1]  # Matrice des données (sans la cible)
y = data[:, end]       # Vecteur cible

# Centrage et normalisation des données
X = center(X)
X = standardize(X)

# Calcul de l'ACP
acp = PCA(X)
V = acp.loadings

# Répartition de la variance expliquée (en pourcentage)
explained_variance = acp.explained_variance
percents = [percent(ve, sum(explained_variance)) for ve in explained_variance]

# Visualisation de la répartition de la variance expliquée
pyplot(percents, title="Répartition de la Variance Expliquée")
xlabel("Composante")
ylabel("Pourcentage de Variance Expliquée")

# Transformation des données
new_data = V' * X  # Projection dans l'espace réduit

# Génération d'un nouveau fichier CSV avec les données réduites
CSV.write("data_reduite.csv", [new_data, y])

# Sauvegarde du modèle ACP
save("model.acp", acp)
```

**Explication du Code**

* Chargement des données à partir d'un fichier CSV.
* Centrage et normalisation des données pour éliminer les biais et améliorer les performances de l'ACP.
* Calcul de l'ACP et extraction des vecteurs propres (chargements).
* Calcul de la répartition de la variance expliquée pour évaluer la pertinence des composants principaux.
* Visualisation de la répartition de la variance expliquée.
* Transformation des données dans l'espace réduit.
* Génération d'un nouveau fichier CSV avec les données réduites.
* Sauvegarde du modèle ACP pour une utilisation ultérieure.