**Analyse des données statistiques complexes à l'aide de régression linéaire multiple**

```julia

using StatsBase, LinearAlgebra, Distributions

# Importer les données
data = readcsv("data.csv")

# Créer une matrice de caractéristiques
X = data[2:end, :]  # Ignorer la première colonne (étiquette)

# Créer un vecteur de réponses
y = data[:, 1]    # Première colonne (étiquette)

# Normaliser les données
X = normalize(X)

# Initialiser les paramètres du modèle
w = 10 * rand(size(X, 2))  # Poids initiaux
b = rand()                # Biais initial

# Définir la fonction de perte (erreur quadratique moyenne)
loss(w, b) = mean((y - (w'X + b)).^2)

# Définir la fonction de gradient
grad_loss(w, b) = ([2 * (X' * (y .- (w'X + b)))), 2 * mean(y .- (w'X + b))]

# Définir la taille du pas de gradient
eta = 0.005

# Itérer sur les mises à jour de gradient
for i in 1:1000
    # Calculer le gradient
    gradient = grad_loss(w, b)

    # Mettre à jour les paramètres
    w -= eta * gradient[1]
    b -= eta * gradient[2]
end

# Évaluer le modèle
r2 = 1 - var(y - (w'X + b)) / var(y)
println("Coefficient de détermination (R²) :", r2)

# Prédire les étiquettes pour les nouvelles données
new_data = readcsv("new_data.csv")[:, 2:end]  # Ignorer la première colonne (étiquette)
new_data = normalize(new_data)
new_labels = w'new_data + b

# Afficher les prédictions
println("Prédictions :", new_labels)

```

**Explication du code :**

* Nous importons les bibliothèques nécessaires.
* Nous lisons les données à partir du fichier CSV.
* Nous créons une matrice de caractéristiques `X` et un vecteur de réponses `y`.
* Nous normalisons les données pour mettre les caractéristiques à la même échelle.
* Nous initialisons les paramètres du modèle (poids `w` et biais `b`).
* Nous définissons la fonction de perte (erreur quadratique moyenne).
* Nous définissons la fonction de gradient qui calcule les dérivées partielles de la fonction de perte par rapport aux paramètres.
* Nous initialisons la taille du pas de gradient `eta`.
* Nous itérons sur les mises à jour de gradient pour optimiser les paramètres du modèle.
* Nous évaluons le modèle à l'aide du coefficient de détermination (R²).
* Nous prédisons les étiquettes pour de nouvelles données.