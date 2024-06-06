```julia

using LinearAlgebra, Statistics

# Définition d'une fonction pour calculer la moyenne d'un vecteur
moyenne(x) = sum(x) / length(x)

# Définition d'une fonction pour calculer la variance d'un vecteur
variance(x) = sum((x - moyenne(x))^2) / length(x)

# Définition d'une fonction pour générer une matrice de corrélation
correlation_matrix(X) = cov(X) ./ sqrt(diag(cov(X))' * diag(cov(X)))

# Définition d'une fonction pour effectuer une analyse en composantes principales (ACP)
pca(X, n_components) = eig(correlation_matrix(X))[:, n_components:-1:1]

# Définition d'une fonction pour réduire la dimensionnalité d'une matrice à l'aide de l'ACP
reduce_dimensionality(X, n_components) = X * pca(X, n_components)

# Définition d'une fonction pour visualiser les composants principaux
plot_pca(X, n_components) = scatter(X * pca(X, n_components)[:, 1:2])

# Lecture d'un fichier de données CSV
data = readdlm("data.csv")

# Calcul de la moyenne et de la variance de chaque colonne de données
print("Moyennes :")
for j in 1:size(data, 2)
    println(moyenne(data[:, j]))
end

println("Variances :")
for j in 1:size(data, 2)
    println(variance(data[:, j]))
end

# Calcul de la matrice de corrélation des données
corr_matrix = correlation_matrix(data)

# Affichage de la matrice de corrélation
print("Matrice de corrélation :")
println(corr_matrix)

# Réduction de la dimensionnalité des données à 2 composantes principales
reduced_data = reduce_dimensionality(data, 2)

# Visualisation des composants principaux
plot_pca(data, 2)

```