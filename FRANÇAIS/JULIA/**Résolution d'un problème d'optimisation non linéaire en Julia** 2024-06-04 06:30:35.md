```julia

# Chargement des bibliothèques nécessaires
using LinearAlgebra, Plots, Optim

# Définition des matrices et vecteurs
A = [1 2 3; 4 5 6; 7 8 9]
b = [10; 20; 30]
x0 = [0; 0; 0]

# Définition de la fonction objective
function f(x)
    return sum(abs.(A * x - b))
end

# Résolution du problème d'optimisation
result = minimize(f, x0)

# Affichage du résultat
println("Solution optimale :")
println(result.x)

# Création d'un graphique pour visualiser la fonction objective
f_values = Vector{Float64}(length(x0))
for i = 1:length(x0)
    f_values[i] = f(x0 + i * (result.x - x0) / 100)
end

plot(1:length(x0), f_values, title="Fonction objective", xlabel="Itérations", ylabel="Valeur de la fonction")

```

**Explication du code :**

Ce code résout un problème d'optimisation non linéaire en minimisant une fonction objective à plusieurs variables.

1. **Chargement des bibliothèques nécessaires** : Le code commence par charger les bibliothèques nécessaires pour la manipulation de matrices, les tracés graphiques et l'optimisation.

2. **Définition des matrices et vecteurs** : Les matrices A et les vecteurs b et x0 sont définis pour représenter le problème d'optimisation. A est une matrice 3x3, b est un vecteur de taille 3 et x0 est un vecteur de taille 3 contenant une estimation initiale de la solution.

3. **Définition de la fonction objective** : La fonction f est définie comme la somme des valeurs absolues des différences entre A * x et b. L'objectif est de minimiser cette fonction pour trouver la valeur de x qui rend les différences aussi petites que possible.

4. **Résolution du problème d'optimisation** : La fonction minimize est utilisée pour résoudre le problème d'optimisation. Elle prend la fonction objective et l'estimation initiale comme arguments et renvoie la solution optimale.

5. **Affichage du résultat** : La solution optimale est imprimée à l'écran.

6. **Création d'un graphique** : Un graphique est créé pour visualiser la fonction objective. La fonction est évaluée pour 100 points entre l'estimation initiale et la solution optimale, et les valeurs résultantes sont tracées. Ce graphique aide à visualiser la forme de la fonction objective et à comprendre comment elle varie en fonction des valeurs de x.