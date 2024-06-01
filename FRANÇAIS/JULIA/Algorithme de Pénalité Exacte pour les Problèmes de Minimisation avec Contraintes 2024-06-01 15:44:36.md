```julia
# Définir la fonction de Lagrange pour un problème de minimisation avec contraintes
lagrangien(f::Function, g::Vector{Function}, λ::Vector{Float64}; x) = f(x) + λ' * g(x)

# Implémenter l'algorithme de pénalité exacte pour résoudre le problème de minimisation avec contraintes
function penalite_exacte(f::Function, g::Vector{Function}, x0::Vector{Float64}; ε=1e-6, ρ=1.1)
    # Initialiser les paramètres de l'algorithme
    λ = zeros(length(g))
    x = x0
    k = 0

    # Itérer jusqu'à ce que les conditions d'arrêt soient satisfaites
    while norm(g(x)) > ε || k < 50
        # Calculer le gradient et la hessienne du lagrangien
        grad_lagrangien = gradient(lagrangien, x, f, g, λ)
        hessien_lagrangien = hessian(lagrangien, x, f, g, λ)

        # Résoudre le problème de minimisation quadratique trust-region pour le lagrangien
        δx = solve(hessien_lagrangien, -grad_lagrangien)

        # Mettre à jour les multiplicateurs de Lagrange
        λ = λ + ρ * (g(x) + δx)

        # Mettre à jour la solution
        x = x + δx

        # Incrémenter le compteur d'itérations
        k += 1
    end

    return x, λ
end

# Exemple d'utilisation de l'algorithme pour un problème de minimisation avec contraintes quadratiques
f(x) = x[1]^2 + x[2]^2
g(x) = [x[1] + x[2] - 1; x[1] - x[2] - 1]
x0 = [0.5, 0.5]

# Résoudre le problème
x, λ = penalite_exacte(f, g, x0)

# Afficher la solution
println("Solution :", x)
println("Multiplicateurs de Lagrange :", λ)
```

**Explication du code :**

Ce code implémente l'algorithme de pénalité exacte pour résoudre un problème de minimisation avec contraintes. L'algorithme fonctionne en transformant le problème contraint en un problème non contraint en ajoutant un terme de pénalité au sein de la fonction objectif. Le terme de pénalité est pondéré par des multiplicateurs de Lagrange, qui sont ajustés itérativement.

La fonction `lagrangien` définit le lagrangien, qui est la fonction objectif avec le terme de pénalité ajouté. La fonction `penalite_exacte` implémente l'algorithme de pénalité exacte, qui commence par une estimation initiale de la solution et des multiplicateurs de Lagrange. À chaque itération, l'algorithme résout un problème de minimisation quadratique trust-region pour le lagrangien afin d'obtenir une nouvelle estimation de la solution. Les multiplicateurs de Lagrange sont ensuite mis à jour en fonction de la nouvelle solution. L'algorithme s'arrête lorsque les contraintes sont satisfaites ou lorsqu'un nombre maximal d'itérations a été atteint.

La dernière partie du code fournit un exemple d'utilisation de l'algorithme pour résoudre un problème de minimisation avec deux contraintes quadratiques. La solution et les multiplicateurs de Lagrange sont affichés à la fin.