**Code Complexe en Julia**

```julia
# Bibliothèque pour les calculs numériques
using LinearAlgebra, Calculus

# Type défini par l'utilisateur pour représenter une matrice complexe
mutable struct ComplexMatrix{T<:Real}
    re::Matrix{T}     # Partie réelle
    im::Matrix{T}     # Partie imaginaire
end

# Méthode pour ajouter deux matrices complexes
function +(A::ComplexMatrix, B::ComplexMatrix)
    ComplexMatrix(A.re + B.re, A.im + B.im)
end

# Méthode pour multiplier une matrice complexe par un scalaire
function *(a::Real, A::ComplexMatrix)
    ComplexMatrix(a * A.re, a * A.im)
end

# Méthode pour calculer l'inverse d'une matrice complexe
function inv(A::ComplexMatrix)
    inv(A.re * I + A.im) * I
end

# Méthode pour calculer les valeurs propres d'une matrice complexe
function eig(A::ComplexMatrix)
    eig(A.re * I + A.im)
end

# Méthode pour résoudre un système linéaire complexe
function solve(A::ComplexMatrix, b::Vector{Complex})
    (A.re * I + A.im) \ b
end

# Exemple d'utilisation

# Créer une matrice complexe
M = ComplexMatrix(re = randn(3, 3), im = randn(3, 3))

# Ajouter une matrice complexe
M += ComplexMatrix(re = ones(3, 3), im = zeros(3, 3))

# Multiplier par un scalaire
M *= 2.0

# Calculer l'inverse
Minv = inv(M)

# Calculer les valeurs propres
eigvals = eig(M)

# Résoudre un système linéaire
b = Complex(randn(3))
x = solve(M, b)

```

**Explication du Code**

Ce code implémente un type de matrice complexe personnalisé et des méthodes pour effectuer des opérations arithmétiques et algébriques courantes sur de telles matrices. Il utilise les bibliothèques LinearAlgebra et Calculus de Julia pour les opérations numériques sous-jacentes.

Le type `ComplexMatrix` représente une matrice complexe en tant qu'un objet composite contenant des matrices séparées pour les parties réelle et imaginaire. Les méthodes `+` et `*` permettent l'addition et la multiplication par des scalaires.

Les méthodes `inv` et `eig` utilisent la bibliothèque Calculus pour calculer l'inverse et les valeurs propres d'une matrice complexe.

Enfin, la méthode `solve` utilise la méthode `\`` de LinearAlgebra pour résoudre un système linéaire complexe.

L'exemple d'utilisation montre comment créer une matrice complexe, effectuer des opérations de base et résoudre un système linéaire.