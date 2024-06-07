**Fonction de tri rapide en Julia pour une liste de nombres entiers**

```julia
function quicksort(xs::Vector{Int})
    if length(xs) <= 1
        return xs
    end

    pivot = xs[rand(1:length(xs))]

    left = filter(x -> x < pivot, xs)
    right = filter(x -> x >= pivot, xs)

    return [quicksort(left), pivot, quicksort(right)]
end
```

**Explication**

Cette fonction implémente l'algorithme de tri rapide pour trier une liste de nombres entiers.

* Elle vérifie d'abord si la liste est vide ou ne contient qu'un seul élément. Si c'est le cas, elle retourne simplement la liste inchangée car elle est déjà triée.
* Ensuite, elle choisit un élément pivot aléatoire dans la liste.
* Elle partitionne ensuite la liste en deux sous-listes : "gauche" contenant les éléments inférieurs au pivot et "droite" contenant les éléments supérieurs ou égaux au pivot.
* Enfin, elle applique récursivement la fonction de tri rapide aux sous-listes gauche et droite, puis fusionne les listes résultantes avec l'élément pivot pour obtenir la liste triée.

**Code de calcul parallèle en Julia pour résoudre un système d'équations linéaires**

```julia
function solve_parallel(A::Matrix, b::Vector)
    n = size(A, 1)

    # Paralléliser la résolution de chaque équation
    results = @distributed each row in 1:n do r
        pivot = A[r, r]
        A[r, :] /= pivot
        b[r] /= pivot

        # Résoudre l'équation restante pour cette ligne
        for i in (r+1):n
            A[i, :] -= A[r, :] * A[i, r]
            b[i] -= b[r] * A[i, r]
        end
    end

    # Rassembler les résultats
    (A, b) = collect(results)
    return solve(A, b)
end
```

**Explication**

Cette fonction calcule la solution d'un système d'équations linéaires en utilisant des calculs parallèles.

* Elle divise la résolution du système en tâches indépendantes, une pour chaque équation.
* Chaque tâche est exécutée en parallèle sur un processeur distinct.
* Une fois que toutes les tâches sont terminées, les résultats sont collectés et la solution finale est calculée.
* En exploitant le parallélisme, cette fonction peut résoudre des systèmes linéaires volumineux beaucoup plus rapidement qu'un code séquentiel.

**Programme de génération de nombres aléatoires en Julia avec distribution normale**

```julia
function random_normal(mean::Float64, stddev::Float64, n::Int)
    # Utiliser la distribution Box-Muller pour générer des nombres aléatoires normalement distribués
    function box_muller()
        u1 = rand()
        u2 = rand()

        sqrt(-2 \* log(u1)) \* cos(2π \* u2)
    end

    # Générer n nombres aléatoires
    return collect(repeat(box_muller, n)) .* stddev + mean
end
```

**Explication**

Ce programme génère une liste de nombres aléatoires normalement distribués avec une moyenne et un écart type spécifiés.

* Il utilise la méthode de Box-Muller pour générer des nombres aléatoires normalement distribués.
* Cette méthode est basée sur la transformation de deux nombres aléatoires uniformes en deux nombres aléatoires normalement distribués.
* Le programme répète cette méthode n fois pour générer une liste de nombres aléatoires normalement distribués.