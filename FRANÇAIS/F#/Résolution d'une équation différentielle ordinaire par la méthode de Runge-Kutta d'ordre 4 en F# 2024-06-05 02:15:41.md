**Problème :**
Résoudre l'équation différentielle ordinaire suivante à l'aide de la méthode de Runge-Kutta d'ordre 4 :

```
y' = x^2 + y
y(0) = 1
```

**Code F# :**

```f#
// Définir la fonction y'
let f x y = x ** 2 + y

// Définir les constantes
let h = 0.1 // Pas de temps
let x0 = 0.0 // Valeur initiale de x
let y0 = 1.0 // Valeur initiale de y
let n = 10 // Nombre de pas

// Créer les tableaux pour stocker les valeurs de x et y
let x = Array.create (n+1) 0.0
let y = Array.create (n+1) 0.0

// Initialiser x et y
x.[0] <- x0
y.[0] <- y0

// Boucle de Runge-Kutta d'ordre 4
for i in 1 .. n do
    let k1 = f x.[i-1] y.[i-1]
    let k2 = f (x.[i-1] + h/2.0) (y.[i-1] + k1 * h/2.0)
    let k3 = f (x.[i-1] + h/2.0) (y.[i-1] + k2 * h/2.0)
    let k4 = f (x.[i-1] + h) (y.[i-1] + k3 * h)
    x.[i] <- x.[i-1] + h
    y.[i] <- y.[i-1] + (k1 + 2.0 * k2 + 2.0 * k3 + k4) * h / 6.0
    printfn "%f %f" x.[i] y.[i]
done
```

**Explication :**

* La fonction `f` définit l'équation différentielle `y' = x^2 + y`.
* Le pas de temps `h` est fixé à 0,1, et les valeurs initiales `x0` et `y0` sont définies.
* Les tableaux `x` et `y` stockent les valeurs de `x` et `y` calculées.
* La boucle de Runge-Kutta d'ordre 4 est utilisée pour itérer sur les pas de temps et calculer les nouvelles valeurs de `x` et `y` en utilisant la formule suivante :

```
y_{i+1} = y_i + (k1 + 2k2 + 2k3 + k4) * h / 6
```

* Les coefficients `k1`, `k2`, `k3` et `k4` sont calculés à partir des valeurs de `x` et `y` aux pas de temps précédents et de `h`.
* Le code affiche les valeurs de `x` et `y` calculées à chaque pas de temps.