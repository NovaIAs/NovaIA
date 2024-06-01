**Calcul d'une intégrale définie à l'aide de la méthode des trapèzes**

```haskell
import Data.Function (on)
import Numerical.Trapezoidal

-- Définition de la fonction à intégrer
f :: Double -> Double
f x = x^2 + sin x

-- Intervalle d'intégration
a, b :: Double
a = 0
b = 1

-- Nombre de sous-intervalles
n :: Int
n = 1000

-- Calcul de l'intégrale
integral :: Double
integral = trapIntegral f a b (n-1)

-- Affichage du résultat
putStrLn ("L'intégrale de f(x) de " ++ show a ++ " à " ++ show b ++ " est : " ++ show integral)
```

**Explication du code :**

* Nous importons les paquets nécessaires.
* Nous définissons la fonction `f` à intégrer.
* Nous définissons les bornes de l'intervalle d'intégration `[a, b]`.
* Nous choisissons le nombre de sous-intervalles `n`.
* Nous appelons la fonction `trapIntegral` du paquet `Numerical.Trapezoidal` pour calculer l'intégrale.
* Nous affichons le résultat.

**Complexité du code :**

* Le code est relativement complexe car il utilise des paquets externes et des concepts de calcul numérique.
* La fonction `trapIntegral` est une implémentation de la méthode des trapèzes, qui est une méthode numérique pour calculer des intégrales.