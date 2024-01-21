**Problema:**

Crear una función en Haskell que reciba una lista de números y devuelva otra lista con los números ordenados de mayor a menor.

**Código:**

```haskell
ordenaMayorMenor :: [Int] -> [Int]
ordenaMayorMenor [] = []
ordenaMayorMenor (x:xs) = ordenaMayorMenor [y | y <- xs, y > x] ++ [x] ++ ordenaMayorMenor [y | y <- xs, y <= x]
```

**Explicación:**

La función `ordenaMayorMenor` recibe una lista de números `[Int]` como entrada y devuelve otra lista de números ordenados de mayor a menor.

La función se define de forma recursiva. El caso base es cuando la lista de entrada está vacía, en cuyo caso la función devuelve una lista vacía.

En caso contrario, la función divide la lista de entrada en dos partes:

* Una lista con los números mayores que el primer elemento de la lista de entrada.
* Una lista con los números menores o iguales que el primer elemento de la lista de entrada.

La función se llama recursivamente sobre cada una de estas dos listas, y los resultados se concatenan con el primer elemento de la lista de entrada para formar la lista ordenada.

**Ejemplo:**

```haskell
ordenaMayorMenor [5, 3, 1, 2, 4] == [5, 4, 3, 2, 1]
```

**Cosas a tener en cuenta:**

* La función `ordenaMayorMenor` es una función de ordenamiento inestable. Esto significa que si hay dos números iguales en la lista de entrada, su orden en la lista ordenada no está garantizado.
* La función `ordenaMayorMenor` tiene una complejidad temporal de O(n^2), donde n es el número de elementos en la lista de entrada. Esto significa que el tiempo de ejecución de la función crece cuadráticamente con el tamaño de la entrada.