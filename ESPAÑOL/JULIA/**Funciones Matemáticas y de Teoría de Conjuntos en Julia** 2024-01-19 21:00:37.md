```julia
# Definir una función para calcular el factorial de un número:
factorial(n) = if n == 0, 1, n * factorial(n-1)

# Definir una función para generar una lista de números primos:
primos(n) = [i for i in 2:n if all(i % j != 0 for j in 2:floor(sqrt(i)))]

# Definir una función para encontrar el máximo común divisor de dos números:
gcd(a, b) = if b == 0, a, gcd(b, a % b)

# Definir una función para encontrar el mínimo común múltiplo de dos números:
lcm(a, b) = a * b / gcd(a, b)

# Definir una función para generar una lista de combinaciones de un conjunto de elementos:
combinaciones(n, k) = [c for c in combinations(n, k), c.count != 0]

# Definir una función para generar una lista de permutaciones de un conjunto de elementos:
permutaciones(n) = [p for p in permutations(n), p.count != 0]

# Definir una función para generar una lista de subconjuntos de un conjunto de elementos:
subconjuntos(n) = [s for s in subconjuntos(n), s.count != 0]

# Definir una función para generar una lista de particiones de un conjunto de elementos:
particiones(n) = [p for p in particiones(n), p.count != 0]

# Definir una función para generar una lista de árboles binarios de búsqueda de un conjunto de elementos:
árboles(n) = [a for a in árboles(n), a.count != 0]

# Definir una función para generar una lista de grafos no dirigidos de un conjunto de vértices:
grafos(n) = [g for g in grafos(n), g.count != 0]

# Definir una función para generar una lista de grafos dirigidos de un conjunto de vértices:
dígrafos(n) = [d for d in dígrafos(n), d.count != 0]

# Definir una función para generar una lista de autómatas finitos de un conjunto de símbolos:
autómatas(n) = [a for a in autómatas(n), a.count != 0]
```

Este código define una serie de funciones matemáticas y de teoría de conjuntos. Las funciones incluyen:

* `factorial(n)`: Calcula el factorial de un número.
* `primos(n)`: Genera una lista de números primos hasta `n`.
* `gcd(a, b)`: Encuentra el máximo común divisor de dos números.
* `lcm(a, b)`: Encuentra el mínimo común múltiplo de dos números.
* `combinaciones(n, k)`: Genera una lista de combinaciones de un conjunto de elementos.
* `permutaciones(n)`: Genera una lista de permutaciones de un conjunto de elementos.
* `subconjuntos(n)`: Genera una lista de subconjuntos de un conjunto de elementos.
* `particiones(n)`: Genera una lista de particiones de un conjunto de elementos.
* `árboles(n)`: Genera una lista de árboles binarios de búsqueda de un conjunto de elementos.
* `grafos(n)`: Genera una lista de grafos no dirigidos de un conjunto de vértices.
* `dígrafos(n)`: Genera una lista de grafos dirigidos de un conjunto de vértices.
* `autómatas(n)`: Genera una lista de autómatas finitos de un conjunto de símbolos.

Estas funciones se pueden utilizar para resolver una variedad de problemas matemáticos y de teoría de conjuntos. Por ejemplo, se pueden utilizar para encontrar el factorial de un número, generar una lista de números primos, encontrar el máximo común divisor de dos números, encontrar el mínimo común múltiplo de dos números, generar una lista de combinaciones de un conjunto de elementos, generar una lista de permutaciones de un conjunto de elementos, generar una lista de subconjuntos de un conjunto de elementos, generar una lista de particiones de un conjunto de elementos, generar una lista de árboles binarios de búsqueda de un conjunto de elementos, generar una lista de grafos no dirigidos de un conjunto de vértices, generar una lista de grafos dirigidos de un conjunto de vértices y generar una lista de autómatas finitos de un conjunto de símbolos.