```julia

using LinearAlgebra, Plots

# Definir la matriz A
A = [2 1 1; 1 2 1; 1 1 2]

# Definir el vector b
b = [1; 2; 3]

# Resolver el sistema lineal Ax = b usando factorización LU
L, U, P = lu(A)
y = forward_substitution(L, P * b)
x = backward_substitution(U, y)

# Verificar la solución
Ax = A * x
println("Solución:")
println(x)
println("Verificación:")
println(Ax)

# Calcular los autovalores y autovectores de A
eigvals, eigvecs = eigen(A)

# Imprimir los autovalores y autovectores
println("Autovalores:")
println(eigvals)
println("Autovectores:")
println(eigvecs)

# Graficar los autovectores
plot(eigvecs[:, 1], eigvecs[:, 2], marker=:o, legend=:Autovectores)

# Calcular la norma de la matriz A
normA = norm(A)

# Imprimir la norma de la matriz A
println("Norma de la matriz A:")
println(normA)

# Calcular la condición de la matriz A
condA = cond(A)

# Imprimir la condición de la matriz A
println("Condición de la matriz A:")
println(condA)

# Calcular el rango de la matriz A
rankA = rank(A)

# Imprimir el rango de la matriz A
println("Rango de la matriz A:")
println(rankA)

# Calcular el espacio nulo de la matriz A
nullspaceA = nullspace(A)

# Imprimir el espacio nulo de la matriz A
println("Espacio nulo de la matriz A:")
println(nullspaceA)

```

Este código realiza una serie de operaciones matemáticas con una matriz A y un vector b en Julia.

El código comienza importando las bibliotecas necesarias, LinearAlgebra y Plots.

A continuación, se define la matriz A y el vector b.

La función `lu` se utiliza para realizar la factorización LU de la matriz A. Esto es útil para resolver sistemas de ecuaciones lineales de forma eficiente.

La función `forward_substitution` se utiliza para resolver el sistema triangular inferior L * y = P * b.

La función `backward_substitution` se utiliza para resolver el sistema triangular superior U * x = y.

El código verifica la solución del sistema lineal Ax = b.

La función `eigen` se utiliza para calcular los autovalores y autovectores de la matriz A.

La función `plot` se utiliza para graficar los autovectores.

Se calcula la norma de la matriz A.

Se calcula la condición de la matriz A.

Se calcula el rango de la matriz A.

Se calcula el espacio nulo de la matriz A.

Al final, el código imprime los resultados de las operaciones realizadas.