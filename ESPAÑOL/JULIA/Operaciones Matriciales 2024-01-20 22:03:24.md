```julia

# Definición de la función "suma_matrices"
function suma_matrices(A, B)
    # Verificamos que las matrices tengan el mismo tamaño
    if size(A) != size(B)
        error("Las matrices deben tener el mismo tamaño")
    end

    # Creamos una matriz vacía con el mismo tamaño que las matrices de entrada
    C = zeros(size(A))

    # Iteramos sobre cada elemento de las matrices de entrada y sumamos los valores correspondientes
    for i in 1:size(A, 1)
        for j in 1:size(A, 2)
            C[i, j] = A[i, j] + B[i, j]
        end
    end

    # Devolvemos la matriz resultante
    return C
end

# Definición de la función "producto_matrices"
function producto_matrices(A, B)
    # Verificamos que el número de columnas de la matriz A sea igual al número de filas de la matriz B
    if size(A, 2) != size(B, 1)
        error("El número de columnas de la matriz A debe ser igual al número de filas de la matriz B")
    end

    # Creamos una matriz vacía con el mismo número de filas que la matriz A y el mismo número de columnas que la matriz B
    C = zeros(size(A, 1), size(B, 2))

    # Iteramos sobre cada elemento de la matriz resultante
    for i in 1:size(C, 1)
        for j in 1:size(C, 2)
            # Calculamos el producto escalar de la fila i de la matriz A y la columna j de la matriz B
            C[i, j] = dot(A[i, :], B[:, j])
        end
    end

    # Devolvemos la matriz resultante
    return C
end

# Definición de la función "determinante"
function determinante(A)
    # Verificamos que la matriz sea cuadrada
    if size(A, 1) != size(A, 2)
        error("La matriz debe ser cuadrada")
    end

    # Si la matriz es de orden 1, devolvemos el único elemento
    if size(A, 1) == 1
        return A[1, 1]
    end

    # Calculamos la suma de los cofactores de la primera fila
    determinante = 0
    for j in 1:size(A, 2)
        cofactor = (-1)^(j + 1) * A[1, j] * determinante(A[2:end, [1:j-1, j+1:end]])
        determinante += cofactor
    end

    # Devolvemos el determinante
    return determinante
end

# Definición de la función "inversa"
function inversa(A)
    # Verificamos que la matriz sea cuadrada
    if size(A, 1) != size(A, 2)
        error("La matriz debe ser cuadrada")
    end

    # Calculamos el determinante de la matriz
    determinante = determinante(A)

    # Verificamos que el determinante no sea cero
    if determinante == 0
        error("La matriz no tiene inversa")
    end

    # Calculamos la matriz de cofactores
    cofactores = zeros(size(A))
    for i in 1:size(A, 1)
        for j in 1:size(A, 2)
            cofactores[i, j] = (-1)^(i + j) * determinante(A[1:i-1, [1:j-1, j+1:end]])
        end
    end

    # Calculamos la matriz adjunta
    adjunta = transpose(cofactores)

    # Calculamos la inversa de la matriz
    inversa = adjunta / determinante

    # Devolvemos la inversa de la matriz
    return inversa
end

# Ejemplo de uso de las funciones

# Definimos dos matrices
A = [1 2 3; 4 5 6; 7 8 9]
B = [9 8 7; 6 5 4; 3 2 1]

# Calculamos la suma de las matrices
C = suma_matrices(A, B)

# Calculamos el producto de las matrices
D = producto_matrices(A, B)

# Calculamos el determinante de la matriz A
determinante_A = determinante(A)

# Calculamos la inversa de la matriz A
inversa_A = inversa(A)

# Imprimimos los resultados
println("Suma de las matrices:")
println(C)
println()
println("Producto de las matrices:")
println(D)
println()
println("Determinante de la matriz A:")
println(determinante_A)
println()
println("Inversa de la matriz A:")
println(inversa_A)

```

Explicación del código:

* La función `suma_matrices` suma dos matrices de igual tamaño y devuelve la matriz resultante.
* La función `producto_matrices` multiplica dos matrices compatibles y devuelve la matriz resultante.
* La función `determinante` calcula el determinante de una matriz cuadrada.
* La función `inversa` calcula la matriz inversa de una matriz cuadrada no singular.
* En el ejemplo de uso, se definen dos matrices, se calculan su suma, su producto, el determinante de una de ellas y su inversa.
* Los resultados se imprimen en la consola.