```julia

using LinearAlgebra

function matriz_diagonal(n)
    # Crea una matriz diagonal de tamaño n x n con 1 en la diagonal principal
    diagonal = zeros(n, n)
    for i in 1:n
        diagonal[i, i] = 1
    end
    return diagonal
end

function matriz_hilbert(n)
    # Crea una matriz de Hilbert de tamaño n x n
    hilbert = zeros(n, n)
    for i in 1:n
        for j in 1:n
            hilbert[i, j] = 1 / (i + j - 1)
        end
    end
    return hilbert
end

function matriz_vandermonde(n, x)
    # Crea una matriz de Vandermonde de tamaño n x n con entradas x[1], x[2], ..., x[n]
    vandermonde = zeros(n, n)
    for i in 1:n
        for j in 1:n
            vandermonde[i, j] = x[j]^(i - 1)
        end
    end
    return vandermonde
end

function matriz_identidad(n)
    # Crea una matriz identidad de tamaño n x n
    identidad = zeros(n, n)
    for i in 1:n
        identidad[i, i] = 1
    end
    return identidad
end

function frobenius_norm(A)
    # Calcula la norma de Frobenius de una matriz A
    suma = 0
    for i in 1:size(A, 1)
        for j in 1:size(A, 2)
            suma += abs(A[i, j])^2
        end
    end
    return sqrt(suma)
end

function inversa_matriz(A)
    # Calcula la inversa de una matriz A usando el método de Gauss-Jordan
    [L, U, P] = lu(A)
    b = zeros(size(A, 1))
    x = zeros(size(A, 2))
    for i in 1:size(A, 2)
        b = zeros(size(A, 1))
        b[i] = 1
        x = L \ (P * b)
        x = U \ x
        A[:, i] = x
    end
    return A
end

function eigenvalores_eigenvectores(A)
    # Calcula los eigenvalores y eigenvectores de una matriz A
    eigenvalores, eigenvectores = eig(A)
    return eigenvalores, eigenvectores
end

function determinante(A)
    # Calcula el determinante de una matriz A usando la eliminación gaussiana
    LU, pivotes = lu(A)
    det = 1
    for i in 1:size(A, 1)
        det *= LU[i, i]
    end
    for i in 1:size(A, 1)
        if pivotes[i] != i
            det = -det
        end
    end
    return det
end

function rango(A)
    # Calcula el rango de una matriz A usando la eliminación gaussiana
    LU, pivotes = lu(A)
    rango = 0
    for i in 1:size(A, 1)
        if LU[i, i] != 0
            rango += 1
        end
    end
    return rango
end

function espacio_nulo(A)
    # Calcula el espacio nulo de una matriz A usando la eliminación gaussiana
    LU, pivotes = lu(A)
    vectores_nulos = zeros(size(A, 2) - rango(A), size(A, 2))
    for i in 1:size(A, 2) - rango(A)
        b = zeros(size(A, 1))
        b[i] = 1
        x = L \ (P * b)
        x = U \ x
        vectores_nulos[:, i] = x
    end
    return vectores_nulos
end

```