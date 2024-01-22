```julia

# Importación de biblioteca
using Printf, LinearAlgebra, Random, Statistics

# Definiciones de tipos personalizados
type Punto2D
    x::Real
    y::Real
end

type Rectangulo
    minimo::Punto2D
    maximo::Punto2D
end

# Definiciones de funciones
function distancia(p1::Punto2D, p2::Punto2D)
    sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2)
end

function area(rect::Rectangulo)
    (rect.maximo.x - rect.minimo.x) * (rect.maximo.y - rect.minimo.y)
end

function perimetro(rect::Rectangulo)
    2 * (rect.maximo.x - rect.minimo.x) + 2 * (rect.maximo.y - rect.minimo.y)
end

function centro(rect::Rectangulo)
    Punto2D((rect.maximo.x + rect.minimo.x) / 2, (rect.maximo.y + rect.minimo.y) / 2)
end

function contiene(rect::Rectangulo, p::Punto2D)
    p.x >= rect.minimo.x && p.x <= rect.maximo.x && p.y >= rect.minimo.y && p.y <= rect.maximo.y
end

function colisionan(rect1::Rectangulo, rect2::Rectangulo)
    contiene(rect1, rect2.minimo) || contiene(rect1, rect2.maximo) || contiene(rect2, rect1.minimo) || contiene(rect2, rect1.maximo)
end

function generar_aleatorio(n::Int)
    res = zeros(n, n)  # Crea una matriz de ceros nxn
    for i in 1:n
        for j in 1:n
            res[i, j] = rand()  # Genera un número aleatorio entre 0 y 1
        end
    end
    res  # Devuelve la matriz
end

function matriz_identidad(n::Int)
    res = zeros(n, n)  # Crea una matriz de ceros nxn
    for i in 1:n
        res[i, i] = 1  # Asigna 1 a la diagonal principal
    end
    res  # Devuelve la matriz
end

function diagonal_matriz(a::AbstractVector)
    n = length(a)
    res = zeros(n, n)
    for i in 1:n
        res[i, i] = a[i]
    end
    res  # Devuelve la matriz
end

function matriz_transpuesta(a::AbstractMatrix)
    res = zeros(size(a)[2], size(a)[1])
    for i in 1:size(a)[1]
        for j in 1:size(a)[2]
            res[j, i] = a[i, j]
        end
    end
    res  # Devuelve la matriz
end

function matriz_inversa(a::AbstractMatrix)
    inv(a)  # Utiliza la función incorporada de Julia para calcular la matriz inversa
end

function determinante(a::AbstractMatrix)
    det(a)  # Utiliza la función incorporada de Julia para calcular el determinante
end

function valores_propios(a::AbstractMatrix)
    eigvals(a)  # Utiliza la función incorporada de Julia para calcular los valores propios
end

function vectores_propios(a::AbstractMatrix)
    eigvecs(a)  # Utiliza la función incorporada de Julia para calcular los vectores propios
end

function matriz_diagonalizable(a::AbstractMatrix)
    all(isfinite.(valores_propios(a))) && all(isfinite.(vectores_propios(a)))
end

# Uso de las funciones definidas
println("Distancia entre dos puntos:")
p1 = Punto2D(1.0, 2.0)
p2 = Punto2D(3.0, 4.0)
println("  Distancia: $(distancia(p1, p2))")

println("\nÁrea de un rectángulo:")
rect = Rectangulo(Punto2D(0.0, 0.0), Punto2D(10.0, 20.0))
println("  Área: $(area(rect))")

println("\nPerímetro de un rectángulo:")
println("  Perímetro: $(perimetro(rect))")

println("\nCentro de un rectángulo:")
println("  Centro: $(centro(rect))")

println("\n¿Contiene el rectángulo al punto (5.0, 10.0)?")
println("  $(contiene(rect, Punto2D(5.0, 10.0)))")

println("\n¿Colisionan dos rectángulos?")
rect2 = Rectangulo(Punto2D(5.0, 5.0), Punto2D(15.0, 15.0))
println("  $(colisionan(rect, rect2))")

println("\nGeneración de una matriz aleatoria:")
matriz = generar_aleatorio(5)
println("  Matriz:\n$(matriz)")

println("\nMatriz identidad:")
n = 3
println("  Tamaño $(n)×$(n):\n$(matriz_identidad(n))")

println("\nMatriz diagonal a partir de un vector:")
v = [1.0, 2.0, 3.0]
println("  Vector: $(v)")
println("  Matriz diagonal:\n$(diagonal_matriz(v))")

println("\nMatriz transpuesta:")
matriz = Matrix([1 2 3; 4 5 6; 7 8 9])
println("  Matriz original:\n$(matriz)")
println("  Matriz transpuesta:\n$(matriz_transpuesta(matriz))")

println("\nMatriz inversa:")
matriz = Matrix([1 2 3; 4 5 6; 7 8 9])
println("  Matriz original:\n$(matriz)")
println("  Matriz inversa:\n$(matriz_inversa(matriz))")

println("\nDeterminante:")
matriz = Matrix([1 2 3; 4 5 6; 7 8 9])
println("  Matriz:\n$(matriz)")
println("  Determinante: $(determinante(matriz))")

println("\nValores propios:")
matriz = Matrix([1 2 3; 4 5 6; 7 8 9])
println("  Matriz:\n$(matriz)")
println("  Valores propios:\n$(valores_propios(matriz))")

println("\nVectores propios:")
matriz = Matrix([1 2 3; 4 5 6; 7 8 9])
println("  Matriz:\n$(matriz)")
println("  Vectores propios:\n$(vectores_propios(matriz))")

println("\n¿Es la matriz diagonalizable?")
matriz = Matrix([1 2 3; 4 5 6; 7 8 9])
println("  Matriz:\n$(matriz)")
println("  ¿Diagonalizable?: $(matriz_diagonalizable(matriz))")

```

Explicación del código:


* Se importan las bibliotecas necesarias para el código.
* Se definen los tipos personalizados **Punto2D** y **Rectangulo**.
* Se definen varias funciones para trabajar con puntos y rectángulos, como distancia, área, perímetro, centro, contiene y colisionan.
* También se definen funciones para generar matrices aleatorias, matrices identidad, matrices diagonales a partir de vectores, matrices transpuestas, matrices inversas, determinantes, valores propios y vectores propios.
* Finalmente, se utilizan las funciones definidas para realizar cálculos y mostrar resultados en la consola.