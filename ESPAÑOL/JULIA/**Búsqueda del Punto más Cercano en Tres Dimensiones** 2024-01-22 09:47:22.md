```julia

# Definición de una estructura de datos personalizada
struct Punto3D
    x::Float64
    y::Float64
    z::Float64
end

# Definición de una función para calcular la distancia entre dos puntos en 3D
function distancia(p1::Punto3D, p2::Punto3D)
    return sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2 + (p1.z - p2.z)^2)
end

# Definición de una función para generar un conjunto de puntos en 3D aleatorios
function generarPuntos3D(n::Int)
    puntos = []
    for i in 1:n
        push!(puntos, Punto3D(rand(), rand(), rand()))
    end
    return puntos
end

# Definición de una función para encontrar el punto más cercano a un punto dado
function encontrarPuntoMasCercano(punto::Punto3D, puntos::Vector{Punto3D})
    min_distancia = Inf
    punto_mas_cercano = Punto3D(0.0, 0.0, 0.0)
    for punto_actual in puntos
        distancia_actual = distancia(punto, punto_actual)
        if distancia_actual < min_distancia
            min_distancia = distancia_actual
            punto_mas_cercano = punto_actual
        end
    end
    return punto_mas_cercano
end

# Definición de la función principal
function main()
    # Generar un conjunto de 1000 puntos en 3D aleatorios
    puntos = generarPuntos3D(1000)

    # Seleccionar un punto aleatorio como punto de referencia
    punto_referencia = puntos[rand(1:length(puntos))]

    # Encontrar el punto más cercano al punto de referencia
    punto_mas_cercano = encontrarPuntoMasCercano(punto_referencia, puntos)

    # Imprimir los resultados
    println("Punto de referencia:")
    println("  x: ", punto_referencia.x)
    println("  y: ", punto_referencia.y)
    println("  z: ", punto_referencia.z)

    println("Punto más cercano:")
    println("  x: ", punto_mas_cercano.x)
    println("  y: ", punto_mas_cercano.y)
    println("  z: ", punto_mas_cercano.z)

    println("Distancia entre los puntos:")
    println("  ", distancia(punto_referencia, punto_mas_cercano))
end

# Ejecutar la función principal
main()

```

**Explicación del código:**

1. **Definición de una estructura de datos personalizada `Punto3D`:** Esta estructura representa un punto en el espacio tridimensional. Contiene tres campos: `x`, `y` y `z`, que representan las coordenadas del punto en cada una de las tres dimensiones.


2. **Definición de una función `distancia`:** Esta función calcula la distancia entre dos puntos en el espacio tridimensional. Recibe dos puntos como argumentos y devuelve la distancia entre ellos.


3. **Definición de una función `generarPuntos3D`:** Esta función genera un conjunto de puntos aleatorios en el espacio tridimensional. Recibe el número de puntos que se desean generar como argumento y devuelve un vector de puntos.


4. **Definición de una función `encontrarPuntoMasCercano`:** Esta función encuentra el punto más cercano a un punto dado en un conjunto de puntos. Recibe el punto de referencia y el conjunto de puntos como argumentos y devuelve el punto más cercano al punto de referencia.


5. **Definición de la función `main`:** Esta es la función principal del programa. Genera un conjunto de 1000 puntos en el espacio tridimensional, selecciona un punto aleatorio como punto de referencia y encuentra el punto más cercano al punto de referencia. Imprime los resultados en la consola.