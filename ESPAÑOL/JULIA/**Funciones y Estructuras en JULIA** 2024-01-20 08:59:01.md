```julia
# Definición de la función factorial usando recursion
function factorial(n::Int64)
    if n == 0
        return 1
    else
        return n * factorial(n-1)
    end
end

# Función para calcular el área de un triángulo
function area_triangulo(base::Float64, altura::Float64)
    return (base * altura) / 2
end

# Función para calcular el volumen de un cubo
function volumen_cubo(lado::Float64)
    return lado^3
end

# Definición de una estructura para representar un punto en 3D
struct Punto3D{T}
    x::T
    y::T
    z::T
end

# Función para calcular la distancia entre dos puntos en 3D
function distancia(p1::Punto3D, p2::Punto3D)
    return sqrt((p2.x - p1.x)^2 + (p2.y - p1.y)^2 + (p2.z - p1.z)^2)
end

# Crea un diccionario para almacenar los nombres y edades de las personas
personas = Dict()
personas["Juan"] = 25
personas["María"] = 30
personas["Pedro"] = 40

# Función para imprimir los nombres y edades de las personas
function imprimir_personas()
    for (nombre, edad) in personas
        println("$nombre tiene $edad años")
    end
end

# Función principal
function main()
    # Calcula el factorial de 5
    println("El factorial de 5 es: $(factorial(5))")

    # Calcula el área de un triángulo con base 10 y altura 5
    println("El área del triángulo es: $(area_triangulo(10, 5))")

    # Calcula el volumen de un cubo con lado 10
    println("El volumen del cubo es: $(volumen_cubo(10))")

    # Crea un punto en 3D con coordenadas (1, 2, 3)
    punto1 = Punto3D(1, 2, 3)

    # Crea un punto en 3D con coordenadas (4, 5, 6)
    punto2 = Punto3D(4, 5, 6)

    # Calcula la distancia entre los dos puntos
    println("La distancia entre los dos puntos es: $(distancia(punto1, punto2))")

    # Imprime los nombres y edades de las personas
    imprimir_personas()
end

main()
```

**Explicación:**

Este código contiene una variedad de funciones y estructuras que realizan diferentes tareas. A continuación se explica cada una de ellas:

* **Función factorial:** Esta función calcula el factorial de un número entero no negativo utilizando la recursión.

* **Función area_triangulo:** Esta función calcula el área de un triángulo dado su base y su altura.

* **Función volumen_cubo:** Esta función calcula el volumen de un cubo dado su lado.

* **Estructura Punto3D:** Esta estructura representa un punto en el espacio 3D con coordenadas x, y y z.

* **Función distancia:** Esta función calcula la distancia entre dos puntos en el espacio 3D.

* **Diccionario personas:** Este diccionario almacena los nombres y edades de las personas.

* **Función imprimir_personas:** Esta función imprime los nombres y edades de las personas almacenadas en el diccionario.

* **Función main:** Esta función es el punto de entrada del programa y llama a las otras funciones para realizar las diferentes tareas.

En general, este código es un buen ejemplo de cómo utilizar diferentes funciones y estructuras para realizar tareas complejas en JULIA.