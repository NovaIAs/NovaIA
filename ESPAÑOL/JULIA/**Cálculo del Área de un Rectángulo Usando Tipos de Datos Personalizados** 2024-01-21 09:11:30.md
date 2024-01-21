```julia

# Definición de tipos de datos personalizados

struct Punto
    x::Float64
    y::Float64
end

struct Rectángulo
    izquierda::Punto
    derecha::Punto
end

# Función para calcular el área de un rectángulo

function area(rect::Rectángulo)
    ancho = rect.derecha.x - rect.izquierda.x
    alto = rect.derecha.y - rect.izquierda.y
    return ancho * alto
end

# Función para crear un nuevo rectángulo

function nuevo_rectángulo(izquierda::Punto, derecha::Punto)
    return Rectángulo(izquierda, derecha)
end

# Función principal

function main()
    # Crear un punto
    punto1 = Punto(1.0, 2.0)

    # Crear un rectángulo
    rectángulo1 = nuevo_rectángulo(punto1, Punto(3.0, 4.0))

    # Calcular el área del rectángulo
    área = area(rectángulo1)

    # Imprimir el área del rectángulo
    println("El área del rectángulo es: $área")
end

# Llamada a la función principal
main()

```

Explicación del código:

* Definimos dos tipos de datos personalizados: `Punto` y `Rectángulo`.

* La función `area` calcula el área de un rectángulo.

* La función `nuevo_rectángulo` crea un nuevo rectángulo.

* La función `main` es la función principal del programa.

* En `main`, creamos un punto, un rectángulo y calculamos el área del rectángulo.

* Por último, imprimimos el área del rectángulo.