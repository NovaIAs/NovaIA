```julia
# Definición de tipos de datos personalizados
type MiTipo
    valor1 :: Int64
    valor2 :: String
end

# Función para crear objetos de tipo MiTipo
function crear_MiTipo(v1, v2)
    return MiTipo(valor1 = v1, valor2 = v2)
end

# Función para imprimir objetos de tipo MiTipo
function imprimir_MiTipo(m)
    println("Valor1: $(m.valor1)")
    println("Valor2: $(m.valor2)")
end

# Función genérica para aplicar una operación a una lista de elementos
function aplicar_operacion(f, lista)
    for x in lista
        println("Resultado para $x: $(f(x))")
    end
end

# Función para calcular la longitud de una cadena de caracteres
function longitud_cadena(cadena)
    return length(cadena)
end

# Función para convertir una cadena de caracteres a mayúsculas
function cadena_a_mayusculas(cadena)
    return uppercase(cadena)
end

# Función para sumar dos números
function sumar(a, b)
    return a + b
end

# Función principal
function main()
    # Crear una lista de objetos de tipo MiTipo
    mi_lista = [crear_MiTipo(1, "uno"), crear_MiTipo(2, "dos"), crear_MiTipo(3, "tres")]

    # Imprimir los objetos de la lista
    for m in mi_lista
        imprimir_MiTipo(m)
    end

    # Aplicar la función `longitud_cadena` a una lista de cadenas de caracteres
    aplicar_operacion(longitud_cadena, ["uno", "dos", "tres"])

    # Aplicar la función `cadena_a_mayusculas` a una lista de cadenas de caracteres
    aplicar_operacion(cadena_a_mayusculas, ["uno", "dos", "tres"])

    # Aplicar la función `sumar` a una lista de pares de números
    aplicar_operacion(sumar, [(1, 2), (3, 4), (5, 6)])
end

# Llamar a la función principal
main()
```

Explicación del código:

* Se definen dos tipos de datos personalizados: `MiTipo` y `Punto`.
* La función `crear_MiTipo` crea un objeto de tipo `MiTipo` con los valores especificados.
* La función `imprimir_MiTipo` imprime los valores de un objeto de tipo `MiTipo`.
* La función `aplicar_operacion` aplica una función especificada a cada elemento de una lista.
* La función `longitud_cadena` calcula la longitud de una cadena de caracteres.
* La función `cadena_a_mayusculas` convierte una cadena de caracteres a mayúsculas.
* La función `sumar` suma dos números.
* La función `main` es la función principal del programa.
* La función `main` crea una lista de objetos de tipo `MiTipo`, imprime los objetos de la lista, aplica las funciones `longitud_cadena`, `cadena_a_mayusculas` y `sumar` a listas de valores especificados, y finalmente llama a la función `main`.