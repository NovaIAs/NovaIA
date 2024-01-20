```julia
# Codifica y decodifica mensajes usando el algoritmo de Vernam (cifrado de una sola vez)

# Función para generar una clave única aleatoria de la misma longitud que el mensaje
function generar_clave(longitud_mensaje::Int)
    key = rand(Bool[], longitud_mensaje)
    return key
end

# Función para codificar un mensaje usando la clave única aleatoria
function codificar_mensaje(mensaje::String, clave::Bool[])
    # Convertir el mensaje en una lista de bits
    bits_mensaje = map(c -> c == '1' ? true : false, mensaje)

    # Codificar el mensaje bit a bit utilizando la clave única aleatoria
    bits_codificados = zip(bits_mensaje, clave) |> map(a -> a[1] ? !a[2] : a[2])

    # Convertir la lista de bits codificados de nuevo en un string
    mensaje_codificado =  join(bits_codificados |> map(b -> b ? '1' : '0'))

    return mensaje_codificado
end

# Función para decodificar un mensaje usando la clave única aleatoria
function decodificar_mensaje(mensaje_codificado::String, clave::Bool[])
    # Convertir el mensaje codificado en una lista de bits
    bits_codificados = map(c -> c == '1' ? true : false, mensaje_codificado)

    # Decodificar el mensaje bit a bit utilizando la clave única aleatoria
    bits_decodificados = zip(bits_codificados, clave) |> map(a -> a[1] ? !a[2] : a[2])

    # Convertir la lista de bits decodificados de nuevo en un string
    mensaje_decodificado =  join(bits_decodificados |> map(b -> b ? '1' : '0'))

    return mensaje_decodificado
end

# Generar una clave única aleatoria
clave = generar_clave(100)

# Codificar un mensaje usando la clave única aleatoria
mensaje_codificado = codificar_mensaje("Hola Mundo!", clave)

# Decodificar el mensaje codificado con la misma clave única aleatoria
mensaje_decodificado = decodificar_mensaje(mensaje_codificado, clave)

# Mostrar el mensaje codificado y decodificado
println("Mensaje codificado: $mensaje_codificado")
println("Mensaje decodificado: $mensaje_decodificado")
```

Explicación:

El código crea una clave única aleatoria de una longitud fija utilizando la función `generar_clave`. Luego usa esta clave para codificar el mensaje de texto en un mensaje codificado binario usando la función `codificar_mensaje`. Después, el código decodifica el mensaje codificado en el mensaje original usando la misma clave aleatoria con la función `decodificar_mensaje`. El código muestra finalmente los resultados.

El código utiliza la función `join` para unir los elementos de un generador en un string. La función `map` se utiliza para aplicar una función a cada elemento de un generador. La función `zip` se utiliza para crear un iterador de tuplas de elementos de dos generadores.

El código utiliza los operadores de comparación `==` y `!=` para comparar valores booleanos. El operador `?` se utiliza para realizar una condicional abreviada, lo que permite escribir condiciones en una sola línea. El operador `!` se utiliza para negar un valor booleano.