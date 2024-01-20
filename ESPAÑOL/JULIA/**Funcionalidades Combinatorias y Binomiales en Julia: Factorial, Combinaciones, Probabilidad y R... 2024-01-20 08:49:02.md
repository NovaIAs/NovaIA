```julia
# Definimos una función para calcular el factorial de un número.
function factorial(n)
    if n == 0
        return 1
    else
        return n * factorial(n-1)
    end
end

# Definimos una función para calcular la combinación de n elementos tomados de k en k.
function combinacion(n, k)
    return factorial(n) / (factorial(k) * factorial(n-k))
end

# Definimos una función para generar una lista de todas las combinaciones posibles de n elementos tomados de k en k.
function combinaciones(n, k)
    combinaciones = []
    for i in 1:n
        for j in i+1:n
            combinaciones.push!([i, j])
        end
    end
    return combinaciones
end

# Definimos una función para calcular el número de combinaciones posibles de n elementos tomados de k en k.
function numero_combinaciones(n, k)
    return combinacion(n, k) * factorial(k)
end

# Definimos una función para calcular la probabilidad de que un evento ocurra k veces en n ensayos, dada una probabilidad p de que el evento ocurra en cada ensayo.
function probabilidad_binomial(n, k, p)
    return combinacion(n, k) * p^k * (1-p)^(n-k)
end

# Definimos una función para generar una lista de todos los posibles resultados de n ensayos, dada una probabilidad p de que el evento ocurra en cada ensayo.
function resultados_binomiales(n, p)
    resultados = []
    for k in 0:n
        resultados.push!([k, probabilidad_binomial(n, k, p)])
    end
    return resultados
end

# Ejemplo de uso de las funciones definidas anteriormente.
n = 10
k = 5
p = 0.5

# Calculamos el factorial de n.
println("Factorial de $n: $(factorial(n))")

# Calculamos la combinación de n elementos tomados de k en k.
println("Combinación de $n elementos tomados de $k en $k: $(combinacion(n, k))")

# Generamos una lista de todas las combinaciones posibles de n elementos tomados de k en k.
println("Combinaciones de $n elementos tomados de $k en $k:")
for combinacion in combinaciones(n, k)
    println("- $combinacion")
end

# Calculamos el número de combinaciones posibles de n elementos tomados de k en k.
println("Número de combinaciones posibles de $n elementos tomados de $k en $k: $(numero_combinaciones(n, k))")

# Calculamos la probabilidad de que un evento ocurra k veces en n ensayos, dada una probabilidad p de que el evento ocurra en cada ensayo.
println("Probabilidad de que un evento ocurra $k veces en $n ensayos, dada una probabilidad $p de que el evento ocurra en cada ensayo: $(probabilidad_binomial(n, k, p))")

# Generamos una lista de todos los posibles resultados de n ensayos, dada una probabilidad p de que el evento ocurra en cada ensayo.
println("Resultados binomiales de $n ensayos, dada una probabilidad $p de que el evento ocurra en cada ensayo:")
for resultado in resultados_binomiales(n, p)
    println("- $(resultado[1]) éxitos con probabilidad $(resultado[2])")
end
```

Explicación del código:

* La función `factorial` calcula el factorial de un número.
* La función `combinacion` calcula la combinación de n elementos tomados de k en k.
* La función `combinaciones` genera una lista de todas las posibles combinaciones de n elementos tomados de k en k.
* La función `numero_combinaciones` calcula el número de combinaciones posibles de n elementos tomados de k en k.
* La función `probabilidad_binomial` calcula la probabilidad de que un evento ocurra k veces en n ensayos, dada una probabilidad p de que el evento ocurra en cada ensayo.
* La función `resultados_binomiales` genera una lista de todos los posibles resultados de n ensayos, dada una probabilidad p de que el evento ocurra en cada ensayo.

El ejemplo de uso del código muestra cómo usar las funciones definidas anteriormente para calcular el factorial de un número, la combinación de n elementos tomados de k en k, el número de combinaciones posibles de n elementos tomados de k en k, la probabilidad de que un evento ocurra k veces en n ensayos, dada una probabilidad p de que el evento ocurra en cada ensayo, y una lista de todos los posibles resultados de n ensayos, dada una probabilidad p de que el evento ocurra en cada ensayo.