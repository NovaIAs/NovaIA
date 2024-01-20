```julia
# Definir una función que calcule el factorial de un número natural.
function factorial(n)
    if n == 0
        return 1
    else
        return n * factorial(n-1)
    end
end

# Definir una función que calcule la suma de los primeros n números naturales.
function suma_naturales(n)
    if n == 0
        return 0
    else
        return n + suma_naturales(n-1)
    end
end

# Definir una función que calcule la media de una lista de números.
function media(lista)
    suma = 0
    for x in lista
        suma += x
    end
    return suma / length(lista)
end

# Definir una función que calcule la desviación estándar de una lista de números.
function desviacion_estandar(lista)
    media_lista = media(lista)
    suma_cuadrados = 0
    for x in lista
        suma_cuadrados += (x - media_lista)^2
    end
    varianza = suma_cuadrados / (length(lista) - 1)
    return sqrt(varianza)
end

# Definir una función que calcule la correlación entre dos listas de números.
function correlacion(lista1, lista2)
    if length(lista1) != length(lista2)
        throw(ArgumentError("Las listas deben tener la misma longitud"))
    end
    media1 = media(lista1)
    media2 = media(lista2)
    suma_productos = 0
    for i in 1:length(lista1)
        suma_productos += (lista1[i] - media1) * (lista2[i] - media2)
    end
    varianza1 = desviacion_estandar(lista1)
    varianza2 = desviacion_estandar(lista2)
    return suma_productos / (varianza1 * varianza2)
end

# Definir una función que calcule la recta de regresión lineal de una lista de puntos.
function recta_regresion_lineal(puntos)
    x = [punto[1] for punto in puntos]
    y = [punto[2] for punto in puntos]
    media_x = media(x)
    media_y = media(y)
    suma_productos = 0
    suma_cuadrados = 0
    for i in 1:length(x)
        suma_productos += (x[i] - media_x) * (y[i] - media_y)
        suma_cuadrados += (x[i] - media_x)^2
    end
    pendiente = suma_productos / suma_cuadrados
    ordenada = media_y - pendiente * media_x
    return [pendiente, ordenada]
end

# Probar las funciones definidas anteriormente.
println("Factorial de 5: ", factorial(5))
println("Suma de los primeros 10 números naturales: ", suma_naturales(10))
lista = [1, 2, 3, 4, 5]
println("Media de la lista: ", media(lista))
println("Desviación estándar de la lista: ", desviacion_estandar(lista))
lista1 = [1, 2, 3, 4, 5]
lista2 = [2, 4, 6, 8, 10]
println("Correlación entre las dos listas: ", correlacion(lista1, lista2))
puntos = [(1, 2), (2, 4), (3, 6), (4, 8), (5, 10)]
println("Recta de regresión lineal de los puntos: ", recta_regresion_lineal(puntos))
```

Este código define una serie de funciones matemáticas comunes en JULIA, como el factorial, la suma de los primeros n números naturales, la media, la desviación estándar, la correlación y la recta de regresión lineal. Estas funciones se pueden utilizar para realizar cálculos matemáticos complejos de forma sencilla y eficiente.

El código también incluye ejemplos de cómo utilizar las funciones definidas anteriormente para resolver problemas matemáticos comunes. Por ejemplo, el siguiente código utiliza la función `factorial` para calcular el factorial de 5:

```julia
println("Factorial de 5: ", factorial(5))
```

El siguiente código utiliza la función `suma_naturales` para calcular la suma de los primeros 10 números naturales:

```julia
println("Suma de los primeros 10 números naturales: ", suma_naturales(10))
```

Y el siguiente código utiliza la función `media` para calcular la media de una lista de números:

```julia
lista = [1, 2, 3, 4, 5]
println("Media de la lista: ", media(lista))
```

El código también incluye ejemplos de cómo utilizar las funciones definidas anteriormente para resolver problemas matemáticos más complejos, como el cálculo de la desviación estándar, la correlación y la recta de regresión lineal.