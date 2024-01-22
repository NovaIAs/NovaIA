```julia

# Definimos una función que recibe un vector de números y devuelve su media
function media(numeros)
    suma = 0
    for i in 1:length(numeros)
        suma += numeros[i]
    end
    return suma / length(numeros)
end

# Definimos una función que recibe dos vectores de números y devuelve su correlación
function correlación(x, y)
    media_x = media(x)
    media_y = media(y)
    covarianza = 0
    for i in 1:length(x)
        covarianza += (x[i] - media_x) * (y[i] - media_y)
    end
    varianza_x = 0
    for i in 1:length(x)
        varianza_x += (x[i] - media_x)^2
    end
    varianza_y = 0
    for i in 1:length(y)
        varianza_y += (y[i] - media_y)^2
    end
    return covarianza / sqrt(varianza_x * varianza_y)
end

# Generamos dos vectores de números aleatorios
x = randn(100)
y = randn(100)

# Calculamos la media y la correlación de los vectores
media_x = media(x)
media_y = media(y)
correlación_xy = correlación(x, y)

# Imprimimos los resultados
println("Media de x:", media_x)
println("Media de y:", media_y)
println("Correlación de x y y:", correlación_xy)

```

Explicación del código:

* La función `media` calcula la media de un vector de números. Para ello, suma todos los números del vector y divide la suma por el número de elementos del vector.
* La función `correlación` calcula la correlación de dos vectores de números. Para ello, calcula la covarianza de los vectores y la divide por la raíz cuadrada del producto de las varianzas de los vectores.
* La función `randn` genera un vector de números aleatorios con distribución normal.
* La función `println` imprime un mensaje en la consola.
* El código genera dos vectores de números aleatorios, calcula la media y la correlación de los vectores, e imprime los resultados en la consola.