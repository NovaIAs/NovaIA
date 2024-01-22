```julia
using Plots, Statistics

# Definimos una función para generar datos aleatorios
function generar_datos(n, mu, sigma)
    return randn(n) * sigma + mu
end

# Generamos datos aleatorios
datos = generar_datos(1000, 0, 1)

# Calculamos la media y la desviación estándar
media = mean(datos)
desviacion_estandar = std(datos)

# Creamos un histograma de los datos
histogram(datos, bins=20, title="Histograma de datos aleatorios", xlabel="Valor", ylabel="Frecuencia")

# Realizamos una prueba t-student para comprobar si la media de los datos es igual a 0
t_test(datos, 0)

# Calculamos la correlación entre los datos y una variable externa
variable_externa = randn(1000)
correlacion = cor(datos, variable_externa)

# Creamos un modelo de regresión lineal para predecir los datos a partir de la variable externa
modelo = lm(datos, variable_externa)
summary(modelo)

# Hacemos una predicción de los datos a partir del modelo
predicciones = predict(modelo, variable_externa)

# Calculamos el error cuadrático medio de las predicciones
error_cuadratico_medio = mean((predicciones - datos)^2)

# Imprimimos los resultados
println("Media:", media)
println("Desviación estándar:", desviacion_estandar)
println("Correlación:", correlacion)
println("Error cuadrático medio:", error_cuadratico_medio)
```

Este código es un ejemplo de cómo utilizar la librería Statistics de Julia para realizar análisis estadísticos de datos aleatorios. El código genera datos aleatorios, calcula la media y la desviación estándar, crea un histograma de los datos, realiza una prueba t-student, calcula la correlación entre los datos y una variable externa, crea un modelo de regresión lineal para predecir los datos a partir de la variable externa, hace una predicción de los datos a partir del modelo y calcula el error cuadrático medio de las predicciones.