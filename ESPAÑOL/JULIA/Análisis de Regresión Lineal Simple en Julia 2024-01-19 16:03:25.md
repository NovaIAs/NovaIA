```julia

# Paquetes necesarios
using: LinearAlgebra, Plots, Statistics

# Genera datos aleatorios para una regresión lineal simple
datos = randn(100, 2)
datos[:, 1] .= 2 * datos[:, 1] + 3
y = datos[:, 2]
x = datos[:, 1:2]

# Ajusta un modelo lineal simple a los datos
modelo = lm(y ~ x, datos)

# Muestra el modelo ajustado
println("Modelo ajustado:")
println(modelo)

# Hace predicciones con el modelo
predicciones = predict(modelo, datos)

# Calcula el error cuadrático medio
ecm = mean((predicciones - y)^2)

# Imprime el ECM
println("ECM:")
println(ecm)

# Dibuja un gráfico disperso de los datos con la línea de regresión
plot(datos, legend=:false, title="Datos y línea de regresión")
plot!(x, predicciones, legend=:false, linecolor=:red)

# Dibuja un histograma residual
resid = residuals(modelo)
hist(resid, legend=:false, title="Histograma de residuos")

# Prueba el modelo con nuevos datos
datos_nuevos = randn(10, 2)
datos_nuevos[:, 1] .= 2 * datos_nuevos[:, 1] + 3
predicciones_nuevas = predict(modelo, datos_nuevos)

# Imprime las predicciones para los nuevos datos
println("Predicciones para nuevos datos:")
println(predicciones_nuevas)

```

Explicación del código:

1. Primero, necesitamos importar los paquetes necesarios para el análisis de regresión lineal, visualización y estadística.
2. A continuación, generamos datos aleatorios que representan una relación lineal simple entre dos variables independientes (x1 y x2) y una variable dependiente (y).
3. Ajustamos un modelo lineal simple a los datos utilizando la función "lm".
4. Mostramos el modelo ajustado utilizando la función "println".
5. Hacemos predicciones con el modelo utilizando la función "predict".
6. Calculamos el error cuadrático medio (ECM) de las predicciones utilizando la función "mean".
7. Imprimimos el ECM utilizando la función "println".
8. Dibujamos un gráfico disperso de los datos con la línea de regresión utilizando la función "plot".
9. Dibujamos un histograma residual de los residuos del modelo utilizando la función "hist".
10. Finalmente, probamos el modelo con nuevos datos y mostramos las predicciones para estos datos.