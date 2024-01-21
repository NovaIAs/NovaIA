```julia
# Importar librerías necesarias
using Plots
using Statistics
using LinearAlgebra

# Definir una función para generar datos aleatorios
function generar_datos()
    # Crear un vector de 100 valores aleatorios entre 0 y 1
    x = rand(100)

    # Crear un vector de 100 valores aleatorios entre -1 y 1
    y = rand(100, -1, 1)

    # Devolver los vectores x e y
    return x, y
end

# Generar datos aleatorios
x, y = generar_datos()

# Crear un modelo de regresión lineal
modelo = LinearRegression()

# Entrenar el modelo con los datos
fit!(modelo, x, y)

# Hacer predicciones con el modelo
y_pred = predict(modelo, x)

# Calcular el error cuadrático medio
rmse = sqrt(mean((y_pred - y) .^ 2))

# Mostrar el error cuadrático medio
println("RMSE:", rmse)

# Crear una gráfica con los datos y las predicciones
plot(x, y, label="Datos")
plot!(x, y_pred, label="Predicciones")

# Mostrar la gráfica
show()
```

Este código genera datos aleatorios, crea un modelo de regresión lineal, lo entrena con los datos, hace predicciones con el modelo, calcula el error cuadrático medio y muestra una gráfica con los datos y las predicciones.

El código está explicado en detalle en los comentarios.