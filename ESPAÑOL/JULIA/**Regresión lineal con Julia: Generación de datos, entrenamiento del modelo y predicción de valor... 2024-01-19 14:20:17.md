```julia
# Librerías
using Plots, Statistics, LinearAlgebra, Calculus

# Definición de funciones
function genera_datos(n)
    x = rand(n, 3)
    y = rand(n, 1)
    return x, y
end

function entrenar_modelo(x, y)
    # Entrenamiento del modelo
    modelo = @linear_regress x y
    return modelo
end

function predecir_valores(x, modelo)
    # Predicción de valores
    y_pred = modelo(x)
    return y_pred
end

# Generación de datos
x, y = genera_datos(100)

# Entrenamiento del modelo
modelo = entrenar_modelo(x, y)

# Predicción de valores
y_pred = predecir_valores(x, modelo)

# Gráfico de datos y predicciones
plot(x, y, label="Datos reales")
plot!(x, y_pred, label="Predicciones")

```

**Explicación:**

* La función `genera_datos` genera datos aleatorios para el entrenamiento y la prueba del modelo.
* La función `entrenar_modelo` entrena un modelo de regresión lineal utilizando los datos proporcionados.
* La función `predecir_valores` utiliza el modelo entrenado para predecir los valores de un conjunto de datos nuevos.
* El código principal genera datos, entrena un modelo, predice valores y dibuja un gráfico que muestra los datos reales y las predicciones del modelo.