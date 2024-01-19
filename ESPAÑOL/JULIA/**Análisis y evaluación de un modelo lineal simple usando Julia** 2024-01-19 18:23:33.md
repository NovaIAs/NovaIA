```julia

# Importar módulos necesarios
using Distributions, StatsBase, Plots

# Generar datos
x = randn(1000)
y = 2 * x + 3 + randn(1000) * 0.1

# Ajustar un modelo lineal
model = fit(LinearModel(), x, y)

# Obtener predicciones y residuos
predictions = predict(model, x)
residuals = y - predictions

# Calcular estadísticas del modelo
r2 = r2(model)
rmse = rmse(model)
mae = mae(model)

# Imprimir estadísticas del modelo
println("Coeficiente de determinación (R^2): $(r2)")
println("Error cuadrático medio (RMSE): $(rmse)")
println("Error medio absoluto (MAE): $(mae)")

# Crear una gráfica de dispersión con la línea de ajuste
plot(x, y, label="Datos")
plot!(x, predictions, label="Ajuste lineal")
legend!()

```

Explicación del código:

1. **Importar módulos necesarios**

```julia
using Distributions, StatsBase, Plots
```

Esta línea importa los módulos `Distributions`, `StatsBase` y `Plots`, que se utilizarán para generar datos, ajustar un modelo lineal y crear una gráfica de dispersión.

2. **Generar datos**

```julia
x = randn(1000)
y = 2 * x + 3 + randn(1000) * 0.1
```

Estas líneas generan dos vectores de datos: `x` e `y`. El vector `x` contiene 1000 números aleatorios generados a partir de una distribución normal estándar. El vector `y` contiene 1000 valores generados a partir de la ecuación `y = 2 * x + 3` con un poco de ruido aleatorio añadido.

3. **Ajustar un modelo lineal**

```julia
model = fit(LinearModel(), x, y)
```

Esta línea ajusta un modelo lineal a los datos. El modelo lineal es un modelo estadístico que intenta encontrar una línea recta que se ajuste mejor a los datos.

4. **Obtener predicciones y residuos**

```julia
predictions = predict(model, x)
residuals = y - predictions
```

Estas líneas obtienen las predicciones del modelo para los datos de `x` y los residuos del modelo. Los residuos son la diferencia entre los valores observados de `y` y las predicciones del modelo.

5. **Calcular estadísticas del modelo**

```julia
r2 = r2(model)
rmse = rmse(model)
mae = mae(model)
```

Estas líneas calculan tres estadísticas del modelo: el coeficiente de determinación (R^2), el error cuadrático medio (RMSE) y el error medio absoluto (MAE). El R^2 es un valor entre 0 y 1 que indica qué tan bien el modelo se ajusta a los datos. El RMSE y el MAE son medidas del error del modelo.

6. **Imprimir estadísticas del modelo**

```julia
println("Coeficiente de determinación (R^2): $(r2)")
println("Error cuadrático medio (RMSE): $(rmse)")
println("Error medio absoluto (MAE): $(mae)")
```

Estas líneas imprimen las estadísticas del modelo en la consola.

7. **Crear una gráfica de dispersión con la línea de ajuste**

```julia
plot(x, y, label="Datos")
plot!(x, predictions, label="Ajuste lineal")
legend!()
```

Estas líneas crean una gráfica de dispersión de los datos, con la línea de ajuste superpuesta. La función `plot()` crea la gráfica de dispersión, mientras que la función `plot!()` añade la línea de ajuste. La función `legend!()` añade una leyenda a la gráfica.