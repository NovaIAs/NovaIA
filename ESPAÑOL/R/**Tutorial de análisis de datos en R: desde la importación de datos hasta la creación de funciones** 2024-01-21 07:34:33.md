```R
# Importar datos
df <- read.csv("datos.csv")

# Limpiar datos
df <- df %>%
  drop_na() %>%
  mutate(variable_numerica = as.numeric(variable_numerica))

# Explorar datos
summary(df)
plot(df$variable_numerica, df$variable_categorica)

# Crear modelo
modelo <- lm(variable_numerica ~ variable_categorica, data = df)

# Evaluar modelo
summary(modelo)
plot(df$variable_numerica, resid(modelo))

# Predecir valores
predicciones <- predict(modelo, newdata = df)

# Crear gráfico
ggplot(df, aes(x = variable_categorica, y = variable_numerica)) +
  geom_boxplot() +
  geom_point(aes(color = predicciones))

# Guardar resultados
save(modelo, file = "modelo.rds")

# Crear función
mi_funcion <- function(x) {
  x + 1
}

# Aplicar función
df$variable_nueva <- mi_funcion(df$variable_numerica)
```

Este código en R es bastante extenso y cubre una amplia gama de temas, como la importación y limpieza de datos, la exploración de datos, la creación y evaluación de modelos estadísticos, la predicción de valores, la creación de gráficos y la creación de funciones.

El código comienza importando los datos de un archivo CSV, luego limpia los datos eliminando las filas con valores faltantes y convirtiendo las variables numéricas a formato numérico. A continuación, explora los datos utilizando la función `summary()` y la función `plot()`.

Luego, crea un modelo lineal utilizando la función `lm()` y evalúa el modelo utilizando la función `summary()` y la función `plot()`. Utiliza el modelo para predecir valores utilizando la función `predict()`.

A continuación, crea un gráfico utilizando la biblioteca `ggplot2` y guarda los resultados del modelo utilizando la función `save()`. Finalmente, crea una función utilizando la función `function()` y la aplica a los datos utilizando la función `mutate()`.

Este código es bastante complejo y cubre una amplia gama de temas, pero puede ser útil para los usuarios de R que quieran aprender más sobre el análisis de datos y la creación de modelos estadísticos.