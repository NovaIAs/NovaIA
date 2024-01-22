```r
# Carga los datos
datos <- read.csv("datos.csv")

# Crea un modelo de regresión lineal
modelo <- lm(y ~ x1 + x2, data = datos)

# Imprime el resumen del modelo
summary(modelo)

# Crea un gráfico de dispersión de los datos
plot(x1, y, main = "Relación entre x1 e y", xlab = "x1", ylab = "y")
points(x1, fitted(modelo), col = "red", pch = 16)

# Crea una tabla con los coeficientes del modelo
coefs <- coef(modelo)
tabla_coefs <- data.frame(coeficiente = names(coefs), valor = coefs)

# Imprime la tabla de coeficientes
print(tabla_coefs)

# Crea un gráfico de residuos del modelo
plot(modelo$resid, main = "Residuos del modelo", xlab = "Índice", ylab = "Residuo")
abline(h = 0, col = "red")

# Crea una tabla con los valores ajustados y los residuos
tabla_ajustes <- data.frame(y_ajustado = fitted(modelo), residuo = modelo$resid)

# Imprime la tabla de valores ajustados y residuos
print(tabla_ajustes)

# Crea un gráfico de diagnóstico del modelo
plot(modelo, which = 1)

# Crea una tabla con los valores de diagnóstico del modelo
tabla_diagnostico <- data.frame(
  variable = c("R-cuadrado", "Suma de cuadrados de residuos", "Suma de cuadrados total", "Varianza residual"),
  valor = c(modelo$r.squared, modelo$rss, modelo$tss, modelo$sigma^2)
)

# Imprime la tabla de valores de diagnóstico del modelo
print(tabla_diagnostico)

# Crea una función para predecir el valor de y a partir de un valor de x1 y x2
predict_y <- function(x1, x2) {
  modelo$coefficients[1] + modelo$coefficients[2] * x1 + modelo$coefficients[3] * x2
}

# Predice el valor de y para algunos valores de x1 y x2
predicciones <- predict_y(x1 = c(1, 2, 3), x2 = c(4, 5, 6))

# Imprime las predicciones
print(predicciones)
```

Este código es un ejemplo de cómo utilizar la función `lm()` de R para crear un modelo de regresión lineal. El código carga los datos, crea el modelo, imprime el resumen del modelo, crea un gráfico de dispersión de los datos, crea una tabla con los coeficientes del modelo, crea un gráfico de residuos del modelo, crea una tabla con los valores ajustados y los residuos, crea un gráfico de diagnóstico del modelo, crea una tabla con los valores de diagnóstico del modelo, crea una función para predecir el valor de y a partir de un valor de x1 y x2, y predice el valor de y para algunos valores de x1 y x2.