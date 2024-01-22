```r
# Carga de datos de ingresos y gastos en formato CSV
datos <- read.csv("ingresos_gastos.csv")

# Creación de función para calcular el saldo acumulado
saldo_acumulado <- function(datos) {
  # Inicialización del saldo acumulado
  saldo <- 0
  
  # Creación de vector para almacenar el saldo acumulado
  saldo_acumulado <- c()
  
  # Iteración sobre las filas de datos
  for (i in 1:nrow(datos)) {
    # Cálculo del saldo acumulado para la fila actual
    saldo <- saldo + datos[i, "ingresos"] - datos[i, "gastos"]
    
    # Almacenamiento del saldo acumulado en el vector
    saldo_acumulado <- c(saldo_acumulado, saldo)
  }
  
  # Retorno del vector de saldo acumulado
  return(saldo_acumulado)
}

# Aplicación de la función de saldo acumulado a los datos
saldo_acumulado_datos <- saldo_acumulado(datos)

# Creación de gráfico de saldo acumulado
plot(saldo_acumulado_datos, type = "l", col = "blue", main = "Saldo acumulado")

# Ajuste de una línea de tendencia lineal al gráfico de saldo acumulado
modelo_lineal <- lm(saldo_acumulado_datos ~ seq(1:length(saldo_acumulado_datos)))
abline(modelo_lineal, col = "red")

# Cálculo de los coeficientes de la línea de tendencia lineal
coeficientes_lineales <- coef(modelo_lineal)

# Impresión de los coeficientes de la línea de tendencia lineal
print("Coeficientes de la línea de tendencia lineal:")
print(coeficientes_lineales)

# Predicción del saldo acumulado para los próximos 12 meses
prediccion <- predict(modelo_lineal, newdata = data.frame(seq(length(saldo_acumulado_datos) + 1, length(saldo_acumulado_datos) + 12)))

# Creación de gráfico de predicción de saldo acumulado
plot(prediccion, type = "l", col = "green", main = "Predicción de saldo acumulado")

# Adición de leyenda al gráfico de predicción de saldo acumulado
legend("topright", legend = c("Saldo acumulado real", "Línea de tendencia lineal", "Predicción de saldo acumulado"), col = c("blue", "red", "green"), lty = 1, lwd = 2)
```

Explicación del código:

1. Carga de datos: Se cargan los datos de ingresos y gastos desde un archivo CSV utilizando la función `read.csv()`.

2. Creación de función para calcular el saldo acumulado: Se define una función llamada `saldo_acumulado()` que calcula el saldo acumulado para un conjunto de datos de ingresos y gastos. La función itera sobre las filas de datos, calcula el saldo acumulado para cada fila y lo almacena en un vector.

3. Aplicación de la función de saldo acumulado a los datos: Se llama a la función `saldo_acumulado()` con los datos de ingresos y gastos como argumento para calcular el saldo acumulado.

4. Creación de gráfico de saldo acumulado: Se crea un gráfico de línea del saldo acumulado utilizando la función `plot()`.

5. Ajuste de una línea de tendencia lineal al gráfico de saldo acumulado: Se ajusta una línea de tendencia lineal al gráfico de saldo acumulado utilizando la función `lm()`. La línea de tendencia lineal se muestra en el gráfico como una línea roja.

6. Cálculo de los coeficientes de la línea de tendencia lineal: Se calculan los coeficientes de la línea de tendencia lineal utilizando la función `coef()`. Los coeficientes se imprimen en la consola.

7. Predicción del saldo acumulado para los próximos 12 meses: Se predice el saldo acumulado para los próximos 12 meses utilizando la función `predict()`. La predicción se muestra en el gráfico como una línea verde.

8. Creación de gráfico de predicción de saldo acumulado: Se crea un gráfico de línea de la predicción de saldo acumulado utilizando la función `plot()`.

9. Adición de leyenda al gráfico de predicción de saldo acumulado: Se añade una leyenda al gráfico de predicción de saldo acumulado utilizando la función `legend()`. La leyenda muestra los nombres de las diferentes líneas del gráfico.