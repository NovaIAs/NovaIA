```r
# Importar los datos
datos <- read.csv("datos.csv")

# Limpiar los datos
datos <- datos %>%
  drop_na() %>%
  mutate(edad = as.numeric(edad))

# Crear un modelo de regresión lineal
modelo <- lm(y ~ x1 + x2, data = datos)

# Evaluar el modelo
summary(modelo)

# Crear un gráfico de dispersión con la línea de regresión
ggplot(datos, aes(x = x1, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Crear un gráfico de residuos
ggplot(datos, aes(x = fitted(modelo), y = resid(modelo))) +
  geom_point()

# Crear un gráfico de residuos versus valores ajustados
ggplot(datos, aes(x = fitted(modelo), y = resid(modelo))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Crear un gráfico de residuos versus valores independientes
ggplot(datos, aes(x = x1, y = resid(modelo))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Crear un gráfico de residuos versus valores independientes
ggplot(datos, aes(x = x2, y = resid(modelo))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Crear un gráfico de residuos versus tiempo
ggplot(datos, aes(x = tiempo, y = resid(modelo))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Crear un gráfico de residuos versus factor
ggplot(datos, aes(x = factor, y = resid(modelo))) +
  geom_boxplot()

# Crear un gráfico de residuos versus factor
ggplot(datos, aes(x = factor, y = resid(modelo))) +
  geom_violin()

# Crear un gráfico de residuos versus factor
ggplot(datos, aes(x = factor, y = resid(modelo))) +
  geom_dotplot()

# Crear un gráfico de residuos versus factor
ggplot(datos, aes(x = factor, y = resid(modelo))) +
  geom_histogram()

# Crear un gráfico de residuos versus factor
ggplot(datos, aes(x = factor, y = resid(modelo))) +
  geom_density()
```

Este código crea un modelo de regresión lineal para los datos del archivo "datos.csv". Limpia los datos, crea el modelo, lo evalúa, crea varios gráficos de residuos y gráficos de residuos versus variables independientes, tiempo, factor y factor.