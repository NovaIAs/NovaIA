```r
# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  id = 1:10,
  nombre = c("Juan", "María", "Pedro", "Ana", "José", "Rosa", "Luis", "Carmen", "Antonio", "Magdalena"),
  edad = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
  sexo = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F"),
  ciudad = c("Madrid", "Barcelona", "Valencia", "Sevilla", "Málaga", "Murcia", "Alicante", "Bilbao", "San Sebastián", "Vitoria")
)

# Imprimir el conjunto de datos
print(datos)

# Crear un modelo de regresión lineal para predecir la edad en función del sexo y la ciudad
modelo <- lm(edad ~ sexo + ciudad, data = datos)

# Obtener el resumen del modelo
summary(modelo)

# Crear una gráfica de dispersión de la edad en función del sexo
ggplot(datos, aes(x = sexo, y = edad)) +
  geom_point() +
  geom_smooth(method = "lm")

# Crear una gráfica de barras de la edad por ciudad
ggplot(datos, aes(x = ciudad, y = edad)) +
  geom_bar(stat = "identity")

# Crear una tabla de frecuencias del sexo
table(datos$sexo)

# Crear una tabla de frecuencias de la ciudad
table(datos$ciudad)

# Crear una tabla de contingencia del sexo y la ciudad
table(datos$sexo, datos$ciudad)

# Crear una correlación de Pearson entre la edad y el sexo
cor(datos$edad, datos$sexo, method = "pearson")

# Crear una correlación de Spearman entre la edad y el sexo
cor(datos$edad, datos$sexo, method = "spearman")

# Crear una correlación de Kendall entre la edad y el sexo
cor(datos$edad, datos$sexo, method = "kendall")

# Crear una regresión logística para predecir el sexo en función de la edad y la ciudad
modelo <- glm(sexo ~ edad + ciudad, data = datos, family = "binomial")

# Obtener el resumen del modelo
summary(modelo)
```