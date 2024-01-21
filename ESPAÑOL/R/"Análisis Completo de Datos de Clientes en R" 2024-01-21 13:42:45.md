```
# Crear un marco de datos con datos de clientes
clientes <- data.frame(
  id_cliente = 1:100,
  nombre_cliente = c(
    "Juan Pérez", "María Martínez", "Pedro García", "Ana López", "Luis Rodríguez",
    "Carolina Herrera", "José Sánchez", "Paula Ortiz", "Miguel Gómez", "Sandra Fernández"
  ),
  edad_cliente = sample(20:60, 100, replace = TRUE),
  sexo_cliente = sample(c("Masculino", "Femenino"), 100, replace = TRUE),
  ciudad_cliente = c(
    "Madrid", "Barcelona", "Valencia", "Sevilla", "Zaragoza",
    "Málaga", "Murcia", "Palma de Mallorca", "Las Palmas de Gran Canaria", "Santa Cruz de Tenerife"
  ),
  saldo_cliente = sample(1000:5000, 100, replace = TRUE)
)

# Crear un modelo de regresión lineal para predecir el saldo del cliente en función de su edad, sexo y ciudad
modelo <- lm(saldo_cliente ~ edad_cliente + sexo_cliente + ciudad_cliente, data = clientes)

# Evaluar el modelo y obtener las métricas de rendimiento
summary(modelo)

# Crear un gráfico para visualizar la relación entre el saldo del cliente y su edad, sexo y ciudad
ggplot(clientes, aes(x = edad_cliente, y = saldo_cliente, color = sexo_cliente, shape = ciudad_cliente)) +
  geom_point() +
  geom_smooth(method = "lm")

# Crear una tabla dinámica para resumir los datos de los clientes por ciudad
clientes_por_ciudad <- ddply(clientes, .(ciudad_cliente), summarize(
  saldo_medio_cliente = mean(saldo_cliente),
  saldo_maximo_cliente = max(saldo_cliente),
  saldo_minimo_cliente = min(saldo_cliente)
))

# Crear un gráfico de barras para visualizar el saldo medio del cliente por ciudad
ggplot(clientes_por_ciudad, aes(x = ciudad_cliente, y = saldo_medio_cliente)) +
  geom_bar(stat = "identity")
```

Este código realiza un análisis completo de los datos de los clientes, incluyendo la creación de un modelo de regresión lineal para predecir el saldo del cliente, la evaluación del modelo y la visualización de los datos de los clientes utilizando gráficos y tablas dinámicas.