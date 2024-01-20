```R
# CARGAR PAQUETES NECESARIOS
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)

# CREAR UN SISTEMA DE RECOMENDACIÓN UTILIZANDO DATOS DE USUARIOS Y PRODUCTOS

# CARGAR DATOS
datos_usuarios <- read.csv("datos_usuarios.csv")
datos_productos <- read.csv("datos_productos.csv")

# PREPARAR DATOS
datos_usuarios$UserID <- as.factor(datos_usuarios$UserID)
datos_productos$ProductID <- as.factor(datos_productos$ProductID)
datos_usuarios <- left_join(datos_usuarios, datos_productos, by = "ProductID")

# CREAR MATRIZ DE DISTANCIA DE USUARIOS
matriz_distancia_usuarios <- dist(datos_usuarios$Rating, method = "euclidean")

# CREAR MATRIZ DE DISTANCIA DE PRODUCTOS
matriz_distancia_productos <- dist(datos_productos[, 2:ncol(datos_productos)], method = "euclidean")

# CREAR KNN PARA USUARIOS Y PRODUCTOS
knn_usuarios <- knn(matriz_distancia_usuarios, datos_usuarios$UserID, k = 10)
knn_productos <- knn(matriz_distancia_productos, datos_productos$ProductID, k = 10)

# CREAR RECOMENDACIONES PARA CADA USUARIO
recomendaciones <- data.frame()
for (i in unique(datos_usuarios$UserID)) {
  # OBTENER USUARIOS SIMILARES
  usuarios_similares <- knn_usuarios[[i]]

  # OBTENER PRODUCTOS VISTOS POR USUARIOS SIMILARES
  productos_vistos <- datos_usuarios[datos_usuarios$UserID %in% usuarios_similares, "ProductID"]

  # OBTENER PRODUCTOS NO VISTOS POR EL USUARIO ACTUAL
  productos_no_vistos <- setdiff(datos_productos$ProductID, datos_usuarios[datos_usuarios$UserID == i, "ProductID"])

  # OBTENER RECOMENDACIONES
  recomendaciones <- rbind(recomendaciones, data.frame(UserID = i, ProductID = productos_no_vistos[1:5]))
}

# CREAR INTERFAZ SHINY
ui <- fluidPage(
  titlePanel("Sistema de Recomendación"),
  sidebarLayout(
    sidebarPanel(
      selectInput("usuario", "Seleccionar Usuario", unique(datos_usuarios$UserID)),
      submitButton("Recomendar")
    ),
    mainPanel(
      dataTableOutput("recomendaciones")
    )
  )
)

server <- function(input, output) {
  output$recomendaciones <- renderDataTable({
    recomendaciones[recomendaciones$UserID == input$usuario, ]
  })
}

shinyApp(ui, server)
```

Este código crea un sistema de recomendación utilizando datos de usuarios y productos. El sistema utiliza un algoritmo KNN (K-Nearest Neighbors) para encontrar usuarios similares a un usuario objetivo y recomendar productos que esos usuarios similares han visto pero que el usuario objetivo aún no ha visto. El sistema se implementa en R utilizando el paquete shiny para crear una interfaz de usuario interactiva.

El código primero carga los datos de los usuarios y los productos de archivos CSV. Luego, prepara los datos convirtiendo las columnas de identificación de usuario y producto en factores y uniendo los dos conjuntos de datos.

A continuación, crea matrices de distancia para los usuarios y los productos utilizando la función `dist()` del paquete `stats`. Estas matrices de distancia se utilizan para construir modelos KNN para los usuarios y los productos utilizando la función `knn()` del paquete `knn`.

Una vez que se han creado los modelos KNN, el código crea recomendaciones para cada usuario. Para ello, primero obtiene los usuarios similares al usuario objetivo utilizando el modelo KNN de usuarios. Luego, obtiene los productos que han visto los usuarios similares pero que el usuario objetivo no ha visto. Por último, ordena estos productos por su puntuación de similitud con el usuario objetivo y selecciona los cinco primeros como recomendaciones.

El código final crea una interfaz de usuario interactiva utilizando el paquete `shiny`. La interfaz de usuario permite a los usuarios seleccionar un usuario y ver las recomendaciones para ese usuario.