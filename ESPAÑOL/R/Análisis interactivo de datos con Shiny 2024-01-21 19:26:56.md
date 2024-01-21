```r

# Importar bibliotecas
biblioteca(shiny)
biblioteca(ggplot2)
biblioteca(plotly)
biblioteca(shinyWidgets)
biblioteca(shinythemes)

# Crear una interfaz de usuario

ui <- shinyUI(
  fluidPage(
    titlePanel("Análisis de datos interactivo"),
    sidebarPanel(
      selectInput("variable",
                   "Variable a analizar",
                   choices = c("Ventas", "Gastos", "Beneficio")),
      sliderInput("anio",
                   "Año a analizar",
                   min = 2015,
                   max = 2022,
                   value = 2021),
      radioButtons("tipo_grafico",
                    "Tipo de gráfico",
                    choices = c("Barras", "Línea", "Dispersión")),
      checkboxInput("mostrar_datos", "Mostrar datos")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Crear un servidor

server <- shinyServer(function(input, output) {

  # Cargar el conjunto de datos
  datos <- read.csv("datos.csv")

  # Subconjunto de datos
  datos_filtrados <- subset(datos, year == input$anio)

  # Crear gráfico
  plot <-ggplot(datos_filtrados, aes(x = variable, y = valor)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Gráfico de barras",
         x = "Variable",
         y = "Valor")

  if (input$tipo_grafico == "Línea") {
    plot <- plot + geom_line(aes(group = 1))
  } else if (input$tipo_grafico == "Dispersión") {
    plot <- plot + geom_point(aes(color = factor(year)))
  }

  # Crear tabla
  table <- datos_filtrados

  # Generar salidas
  output$plot <- renderPlotly(plot)
  output$table <- renderTable(table)
})

# Ejecutar la aplicación
shinyApp(ui, server)

```

Este código crea una aplicación Shiny que permite al usuario analizar datos de forma interactiva. El usuario puede seleccionar la variable a analizar, el año a analizar y el tipo de gráfico que desea ver. La aplicación también muestra una tabla con los datos.

El código está organizado en dos partes: la interfaz de usuario (UI) y el servidor. La UI define el diseño de la aplicación, mientras que el servidor define la lógica de la aplicación.

La interfaz de usuario se crea utilizando la función `shinyUI()`. Esta función toma un argumento `fluidPage()` que define el diseño de la aplicación. La aplicación tiene dos paneles: un panel lateral y un panel principal. El panel lateral contiene los controles de entrada, mientras que el panel principal contiene las salidas.

El servidor se crea utilizando la función `shinyServer()`. Esta función toma dos argumentos: `input` y `output`. El argumento `input` contiene los valores de los controles de entrada, mientras que el argumento `output` contiene las salidas.

El servidor utiliza la función `subset()` para subconjuntar los datos en función del año seleccionado por el usuario. A continuación, utiliza la función `ggplot()` para crear un gráfico de barras de la variable seleccionada por el usuario. Si el usuario selecciona "Línea" o "Dispersión" como tipo de gráfico, el servidor modifica el gráfico en consecuencia.

El servidor también utiliza la función `renderPlotly()` para generar una salida de Plotly del gráfico. La función `renderTable()` se utiliza para generar una salida de tabla de los datos.

Finalmente, la función `shinyApp()` se utiliza para ejecutar la aplicación. Esta función toma dos argumentos: `ui` y `server`. El argumento `ui` especifica la interfaz de usuario de la aplicación, mientras que el argumento `server` especifica el servidor de la aplicación.