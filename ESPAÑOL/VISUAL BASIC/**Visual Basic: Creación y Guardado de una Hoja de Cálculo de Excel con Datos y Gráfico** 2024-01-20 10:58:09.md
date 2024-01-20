```visual basic
' Este código crea una hoja de cálculo de Excel, la rellena con datos y luego la guarda.
' También crea un gráfico a partir de los datos de la hoja de cálculo y lo guarda como un archivo de imagen.

' Crear una nueva hoja de cálculo de Excel
Application.Workbooks.Add

' Establecer el nombre de la hoja de cálculo
ActiveSheet.Name = "Datos"

' Rellenar la hoja de cálculo con datos
Range("A1").Value = "Mes"
Range("B1").Value = "Ventas"
Range("A2").Value = "Enero"
Range("B2").Value = 1000
Range("A3").Value = "Febrero"
Range("B3").Value = 1500
Range("A4").Value = "Marzo"
Range("B4").Value = 2000

' Crear un gráfico a partir de los datos de la hoja de cálculo
ActiveSheet.Shapes.AddChart(xlColumnClustered).Select
ActiveChart.ChartType = xlColumnClustered
ActiveChart.SetSourceData Source:=Range("A1:B4")

' Guardar la hoja de cálculo
ActiveWorkbook.SaveAs Filename:="C:\Users\Juan\Desktop\Datos.xlsx"

' Guardar el gráfico como un archivo de imagen
ActiveChart.Export Filename:="C:\Users\Juan\Desktop\Gráfico.png", FilterName:="PNG"
```

Explicación del código:

1. `Application.Workbooks.Add` crea una nueva hoja de cálculo de Excel.
2. `ActiveSheet.Name = "Datos"` establece el nombre de la hoja de cálculo como "Datos".
3. `Range("A1").Value = "Mes"` y `Range("B1").Value = "Ventas"` establecen los encabezados de las columnas "Mes" y "Ventas" en la primera fila de la hoja de cálculo.
4. `Range("A2").Value = "Enero"`, `Range("B2").Value = 1000`, `Range("A3").Value = "Febrero"`, `Range("B3").Value = 1500`, `Range("A4").Value = "Marzo"`, y `Range("B4").Value = 2000` rellenan la hoja de cálculo con datos.
5. `ActiveSheet.Shapes.AddChart(xlColumnClustered).Select` crea un nuevo gráfico de columnas en la hoja de cálculo.
6. `ActiveChart.ChartType = xlColumnClustered` establece el tipo de gráfico como un gráfico de columnas.
7. `ActiveChart.SetSourceData Source:=Range("A1:B4")` establece los datos de origen del gráfico como el rango de celdas "A1:B4" de la hoja de cálculo.
8. `ActiveWorkbook.SaveAs Filename:="C:\Users\Juan\Desktop\Datos.xlsx"` guarda la hoja de cálculo en el escritorio del usuario con el nombre "Datos.xlsx".
9. `ActiveChart.Export Filename:="C:\Users\Juan\Desktop\Gráfico.png", FilterName:="PNG"` exporta el gráfico como un archivo de imagen PNG en el escritorio del usuario con el nombre "Gráfico.png".