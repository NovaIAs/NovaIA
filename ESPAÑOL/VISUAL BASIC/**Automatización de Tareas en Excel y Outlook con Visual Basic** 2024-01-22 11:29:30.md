```visual basic
' Crea un nuevo libro de Excel
Sub CrearLibroExcel()
    Dim oExcel As New Excel.Application
    oExcel.Visible = True
    oExcel.Workbooks.Add
End Sub

' Lee un archivo CSV y lo carga en una hoja de cálculo de Excel
Sub LeerCSV()
    Dim oExcel As New Excel.Application
    oExcel.Visible = True
    Dim oLibro As Excel.Workbook = oExcel.Workbooks.Open("C:\ruta\a\archivo.csv")
    Dim oHoja As Excel.Worksheet = oLibro.Worksheets(1)

    ' Carga los datos del CSV en la hoja de cálculo
    Dim iFila As Integer = 1
    Dim iColumna As Integer = 1
    Dim sValor As Variant
    Do While Not oHoja.Cells(iFila, iColumna).Value = ""
        sValor = oHoja.Cells(iFila, iColumna).Value
        oHoja.Cells(iFila, iColumna).Value = sValor
        iColumna = iColumna + 1
    Loop
    iColumna = 1
    iFila = iFila + 1

    ' Da formato a la hoja de cálculo
    oHoja.Columns.AutoFit
    oHoja.Rows.AutoFit

    ' Guarda el libro de Excel
    oLibro.SaveAs "C:\ruta\a\archivo.xlsx"

    ' Cierra el libro de Excel
    oLibro.Close
    oExcel.Quit
End Sub

' Crea un gráfico en una hoja de cálculo de Excel
Sub CrearGrafico()
    Dim oExcel As New Excel.Application
    oExcel.Visible = True
    Dim oLibro As Excel.Workbook = oExcel.Workbooks.Open("C:\ruta\a\archivo.xlsx")
    Dim oHoja As Excel.Worksheet = oLibro.Worksheets(1)

    ' Selecciona los datos para el gráfico
    oHoja.Range("A1:D10").Select

    ' Crea el gráfico
    Dim oGrafico As Excel.Chart = oHoja.Shapes.AddChart.Chart

    ' Establece el tipo de gráfico
    oGrafico.ChartType = Excel.XlChartType.xlColumnClustered

    ' Establece el título del gráfico
    oGrafico.ChartTitle.Text = "Ventas por mes"

    ' Establece las etiquetas de los ejes
    oGrafico.Axes(Excel.XlAxisType.xlCategory).HasTitle = True
    oGrafico.Axes(Excel.XlAxisType.xlCategory).AxisTitle.Text = "Mes"
    oGrafico.Axes(Excel.XlAxisType.xlValue).HasTitle = True
    oGrafico.Axes(Excel.XlAxisType.xlValue).AxisTitle.Text = "Ventas"

    ' Guarda el libro de Excel
    oLibro.Save
    oLibro.Close
    oExcel.Quit
End Sub

' Envía un correo electrónico con un archivo adjunto
Sub EnviarCorreo()
    Dim oOutlook As Outlook.Application
    Set oOutlook = CreateObject("Outlook.Application")

    Dim oCorreo As Outlook.MailItem
    Set oCorreo = oOutlook.CreateItem(Outlook.OlItemType.olMailItem)

    ' Establece los destinatarios
    oCorreo.Recipients.Add "destinatario1@dominio.com"
    oCorreo.Recipients.Add "destinatario2@dominio.com"

    ' Establece el asunto
    oCorreo.Subject = "Informe de ventas"

    ' Establece el cuerpo del mensaje
    oCorreo.Body = "Hola,\n\nAdjunto el informe de ventas.\n\nSaludos,\n\n[Tu nombre]"

    ' Adjunta el archivo
    oCorreo.Attachments.Add "C:\ruta\a\archivo.xlsx"

    ' Envía el correo electrónico
    oCorreo.Send
End Sub
```

Este código es un ejemplo complejo de código en Visual Basic que realiza varias tareas:

* Crea un nuevo libro de Excel.
* Lee un archivo CSV y lo carga en una hoja de cálculo de Excel.
* Crea un gráfico en una hoja de cálculo de Excel.
* Envía un correo electrónico con un archivo adjunto.

El código está bien documentado y es fácil de seguir. Utiliza objetos de la biblioteca de objetos de Excel y Outlook para realizar tareas avanzadas.

Este código es un buen ejemplo de cómo se puede utilizar Visual Basic para automatizar tareas complejas.