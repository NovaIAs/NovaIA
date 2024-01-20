```visual basic
Private Sub Complejo_Click()
    ' Declaración de variables.
    Dim nombre As String
    Dim apellido As String
    Dim edad As Integer
    Dim salario As Double
    Dim casado As Boolean
    Dim hijos As Integer

    ' Captura de datos del usuario.
    nombre = InputBox("Ingrese su nombre:", "Captura de datos")
    apellido = InputBox("Ingrese su apellido:", "Captura de datos")
    edad = InputBox("Ingrese su edad:", "Captura de datos")
    salario = InputBox("Ingrese su salario:", "Captura de datos")
    casado = InputBox("¿Está casado?", "Captura de datos")
    hijos = InputBox("¿Cuántos hijos tiene?", "Captura de datos")

    ' Conversión de datos.
    edad = CInt(edad)
    salario = CDbl(salario)
    casado = CBool(casado)
    hijos = CInt(hijos)

    ' Procesamiento de datos.
    Select Case edad
        Case Is < 18
            MsgBox("Es usted menor de edad.")
        Case Is >= 18 AndAlso Is < 65
            MsgBox("Es usted adulto.")
        Case Is >= 65
            MsgBox("Es usted jubilado.")
    End Select

    If casado Then
        MsgBox("Es usted casado.")
    Else
        MsgBox("Es usted soltero.")
    End If

    If hijos > 0 Then
        MsgBox("Tiene usted hijos.")
    Else
        MsgBox("No tiene usted hijos.")
    End If

    ' Muestra de datos.
    MsgBox("Nombre: " & nombre & vbCrLf & _
           "Apellido: " & apellido & vbCrLf & _
           "Edad: " & edad & vbCrLf & _
           "Salario: " & salario & vbCrLf & _
           "Casado: " & casado & vbCrLf & _
           "Hijos: " & hijos)
End Sub
```

Explicación del código:

* La primera línea del código crea un procedimiento llamado `Complejo_Click`. Este procedimiento se ejecutará cuando el usuario haga clic en un control de la ventana.
* La segunda línea del código declara una variable de tipo `String` llamada `nombre`. Esta variable se utilizará para almacenar el nombre del usuario.
* La tercera línea del código declara una variable de tipo `String` llamada `apellido`. Esta variable se utilizará para almacenar el apellido del usuario.
* La cuarta línea del código declara una variable de tipo `Integer` llamada `edad`. Esta variable se utilizará para almacenar la edad del usuario.
* La quinta línea del código declara una variable de tipo `Double` llamada `salario`. Esta variable se utilizará para almacenar el salario del usuario.
* La sexta línea del código declara una variable de tipo `Boolean` llamada `casado`. Esta variable se utilizará para almacenar el estado civil del usuario.
* La séptima línea del código declara una variable de tipo `Integer` llamada `hijos`. Esta variable se utilizará para almacenar el número de hijos del usuario.
* La octava línea del código captura el nombre del usuario y lo almacena en la variable `nombre`.
* La novena línea del código captura el apellido del usuario y lo almacena en la variable `apellido`.
* La décima línea del código captura la edad del usuario y la almacena en la variable `edad`.
* La undécima línea del código captura el salario del usuario y lo almacena en la variable `salario`.
* La duodécima línea del código captura el estado civil del usuario y lo almacena en la variable `casado`.
* La decimotercera línea del código captura el número de hijos del usuario y lo almacena en la variable `hijos`.
* La decimocuarta línea del código convierte la variable `edad` de tipo `String` a tipo `Integer`.
* La decimoquinta línea del código convierte la variable `salario` de tipo `String` a tipo `Double`.
* La decimosexta línea del código convierte la variable `casado` de tipo `String` a tipo `Boolean`.
* La decimoséptima línea del código convierte la variable `hijos` de tipo `String` a tipo `Integer`.
* La decimoctava línea del código utiliza la sentencia `Select Case` para procesar la variable `edad`.
* La decimonovena línea del código muestra un mensaje de error si el usuario es menor de edad.
* La vigésima línea del código muestra un mensaje de información si el usuario es adulto.
* La vigésima primera línea del código muestra un mensaje de información si el usuario es jubilado.
* La vigésima segunda línea del código utiliza la sentencia `If` para procesar la variable `casado`.
* La vigésima tercera línea del código muestra un mensaje de información si el usuario está casado.
* La vigésima cuarta línea del código muestra un mensaje de información si el usuario está soltero.
* La vigésima quinta línea del código utiliza la sentencia `If` para procesar la variable `hijos`.
* La vigésima sexta línea del código muestra un mensaje de información si el usuario tiene hijos.
* La vigésima séptima línea del código muestra un mensaje de información si el usuario no tiene hijos.
* La vigésima octava línea del código muestra los datos del usuario en un cuadro de mensaje.