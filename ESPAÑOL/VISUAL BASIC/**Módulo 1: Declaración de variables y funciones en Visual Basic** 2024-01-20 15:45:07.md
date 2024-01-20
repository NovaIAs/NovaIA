**Módulo 1: Declaración de variables y procedimientos**

```visual basic
' Declaración de variables
Dim nombre As String
Dim apellido As String
Dim edad As Integer
Dim sueldo As Double

' Declaración de procedimientos
Sub Saludo(nombre As String, apellido As String)
    ' Mensaje de saludo
    MsgBox "Hola " & nombre & " " & apellido & "!"
End Sub

Function CalcularSueldo(edad As Integer, sueldo As Double) As Double
    ' Cálculo del sueldo con base en la edad y el sueldo base
    If edad >= 65 Then
        return sueldo * 1.20
    Else
        return sueldo
    End If
End Function
```

**Módulo 2: Formulario principal**

```visual basic
' Código del formulario principal

' Declaración de controles
Private txtNombre As TextBox
Private txtApellido As TextBox
Private txtEdad As TextBox
Private txtSueldo As TextBox
Private btnSaludo As Button
Private btnCalcularSueldo As Button

' Procedimiento de carga del formulario
Private Sub Form_Load()
    ' Inicialización de los controles
    txtNombre.Text = ""
    txtApellido.Text = ""
    txtEdad.Text = ""
    txtSueldo.Text = ""
End Sub

' Procedimiento del botón de saludo
Private Sub btnSaludo_Click()
    ' Llamada al procedimiento de saludo
    Saludo txtNombre.Text, txtApellido.Text
End Sub

' Procedimiento del botón de cálculo de sueldo
Private Sub btnCalcularSueldo_Click()
    ' Obtención de los valores de los controles
    Dim edad As Integer = Val(txtEdad.Text)
    Dim sueldo As Double = Val(txtSueldo.Text)

    ' Llamada a la función de cálculo de sueldo
    Dim nuevoSueldo As Double = CalcularSueldo(edad, sueldo)

    ' Actualización del control de sueldo
    txtSueldo.Text = nuevoSueldo
End Sub
```

**Explicación del código:**

* El código está dividido en dos módulos: el módulo 1 contiene las declaraciones de variables y procedimientos, mientras que el módulo 2 contiene el formulario principal.
* El módulo 1 declara las variables que se utilizarán en el formulario principal, así como los procedimientos que se utilizarán para saludar al usuario y calcular el sueldo.
* El formulario principal contiene los controles que se utilizarán para interactuar con el usuario, así como el código que se ejecutará cuando el usuario interactúe con los controles.
* Cuando el formulario se carga, se inicializan los controles con valores vacíos.
* Cuando el usuario hace clic en el botón de saludo, se llama al procedimiento de saludo, que muestra un mensaje de saludo al usuario.
* Cuando el usuario hace clic en el botón de cálculo de sueldo, se obtienen los valores de los controles de edad y sueldo, se llama a la función de cálculo de sueldo y se actualiza el control de sueldo con el nuevo valor calculado.