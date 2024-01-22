```visual basic
' Este código crea una interfaz gráfica de usuario básica con Visual Basic.

' Crear un nuevo proyecto de Visual Basic.
' Seleccionar "Formulario estándar" como tipo de proyecto.

' Agregar los siguientes controles al formulario:
' - Un botón de comando llamado "Calcular".
' - Un cuadro de texto llamado "txtNúmero1".
' - Un cuadro de texto llamado "txtNúmero2".
' - Una etiqueta llamada "lblResultado".

' Declarar las variables necesarias.
Dim número1 As Double
Dim número2 As Double
Dim resultado As Double

' Agregar el siguiente código al evento "Click" del botón de comando "Calcular":

Private Sub btnCalcular_Click()
    ' Obtener los valores de los cuadros de texto.
    número1 = Val(txtNúmero1.Text)
    número2 = Val(txtNúmero2.Text)

    ' Calcular el resultado.
    resultado = número1 + número2

    ' Mostrar el resultado en la etiqueta.
    lblResultado.Caption = resultado
End Sub

' Ejecutar el proyecto.

' Explicación del código:

' El código crea una interfaz gráfica de usuario básica con Visual Basic.

' La interfaz gráfica de usuario consta de los siguientes controles:
' - Un botón de comando llamado "Calcular".
' - Un cuadro de texto llamado "txtNúmero1".
' - Un cuadro de texto llamado "txtNúmero2".
' - Una etiqueta llamada "lblResultado".

' El código también declara las variables necesarias para almacenar los valores de los cuadros de texto y el resultado del cálculo.

' El código agrega el siguiente código al evento "Click" del botón de comando "Calcular":

' Este código obtiene los valores de los cuadros de texto.
' Calcula el resultado.
' Muestra el resultado en la etiqueta.

' El código ejecuta el proyecto.
```