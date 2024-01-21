```visual basic
' Crear una función que calcula el área de un triángulo
Function AreaTriangulo(base, altura)
    ' Calcular el área del triángulo
    area = (base * altura) / 2
    ' Devolver el área del triángulo
    AreaTriangulo = area
End Function

' Crear un procedimiento que muestra el área de un triángulo
Sub MostrarAreaTriangulo(base, altura)
    ' Calcular el área del triángulo
    area = AreaTriangulo(base, altura)
    ' Mostrar el área del triángulo
    MsgBox "El área del triángulo es " & area & " unidades cuadradas."
End Sub

' Crear un formulario con un botón y dos cuadros de texto
Form1.Caption = "Calculadora de área de triángulos"
Form1.Width = 200
Form1.Height = 150

' Crear un botón en el formulario
Button1.Caption = "Calcular"
Button1.Left = 20
Button1.Top = 20
Button1.Width = 80
Button1.Height = 20

' Crear dos cuadros de texto en el formulario
TextBox1.Left = 20
TextBox1.Top = 50
TextBox1.Width = 80
TextBox1.Height = 20
TextBox1.Text = "Base"

TextBox2.Left = 20
TextBox2.Top = 80
TextBox2.Width = 80
TextBox2.Height = 20
TextBox2.Text = "Altura"

' Agregar un evento Click al botón
Private Sub Button1_Click()
    ' Obtener los valores de los cuadros de texto
    base = Val(TextBox1.Text)
    altura = Val(TextBox2.Text)

    ' Llamar al procedimiento MostrarAreaTriangulo
    MostrarAreaTriangulo base, altura
End Sub
```

Este código crea una calculadora de área de triángulos en Visual Basic. La calculadora tiene un botón llamado "Calcular" y dos cuadros de texto, uno para la base del triángulo y otro para la altura del triángulo. Cuando el usuario hace clic en el botón "Calcular", la calculadora calcula el área del triángulo y lo muestra en un cuadro de mensaje.

El código está dividido en tres partes:

1. La primera parte define una función llamada `AreaTriangulo` que calcula el área de un triángulo. La función toma dos argumentos: la base del triángulo y la altura del triángulo. La función devuelve el área del triángulo.
2. La segunda parte define un procedimiento llamado `MostrarAreaTriangulo` que muestra el área de un triángulo. El procedimiento toma dos argumentos: la base del triángulo y la altura del triángulo. El procedimiento calcula el área del triángulo usando la función `AreaTriangulo` y luego muestra el área del triángulo en un cuadro de mensaje.
3. La tercera parte crea un formulario con un botón y dos cuadros de texto. El formulario también incluye un evento `Click` que se activa cuando el usuario hace clic en el botón "Calcular". El evento `Click` obtiene los valores de los cuadros de texto, llama al procedimiento `MostrarAreaTriangulo` y pasa los valores de los cuadros de texto como argumentos.

Cuando el usuario hace clic en el botón "Calcular", el evento `Click` se activa y se llama al procedimiento `MostrarAreaTriangulo`. El procedimiento `MostrarAreaTriangulo` calcula el área del triángulo usando la función `AreaTriangulo` y luego muestra el área del triángulo en un cuadro de mensaje.