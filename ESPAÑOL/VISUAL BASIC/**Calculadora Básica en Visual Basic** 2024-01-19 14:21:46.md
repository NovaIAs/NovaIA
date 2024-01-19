```visual basic
' Este código crea una aplicación sencilla de calculadora en Visual Basic que permite realizar operaciones básicas aritméticas.

' Definir la clase calculadora
Public Class Calculadora

    ' Definir las variables privadas
    Private iResultado As Decimal = 0
    Private bOperacionEnCurso As Boolean = False
    Private sOperadorActual As String = ""

    ' Definir los métodos públicos
    Public Sub Sumar(ByVal numero As Decimal)
        RealizarOperacion("+", numero)
    End Sub

    Public Sub Restar(ByVal numero As Decimal)
        RealizarOperacion("-", numero)
    End Sub

    Public Sub Multiplicar(ByVal numero As Decimal)
        RealizarOperacion("*", numero)
    End Sub

    Public Sub Dividir(ByVal numero As Decimal)
        RealizarOperacion("/", numero)
    End Sub

    Public Sub Igual()
        Si bOperacionEnCurso entonces
            RealizarCalculo()
            bOperacionEnCurso = False
        End If
    End Sub

    Public Sub Borrar()
        iResultado = 0
        sOperadorActual = ""
        bOperacionEnCurso = False
    End Sub

    ' Definir los métodos privados
    Private Sub RealizarOperacion(ByVal operador As String, ByVal numero As Decimal)
        Si bOperacionEnCurso entonces
            RealizarCalculo()
        End If

        iResultado = numero
        sOperadorActual = operador
        bOperacionEnCurso = True
    End Sub

    Private Sub RealizarCalculo()
        Selecciona Caso sOperadorActual
            Case "+"
                iResultado += numero
            Case "-"
                iResultado -= numero
            Case "*"
                iResultado *= numero
            Case "/"
                Si numero <> 0 entonces
                    iResultado /= numero
                End If
        End Select
    End Sub

End Class

' Definir el formulario principal
Public Class FormularioPrincipal

    ' Definir las variables privadas
    Private calculadora As New Calculadora

    ' Definir los componentes de la interfaz gráfica de usuario
    Private txtPantalla As TextBox
    Private btnSumar As Button
    Private btnRestar As Button
    Private btnMultiplicar As Button
    Private btnDividir As Button
    Private btnIgual As Button
    Private btnBorrar As Button
    Private btn0 As Button
    Private btn1 As Button
    Private btn2 As Button
    Private btn3 As Button
    Private btn4 As Button
    Private btn5 As Button
    Private btn6 As Button
    Private btn7 As Button
    Private btn8 As Button
    Private btn9 As Button
    Private btnComa As Button

    ' Definir el evento Load del formulario
    Private Sub FormularioPrincipal_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Inicializar la calculadora
        calculadora.Resultado = 0
    End Sub

    ' Definir el evento Click del botón Sumar
    Private Sub BtnSumar_Click(sender As Object, e As EventArgs) Handles btnSumar.Click
        ' Sumar el número a la calculadora
        calculadora.Sumar(CDbl(txtPantalla.Text))

        ' Actualizar la pantalla
        txtPantalla.Text = calculadora.Resultado.ToString()
    End Sub

    ' Definir el evento Click del botón Restar
    Private Sub BtnRestar_Click(sender As Object, e As EventArgs) Handles btnRestar.Click
        ' Restar el número a la calculadora
        calculadora.Restar(CDbl(txtPantalla.Text))

        ' Actualizar la pantalla
        txtPantalla.Text = calculadora.Resultado.ToString()
    End Sub

    ' Definir el evento Click del botón Multiplicar
    Private Sub BtnMultiplicar_Click(sender As Object, e As EventArgs) Handles btnMultiplicar.Click
        ' Multiplicar el número a la calculadora
        calculadora.Multiplicar(CDbl(txtPantalla.Text))

        ' Actualizar la pantalla
        txtPantalla.Text = calculadora.Resultado.ToString()
    End Sub

    ' Definir el evento Click del botón Dividir
    Private Sub BtnDividir_Click(sender As Object, e As EventArgs) Handles btnDividir.Click
        ' Dividir el número a la calculadora
        calculadora.Dividir(CDbl(txtPantalla.Text))

        ' Actualizar la pantalla
        txtPantalla.Text = calculadora.Resultado.ToString()
    End Sub

    ' Definir el evento Click del botón Igual
    Private Sub BtnIgual_Click(sender As Object, e As EventArgs) Handles btnIgual.Click
        ' Realizar el cálculo
        calculadora.Igual()

        ' Actualizar la pantalla
        txtPantalla.Text = calculadora.Resultado.ToString()
    End Sub

    ' Definir el evento Click del botón Borrar
    Private Sub BtnBorrar_Click(sender As Object, e As EventArgs) Handles btnBorrar.Click
        ' Borrar la calculadora
        calculadora.Borrar()

        ' Actualizar la pantalla
        txtPantalla.Text = "0"
    End Sub

    ' Definir los eventos Click de los botones numéricos
    Private Sub Btn0_Click(sender As Object, e As EventArgs) Handles btn0.Click
        ' Agregar el número 0 a la pantalla
        Si txtPantalla.Text <> "0" entonces
            txtPantalla.Text += "0"
        End If
    End Sub

    Private Sub Btn1_Click(sender As Object, e As EventArgs) Handles btn1.Click
        ' Agregar el número 1 a la pantalla
        Si txtPantalla.Text <> "0" entonces
            txtPantalla.Text += "1"
        Else
            txtPantalla.Text = "1"
        End If
    End Sub

    Private Sub Btn2_Click(sender As Object, e As EventArgs) Handles btn2.Click
        ' Agregar el número 2 a la pantalla
        Si txtPantalla.Text <> "0" then
            txtPantalla.Text += "2"
        Else
            txtPantalla.Text = "2"
        End If
    End Sub

    Private Sub Btn3_Click(sender As Object, e As EventArgs) Handles btn3.Click
        ' Agregar el número 3 a la pantalla
        Si txtPantalla.Text <> "0" then
            txtPantalla.Text += "3"
        Else
            txtPantalla.Text = "3"
        End If
    End Sub

    Private Sub Btn4_Click(sender As Object, e As EventArgs) Handles btn4.Click
        ' Agregar el número 4 a la pantalla
        Si txtPantalla.Text <> "0" then
            txtPantalla.Text += "4"
        Else
            txtPantalla.Text = "4"
        End If
    End Sub

    Private Sub Btn5_Click(sender As Object, e As EventArgs) Handles btn5.Click
        ' Agregar el número 5 a la pantalla
        Si txtPantalla.Text <> "0" then
            txtPantalla.Text += "5"
        Else
            txtPantalla.Text = "5"
        End If
    End Sub

    Private Sub Btn6_Click(sender As Object, e As EventArgs) Handles btn6.Click
        ' Agregar el número 6 a la pantalla
        Si txtPantalla.Text <> "0" then
            txtPantalla.Text += "6"
        Else
            txtPantalla.Text = "6"
        End If
    End Sub

    Private Sub Btn7_Click(sender As Object, e As EventArgs) Handles btn7.Click
        ' Agregar el número 7 a la pantalla
        Si txtPantalla.Text <> "0" then
            txtPantalla.Text += "7"
        Else
            txtPantalla.Text = "7"
        End If
    End Sub

    Private Sub Btn8_Click(sender As Object, e As EventArgs) Handles btn8.Click
        ' Agregar el número 8 a la pantalla
        Si txtPantalla.Text <> "0" then
            txtPantalla.Text += "8"
        Else
            txtPantalla.Text = "8"
        End If
    End Sub

    Private Sub Btn9_Click(sender As Object, e As EventArgs) Handles btn9.Click
        ' Agregar el número 9 a la pantalla
        Si txtPantalla.Text <> "0" then
            txtPantalla.Text += "9"
        Else
            txtPantalla.Text = "9"
        End If
    End Sub

    ' Definir el evento Click del botón Coma
    Private Sub BtnComa_Click(sender As Object, e As EventArgs) Handles btnComa.Click
        ' Agregar la coma a la pantalla
        Si txtPantalla.Text.Contains(".") = False then
            txtPantalla.Text += ","
        End If
    End Sub

End Class
```

**Explicación:**

Este código crea una aplicación sencilla de calculadora en Visual Basic que permite realizar operaciones básicas aritméticas. La aplicación está formada por una clase `Calculadora` que se encarga de realizar los cálculos y una clase `FormularioPrincipal` que se encarga de la interfaz gráfica de usuario.

La clase `Calculadora` tiene tres variables privadas: `iResultado`, `bOperacionEnCurso` y `sOperadorActual`. La variable `iResultado` almacena el resultado de la operación actual, la variable `bOperacionEnCurso` indica si hay una operación en curso y la variable `sOperadorActual` almacena el operador de la operación actual.

La clase `Calculadora` también tiene cuatro métodos públicos: `Sumar`, `Restar`, `Multiplicar` y `Dividir`. Estos métodos realizan las operaciones básicas aritméticas y actualizan el resultado de la operación.

La clase `Calculadora` también tiene un método público llamado `Igual` que realiza el cálculo de la operación actual y actualiza el resultado de la operación.

La clase `Calculadora` también tiene un método público llamado `Borrar` que borra el resultado de la operación y el operador de la operación actual.

La clase `FormularioPrincipal` tiene una variable privada llamada `calculadora` que es una instancia de la clase `Calculadora`.

La clase `FormularioPrincipal` también tiene varios componentes de la interfaz gráfica de usuario, como un cuadro de texto para mostrar el resultado de la operación, botones para realizar las operaciones básicas aritméticas, botones para el número 0 a 9 y un botón para la coma.

Los eventos `Click` de los botones de las operaciones básicas aritméticas llaman a los métodos correspondientes de la clase `Calculadora` para realizar las operaciones.

El evento `Click` del botón `Igual` llama al método `Igual` de la clase `Calculadora` para realizar el cálculo de la operación actual.

El evento `Click` del botón `Borrar` llama al método `Borrar` de la clase `Calculadora` para borrar el resultado de la operación y el operador de la operación actual.

Los eventos `Click` de los botones numéricos agregan el número correspondiente a la pantalla.

El evento `Click` del botón `Coma` agrega la coma a la pantalla.