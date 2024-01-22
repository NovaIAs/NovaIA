```visual basic
' Esta es una aplicación de calculadora simple que permite al usuario realizar cálculos matemáticos básicos.

' Declarar variables
Dim num1 As Double
Dim num2 As Double
Dim operacion As String
Dim resultado As Double

' Crear una interfaz de usuario
Form1.Caption = "Calculadora"
Form1.Width = 300
Form1.Height = 200
Form1.FormBorderStyle = FormBorderStyle.FixedSingle

' Añadir controles a la interfaz de usuario
Dim lblNum1 As Label
lblNum1.Location = New Point(10, 10)
lblNum1.Size = New Size(100, 20)
lblNum1.Text = "Número 1:"
Form1.Controls.Add(lblNum1)

Dim txtNum1 As TextBox
txtNum1.Location = New Point(10, 30)
txtNum1.Size = New Size(100, 20)
Form1.Controls.Add(txtNum1)

Dim lblNum2 As Label
lblNum2.Location = New Point(10, 50)
lblNum2.Size = New Size(100, 20)
lblNum2.Text = "Número 2:"
Form1.Controls.Add(lblNum2)

Dim txtNum2 As TextBox
txtNum2.Location = New Point(10, 70)
txtNum2.Size = New Size(100, 20)
Form1.Controls.Add(txtNum2)

Dim lblOperacion As Label
lblOperacion.Location = New Point(10, 90)
lblOperacion.Size = New Size(100, 20)
lblOperacion.Text = "Operación:"
Form1.Controls.Add(lblOperacion)

Dim cmbOperacion As ComboBox
cmbOperacion.Location = New Point(10, 110)
cmbOperacion.Size = New Size(100, 20)
cmbOperacion.Items.AddRange({"Suma", "Resta", "Multiplicación", "División"})
Form1.Controls.Add(cmbOperacion)

Dim lblResultado As Label
lblResultado.Location = New Point(10, 130)
lblResultado.Size = New Size(100, 20)
lblResultado.Text = "Resultado:"
Form1.Controls.Add(lblResultado)

Dim txtResultado As TextBox
txtResultado.Location = New Point(10, 150)
txtResultado.Size = New Size(100, 20)
txtResultado.ReadOnly = True
Form1.Controls.Add(txtResultado)

' Añadir un evento Click al botón Calcular
Dim btnCalcular As Button
btnCalcular.Location = New Point(120, 110)
btnCalcular.Size = New Size(75, 23)
btnCalcular.Text = "Calcular"
AddHandler btnCalcular.Click, AddressOf btnCalcular_Click
Form1.Controls.Add(btnCalcular)

' Definir el evento Click del botón Calcular
Private Sub btnCalcular_Click(sender As Object, e As EventArgs)
    ' Obtener los números ingresados por el usuario
    num1 = Val(txtNum1.Text)
    num2 = Val(txtNum2.Text)

    ' Obtener la operación seleccionada por el usuario
    operacion = cmbOperacion.SelectedItem.ToString()

    ' Realizar la operación matemática seleccionada
    Select Case operacion
        Case "Suma"
            resultado = num1 + num2
        Case "Resta"
            resultado = num1 - num2
        Case "Multiplicación"
            resultado = num1 * num2
        Case "División"
            If num2 <> 0 Then
                resultado = num1 / num2
            Else
                MessageBox.Show("No se puede dividir por cero.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
    End Select

    ' Mostrar el resultado en la interfaz de usuario
    txtResultado.Text = resultado
End Sub

' Iniciar la aplicación
Application.Run(Form1)
```

**Explicación:**

* La primera parte del código declara las variables que se utilizarán en la aplicación.
* La segunda parte crea la interfaz de usuario, añadiendo controles como etiquetas, cuadros de texto, un cuadro combinado y un botón.
* La tercera parte añade un evento Click al botón Calcular, que se ejecutará cuando el usuario haga clic en él.
* La cuarta parte define el evento Click del botón Calcular, que obtiene los números ingresados por el usuario, la operación seleccionada y realiza la operación matemática correspondiente.
* La quinta parte muestra el resultado de la operación en la interfaz de usuario.
* La sexta parte inicia la aplicación.