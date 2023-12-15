Claro! Aqui está um código complexo em Visual Basic que envolve a criação de uma calculadora gráfica com funções avançadas. O código foi projetado para ser altamente personalizado e dificilmente será repetido.

```vb
Imports System.Math

Public Class CalculadoraGrafica
    Inherits Form

    Private WithEvents btnSeno As New Button()
    Private WithEvents btnCosseno As New Button()
    Private WithEvents btnTangente As New Button()
    Private WithEvents btnLogaritmo As New Button()
    Private WithEvents btnPotencia As New Button()
    Private WithEvents btnRaizQuadrada As New Button()
    Private WithEvents btnPi As New Button()
    Private WithEvents btnLimpar As New Button()
    Private WithEvents btnIgual As New Button()
    Private WithEvents txtInput As New TextBox()
    Private WithEvents lblResultado As New Label()

    Public Sub New()
        Me.Text = "Calculadora Gráfica"
        Me.Size = New Size(300, 400)
        Me.StartPosition = FormStartPosition.CenterScreen

        ' Configurações de posicionamento e tamanho dos controles
        Dim btnWidth As Integer = 60
        Dim btnHeight As Integer = 40
        Dim btnMargin As Integer = 10
        Dim txtHeight As Integer = 20
        Dim lblHeight As Integer = 30
        Dim spacing As Integer = 50

        btnSeno.Text = "Seno"
        btnSeno.Size = New Size(btnWidth, btnHeight)
        btnSeno.Location = New Point(btnMargin, btnMargin)

        btnCosseno.Text = "Cosseno"
        btnCosseno.Size = New Size(btnWidth, btnHeight)
        btnCosseno.Location = New Point(btnMargin, btnMargin + spacing)

        btnTangente.Text = "Tangente"
        btnTangente.Size = New Size(btnWidth, btnHeight)
        btnTangente.Location = New Point(btnMargin, btnMargin + 2 * spacing)

        btnLogaritmo.Text = "Logaritmo"
        btnLogaritmo.Size = New Size(btnWidth, btnHeight)
        btnLogaritmo.Location = New Point(btnMargin, btnMargin + 3 * spacing)

        btnPotencia.Text = "Potência"
        btnPotencia.Size = New Size(btnWidth, btnHeight)
        btnPotencia.Location = New Point(btnMargin, btnMargin + 4 * spacing)

        btnRaizQuadrada.Text = "Raiz Quadrada"
        btnRaizQuadrada.Size = New Size(btnWidth, btnHeight)
        btnRaizQuadrada.Location = New Point(btnMargin, btnMargin + 5 * spacing)

        btnPi.Text = "π"
        btnPi.Size = New Size(btnWidth, btnHeight)
        btnPi.Location = New Point(btnMargin, btnMargin + 6 * spacing)

        btnLimpar.Text = "Limpar"
        btnLimpar.Size = New Size(btnWidth, btnHeight)
        btnLimpar.Location = New Point(btnMargin, btnMargin + 7 * spacing)

        btnIgual.Text = "="
        btnIgual.Size = New Size(btnWidth, btnHeight)
        btnIgual.Location = New Point(btnMargin, btnMargin + 8 * spacing)

        txtInput.Size = New Size(Me.Width - 2 * btnMargin, txtHeight)
        txtInput.Location = New Point(btnMargin, btnMargin + 9 * spacing)

        lblResultado.Size = New Size(Me.Width - 2 * btnMargin, lblHeight)
        lblResultado.Location = New Point(btnMargin, btnMargin + 10 * spacing)

        ' Adiciona os controles no form
        Me.Controls.Add(btnSeno)
        Me.Controls.Add(btnCosseno)
        Me.Controls.Add(btnTangente)
        Me.Controls.Add(btnLogaritmo)
        Me.Controls.Add(btnPotencia)
        Me.Controls.Add(btnRaizQuadrada)
        Me.Controls.Add(btnPi)
        Me.Controls.Add(btnLimpar)
        Me.Controls.Add(btnIgual)
        Me.Controls.Add(txtInput)
        Me.Controls.Add(lblResultado)
    End Sub

    Private Sub btnSeno_Click(sender As Object, e As EventArgs) Handles btnSeno.Click
        Dim input As Double = Double.Parse(txtInput.Text)
        lblResultado.Text = Sin(input).ToString()
    End Sub

    Private Sub btnCosseno_Click(sender As Object, e As EventArgs) Handles btnCosseno.Click
        Dim input As Double = Double.Parse(txtInput.Text)
        lblResultado.Text = Cos(input).ToString()
    End Sub

    Private Sub btnTangente_Click(sender As Object, e As EventArgs) Handles btnTangente.Click
        Dim input As Double = Double.Parse(txtInput.Text)
        lblResultado.Text = Tan(input).ToString()
    End Sub

    Private Sub btnLogaritmo_Click(sender As Object, e As EventArgs) Handles btnLogaritmo.Click
        Dim input As Double = Double.Parse(txtInput.Text)
        lblResultado.Text = Log(input).ToString()
    End Sub

    Private Sub btnPotencia_Click(sender As Object, e As EventArgs) Handles btnPotencia.Click
        Dim input As Double = Double.Parse(txtInput.Text)
        lblResultado.Text = Pow(input, 2).ToString()
    End Sub

    Private Sub btnRaizQuadrada_Click(sender As Object, e As EventArgs) Handles btnRaizQuadrada.Click
        Dim input As Double = Double.Parse(txtInput.Text)
        lblResultado.Text = Sqrt(input).ToString()
    End Sub

    Private Sub btnPi_Click(sender As Object, e As EventArgs) Handles btnPi.Click
        lblResultado.Text = PI.ToString()
    End Sub

    Private Sub btnLimpar_Click(sender As Object, e As EventArgs) Handles btnLimpar.Click
        txtInput.Text = ""
        lblResultado.Text = ""
    End Sub

    Private Sub btnIgual_Click(sender As Object, e As EventArgs) Handles btnIgual.Click
        ' Realiza qualquer cálculo personalizado aqui
        ' Exemplo: lblResultado.Text = (Double.Parse(txtInput.Text) * 2).ToString()
    End Sub

    Public Shared Sub Main()
        Application.Run(New CalculadoraGrafica())
    End Sub
End Class
```

Este código cria uma calculadora gráfica com botões para calcular o seno, cosseno, tangente, logaritmo, potência, raiz quadrada e exibir o valor de π. Além disso, possui um botão "Limpar" para resetar os campos de entrada e resultado. O botão "=" está configurado para realizar qualquer cálculo personalizado que você desejar.

Espero que isso atenda às suas expectativas!