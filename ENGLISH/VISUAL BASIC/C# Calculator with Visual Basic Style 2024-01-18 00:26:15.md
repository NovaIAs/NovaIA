```visual basic
Public Class Calculator
    Private Const ADDITION As Integer = 1
    Private Const SUBTRACTION As Integer = 2
    Private Const MULTIPLICATION As Integer = 3
    Private Const DIVISION As Integer = 4

    Private Sub Form1_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ' Add the numbers 1 through 9 to the buttons on the form.
        For i As Integer = 1 To 9
            Me.Controls.Add(New Button() With {
                .Text = i.ToString(),
                .Name = "btn" & i,
                .Location = New Point((i - 1) * 50 + 10, 10),
                .Size = New Size(40, 40)
            })
        }

        ' Add the operator buttons (+, -, *, /) to the form.
        Me.Controls.Add(New Button() With {
            .Text = "+",
            .Name = "btnPlus",
            .Location = New Point(210, 10),
            .Size = New Size(40, 40)
        })

        Me.Controls.Add(New Button() With {
            .Text = "-",
            .Name = "btnMinus",
            .Location = New Point(210, 60),
            .Size = New Size(40, 40)
        })

        Me.Controls.Add(New Button() With {
            .Text = "*",
            .Name = "btnMultiply",
            .Location = New Point(210, 110),
            .Size = New Size(40, 40)
        })

        Me.Controls.Add(New Button() With {
            .Text = "/",
            .Name = "btnDivide",
            .Location = New Point(210, 160),
            .Size = New Size(40, 40)
        })

        ' Add the equals button (=) to the form.
        Me.Controls.Add(New Button() With {
            .Text = "=",
            .Name = "btnEquals",
            .Location = New Point(150, 210),
            .Size = New Size(100, 40)
        })

        ' Add the clear button (C) to the form.
        Me.Controls.Add(New Button() With {
            .Text = "C",
            .Name = "btnClear",
            .Location = New Point(20, 210),
            .Size = New Size(100, 40)
        })

        ' Add the decimal point button (.) to the form.
        Me.Controls.Add(New Button() With {
            .Text = ".",
            .Name = "btnDecimal",
            .Location = New Point(100, 210),
            .Size = New Size(40, 40)
        })
    End Sub

    Private Sub Button_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btn0.Click, btn1.Click, _
        btn2.Click, btn3.Click, btn4.Click, btn5.Click, btn6.Click, btn7.Click, btn8.Click, btn9.Click, _
        btnPlus.Click, btnMinus.Click, btnMultiply.Click, btnDivide.Click, btnDecimal.Click, btnEquals.Click, _
        btnClear.Click

        ' Get the button that was clicked.
        Dim button As Button = DirectCast(sender, Button)

        ' If the button is a number button, add the number to the display.
        If button.Name.StartsWith("btn") Then
            Me.txtDisplay.Text += button.Text
        End If

        ' If the button is the decimal point button, add a decimal point to the display.
        If button.Name = "btnDecimal" Then
            If Me.txtDisplay.Text.IndexOf(".") = -1 Then
                Me.txtDisplay.Text += "."
            End If
        End If

        ' If the button is an operator button, store the operator and the first number in the calculation.
        If button.Name = "btnPlus" OrElse button.Name = "btnMinus" OrElse button.Name = "btnMultiply" OrElse button.Name = "btnDivide" Then
            Me.operator = If(button.Name = "btnPlus", ADDITION, _
                If(button.Name = "btnMinus", SUBTRACTION, _
                    If(button.Name = "btnMultiply", MULTIPLICATION, DIVISION)))
            Me.firstNumber = Val(Me.txtDisplay.Text)
            Me.txtDisplay.Text = ""
        End If

        ' If the button is the equals button, perform the calculation and display the result.
        If button.Name = "btnEquals" Then
            Select Case Me.operator
                Case ADDITION
                    Me.txtDisplay.Text = (Me.firstNumber + Val(Me.txtDisplay.Text)).ToString()
                Case SUBTRACTION
                    Me.txtDisplay.Text = (Me.firstNumber - Val(Me.txtDisplay.Text)).ToString()
                Case MULTIPLICATION
                    Me.txtDisplay.Text = (Me.firstNumber * Val(Me.txtDisplay.Text)).ToString()
                Case DIVISION
                    If Val(Me.txtDisplay.Text) = 0 Then
                        MsgBox("Cannot divide by zero.", MsgBoxStyle.Critical)
                    Else
                        Me.txtDisplay.Text = (Me.firstNumber / Val(Me.txtDisplay.Text)).ToString()
                    End If
            End Select
        End If

        ' If the button is the clear button, clear the display.
        If button.Name = "btnClear" Then
            Me.txtDisplay.Text = ""
        End If
    End Sub
End Class
```

This code is a simple calculator that allows the user to enter numbers and operators to perform basic arithmetic operations. The calculator has a text box where the user can enter numbers and operators, and a button for each number and operator. The calculator also has an equals button that performs the calculation and displays the result in the text box.

The code is written in Visual Basic and uses the Windows Forms library to create the calculator's user interface. The code is divided into two classes: the `Calculator` class and the `Form1` class.

The `Calculator` class contains the code that performs the calculations. The class has a constructor that initializes the operator and first number properties, and a `Button_Click` method that handles the click events for all of the buttons on the calculator. The `Button_Click` method uses the `If` statement to determine which button was clicked, and then performs the appropriate action.

The `Form1` class contains the code that creates the calculator's user interface. The class has a `Load` method that creates the buttons and text box for the calculator, and a `Button_Click` method that handles the click events for all of the buttons on the calculator. The `Button_Click` method calls the `Button_Click` method in the `Calculator` class to perform the calculations.