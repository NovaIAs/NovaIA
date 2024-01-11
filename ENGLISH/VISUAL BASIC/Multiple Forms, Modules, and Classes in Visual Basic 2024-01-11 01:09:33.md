```visual basic
' This program demonstrates the use of multiple forms, modules, and classes in a Visual Basic application.

' Define the main form.
Public Class MainForm
    ' Declare variables.
    Private Sub MainForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Create a new instance of the child form.
        Dim childForm As New ChildForm()

        ' Set the child form's properties.
        childForm.Text = "Child Form"
        childForm.StartPosition = FormStartPosition.CenterScreen
        childForm.Show()
    End Sub

    ' Define a method to pass data to the child form.
    Public Sub PassDataToChildForm(data As String)
        ' Get the child form instance.
        Dim childForm As ChildForm = Me.ActiveMdiChild

        ' Pass the data to the child form.
        childForm.Label1.Text = data
    End Sub
End Class

' Define the child form.
Public Class ChildForm
    ' Declare variables.
    Private Sub ChildForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Set the form's properties.
        Me.Text = "Child Form"
        Me.StartPosition = FormStartPosition.CenterScreen
    End Sub
End Class

' Define a module to hold shared methods.
Module SharedMethods
    ' Define a method to generate a random number.
    Public Function GetRandomNumber() As Integer
        ' Generate a random number.
        Dim randomNumber As Integer = New Random().Next(1, 100)

        ' Return the random number.
        Return randomNumber
    End Function
End Module

' Define a class to hold data.
Public Class DataClass
    ' Declare variables.
    Private _name As String
    Private _age As Integer

    ' Define properties.
    Public Property Name() As String
        Get
            Return _name
        End Get
        Set(value As String)
            _name = value
        End Set
    End Property

    Public Property Age() As Integer
        Get
            Return _age
        End Get
        Set(value As Integer)
            _age = value
        End Set
    End Property

    ' Define a constructor.
    Public Sub New(name As String, age As Integer)
        _name = name
        _age = age
    End Sub
End Class

' Define a form to display data.
Public Class DataForm
    ' Declare variables.
    Private Sub DataForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Create a new instance of the data class.
        Dim data As New DataClass("John Doe", 30)

        ' Display the data.
        Me.Label1.Text = data.Name & " is " & data.Age & " years old."
    End Sub
End Class
```

This program demonstrates the use of multiple forms, modules, and classes in a Visual Basic application.

The MainForm class is the main form of the application. It contains a method called PassDataToChildForm() that is used to pass data to the child form.

The ChildForm class is the child form of the application. It contains a label called Label1 that is used to display data from the main form.

The SharedMethods module contains a method called GetRandomNumber() that is used to generate a random number.

The DataClass class is used to hold data. It contains two properties, Name and Age, and a constructor that is used to initialize the properties.

The DataForm class is used to display data from the DataClass class. It contains a label called Label1 that is used to display the data.

To use the program, simply run the MainForm class. The main form will be displayed, and you can then click the button to pass data to the child form. The child form will then be displayed, and the data will be displayed in the label.