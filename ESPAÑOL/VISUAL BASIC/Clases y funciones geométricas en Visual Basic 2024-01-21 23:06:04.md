```visual basic
' Módulo que define una clase para representar un círculo.

Public Class Circulo

    ' Declaración de variables privadas.
    Private _radio As Double
    Private _diámetro As Double
    Private _área As Double
    Private _circunferencia As Double

    ' Constructor público que toma el radio del círculo como argumento.
    Public Sub New(radio As Double)
        _radio = radio
        _diámetro = _radio * 2
        _área = Math.PI * _radio ^ 2
        _circunferencia = 2 * Math.PI * _radio
    End Sub

    ' Función pública que devuelve el radio del círculo.
    Public Function GetRadio() As Double
        Return _radio
    End Function

    ' Función pública que devuelve el diámetro del círculo.
    Public Function GetDiámetro() As Double
        Return _diámetro
    End Function

    ' Función pública que devuelve el área del círculo.
    Public Function GetÁrea() As Double
        Return _área
    End Function

    ' Función pública que devuelve la circunferencia del círculo.
    Public Function GetCircunferencia() As Double
        Return _circunferencia
    End Function

End Class

' Módulo que define una clase para representar un cuadrado.

Public Class Cuadrado

    ' Declaración de variables privadas.
    Private _lado As Double
    Private _área As Double
    Private _perímetro As Double

    ' Constructor público que toma el lado del cuadrado como argumento.
    Public Sub New(lado As Double)
        _lado = lado
        _área = _lado ^ 2
        _perímetro = _lado * 4
    End Sub

    ' Función pública que devuelve el lado del cuadrado.
    Public Function GetLado() As Double
        Return _lado
    End Function

    ' Función pública que devuelve el área del cuadrado.
    Public Function GetÁrea() As Double
        Return _área
    End Function

    ' Función pública que devuelve el perímetro del cuadrado.
    Public Function GetPerímetro() As Double
        Return _perímetro
    End Function

End Class

' Módulo que define una clase para representar un rectángulo.

Public Class Rectángulo

    ' Declaración de variables privadas.
    Private _largo As Double
    Private _ancho As Double
    Private _área As Double
    Private _perímetro As Double

    ' Constructor público que toma el largo y el ancho del rectángulo como argumentos.
    Public Sub New(largo As Double, ancho As Double)
        _largo = largo
        _ancho = ancho
        _área = _largo * _ancho
        _perímetro = 2 * _largo + 2 * _ancho
    End Sub

    ' Función pública que devuelve el largo del rectángulo.
    Public Function GetLargo() As Double
        Return _largo
    End Function

    ' Función pública que devuelve el ancho del rectángulo.
    Public Function GetAncho() As Double
        Return _ancho
    End Function

    ' Función pública que devuelve el área del rectángulo.
    Public Function GetÁrea() As Double
        Return _área
    End Function

    ' Función pública que devuelve el perímetro del rectángulo.
    Public Function GetPerímetro() As Double
        Return _perímetro
    End Function

End Class

' Módulo que define una clase para representar un triángulo.

Public Class Triángulo

    ' Declaración de variables privadas.
    Private _lado1 As Double
    Private _lado2 As Double
    Private _lado3 As Double
    Private _área As Double
    Private _perímetro As Double

    ' Constructor público que toma los tres lados del triángulo como argumentos.
    Public Sub New(lado1 As Double, lado2 As Double, lado3 As Double)
        _lado1 = lado1
        _lado2 = lado2
        _lado3 = lado3
        _área = 0.5 * _lado1 * _lado2 * Math.Sin(Math.PI / 3)
        _perímetro = _lado1 + _lado2 + _lado3
    End Sub

    ' Función pública que devuelve el lado 1 del triángulo.
    Public Function GetLado1() As Double
        Return _lado1
    End Function

    ' Función pública que devuelve el lado 2 del triángulo.
    Public Function GetLado2() As Double
        Return _lado2
    End Function