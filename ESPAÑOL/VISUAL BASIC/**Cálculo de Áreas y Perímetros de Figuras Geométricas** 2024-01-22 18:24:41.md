```visual basic
' Definición de la clase "Circulo"
Class Circulo

    ' Propiedades privadas
    Private mRadio As Single
    Private mColor As String

    ' Constructor
    Public Sub New(ByVal radio As Single, ByVal color As String)
        mRadio = radio
        mColor = color
    End Sub

    ' Métodos
    Public Function CalcularArea() As Single
        Return Math.PI * mRadio ^ 2
    End Function

    Public Function CalcularPerimetro() As Single
        Return 2 * Math.PI * mRadio
    End Function

    Public Function ObtenerRadio() As Single
        Return mRadio
    End Function

    Public Function ObtenerColor() As String
        Return mColor
    End Function

End Class

' Definición de la clase "Figura"
Class Figura

    ' Propiedades privadas
    Private mNombre As String
    Private mArea As Single
    Private mPerimetro As Single

    ' Constructor
    Public Sub New(ByVal nombre As String, ByVal area As Single, ByVal perimetro As Single)
        mNombre = nombre
        mArea = area
        mPerimetro = perimetro
    End Sub

    ' Métodos
    Public Function ObtenerNombre() As String
        Return mNombre
    End Function

    Public Function ObtenerArea() As Single
        Return mArea
    End Function

    Public Function ObtenerPerimetro() As Single
        Return mPerimetro
    End Function

End Class

' Definición de la clase "Cuadrado"
Class Cuadrado

    ' Propiedades privadas
    Private mLado As Single

    ' Constructor
    Public Sub New(ByVal lado As Single)
        mLado = lado
    End Sub

    ' Métodos
    Public Function CalcularArea() As Single
        Return mLado ^ 2
    End Function

    Public Function CalcularPerimetro() As Single
        Return 4 * mLado
    End Function

End Class

' Definición de la clase "Triangulo"
Class Triangulo

    ' Propiedades privadas
    Private mA As Single
    Private mB As Single
    Private mC As Single

    ' Constructor
    Public Sub New(ByVal a As Single, ByVal b As Single, ByVal c As Single)
        mA = a
        mB = b
        mC = c
    End Sub

    ' Métodos
    Public Function CalcularPerimetro() As Single
        Return mA + mB + mC
    End Function

End Class

' Definición de la clase "Programa"
Class Programa

    ' Métodos
    Public Shared Sub Main()

        ' Crear objetos de las diferentes clases
        Dim circulo1 As New Circulo(5, "Rojo")
        Dim circulo2 As New Circulo(10, "Azul")
        Dim cuadrado1 As New Cuadrado(5)
        Dim cuadrado2 As New Cuadrado(10)
        Dim triangulo1 As New Triangulo(3, 4, 5)

        ' Almacenar los objetos en una lista
        Dim figuras As New List(Of Figura)()
        figuras.Add(circulo1)
        figuras.Add(circulo2)
        figuras.Add(cuadrado1)
        figuras.Add(cuadrado2)
        figuras.Add(triangulo1)

        ' Recorrer la lista y mostrar la información de cada figura
        For Each figura In figuras
            Console.WriteLine("Nombre: {0}", figura.ObtenerNombre())
            Console.WriteLine("Área: {0}", figura.ObtenerArea())
            Console.WriteLine("Perímetro: {0}", figura.ObtenerPerimetro())
            Console.WriteLine()
        Next

    End Sub

End Class
```

Explicación del código:

* Se definen las clases `Circulo`, `Figura`, `Cuadrado` y `Triangulo`, que representan diferentes tipos de figuras geométricas.
* La clase `Circulo` tiene propiedades para el radio y el color, y métodos para calcular el área y el perímetro.
* La clase `Figura` tiene propiedades para el nombre, el área y el perímetro.
* La clase `Cuadrado` tiene una propiedad para el lado, y métodos para calcular el área y el perímetro.
* La clase `Triangulo` tiene propiedades para los tres lados, y un método para calcular el perímetro.
* Se define la clase `Programa` que contiene el método `Main()` que es el punto de entrada del programa.
* En el método `Main()` se crean objetos de las diferentes clases y se almacenan en una lista.
* Se recorre la lista y se muestra la información de cada figura.