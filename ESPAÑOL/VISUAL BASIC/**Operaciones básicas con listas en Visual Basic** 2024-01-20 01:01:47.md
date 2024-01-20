```visual basic
' Programa principal
Private Sub Main()
    ' Declarar variables
    Dim listaPares() As Integer = {2, 4, 6, 8, 10}
    Dim listaImpares() As Integer = {1, 3, 5, 7, 9}
    Dim listaMixta() As Integer = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

    ' Muestra las listas en un bucle
    For i As Integer = 0 To listaPares.Length - 1
        Console.WriteLine("Lista pares: {0}", listaPares(i))
    Next

    For i As Integer = 0 To listaImpares.Length - 1
        Console.WriteLine("Lista impares: {0}", listaImpares(i))
    Next

    For i As Integer = 0 To listaMixta.Length - 1
        Console.WriteLine("Lista mixta: {0}", listaMixta(i))
    Next

    ' Buscar un elemento en una lista
    Dim elementoABuscar As Integer = 5
    If listaMixta.Contains(elementoABuscar) Then
        Console.WriteLine("El elemento {0} se encuentra en la lista mixta", elementoABuscar)
    Else
        Console.WriteLine("El elemento {0} no se encuentra en la lista mixta", elementoABuscar)
    End If

    ' Ordenar una lista
    Array.Sort(listaMixta)

    ' Muestra la lista ordenada
    For i As Integer = 0 To listaMixta.Length - 1
        Console.WriteLine("Lista ordenada: {0}", listaMixta(i))
    Next

    ' Invierte el orden de una lista
    Array.Reverse(listaMixta)

    ' Muestra la lista invertida
    For i As Integer = 0 To listaMixta.Length - 1
        Console.WriteLine("Lista invertida: {0}", listaMixta(i))
    Next

    ' Eliminar un elemento de una lista
    Dim elementoAEliminar As Integer = 5
    listaMixta = listaMixta.Where(Function(x) x <> elementoAEliminar).ToArray()

    ' Muestra la lista sin el elemento eliminado
    For i As Integer = 0 To listaMixta.Length - 1
        Console.WriteLine("Lista sin el elemento {0}: {1}", elementoAEliminar, listaMixta(i))
    Next

    ' Agregar un elemento a una lista
    listaMixta = listaMixta.Append(11).ToArray()

    ' Muestra la lista con el elemento agregado
    For i As Integer = 0 To listaMixta.Length - 1
        Console.WriteLine("Lista con el elemento {0} agregado: {1}", 11, listaMixta(i))
    Next

    ' Crear una lista de objetos
    Dim listaPersonas() As Persona = {New Persona("Juan", 25), New Persona("María", 30), New Persona("Pedro", 35)}

    ' Muestra la lista de objetos
    For i As Integer = 0 To listaPersonas.Length - 1
        Console.WriteLine("Nombre: {0}, Edad: {1}", listaPersonas(i).Nombre, listaPersonas(i).Edad)
    Next

    ' Ordenar una lista de objetos por una propiedad
    Array.Sort(listaPersonas, Function(x, y) x.Edad.CompareTo(y.Edad))

    ' Muestra la lista de objetos ordenada
    For i As Integer = 0 To listaPersonas.Length - 1
        Console.WriteLine("Nombre: {0}, Edad: {1}", listaPersonas(i).Nombre, listaPersonas(i).Edad)
    Next
End Sub

' Clase Persona
Public Class Persona
    ' Propiedades
    Public Property Nombre As String
    Public Property Edad As Integer

    ' Constructor
    Public Sub New(nombre As String, edad As Integer)
        Me.Nombre = nombre
        Me.Edad = edad
    End Sub

    ' Método ToString()
    Public Overrides Function ToString() As String
        Return String.Format("{{Nombre: {0}, Edad: {1}}}", Nombre, Edad)
    End Function
End Class
```

Explicación del código:

1. Se declaran tres listas de enteros: `listaPares`, `listaImpares` y `listaMixta`. La primera contiene números pares, la segunda números impares y la tercera una mezcla de ambos.
2. Se utiliza un bucle `For` para recorrer cada una de las listas y mostrar sus elementos en la consola.
3. Se busca el elemento 5 en la lista `listaMixta` utilizando el método `Contains()`. Si se encuentra, se muestra un mensaje en la consola.
4. Se ordena la lista `listaMixta` utilizando el método `Array.Sort()`.
5. Se invierte el orden de la lista `listaMixta` utilizando el método `Array.Reverse()`.
6. Se elimina el elemento 5 de la lista `listaMixta` utilizando el método `Where()`.
7. Se agrega el elemento 11 a la lista `listaMixta` utilizando el método `Append()`.
8. Se crea una lista de objetos `Persona` llamada `listaPersonas`. Cada objeto `Persona` tiene dos propiedades: `Nombre` y `Edad`.
9. Se muestra la lista de objetos `listaPersonas` en la consola.
10. Se ordena la lista `listaPersonas` por la propiedad `Edad` utilizando el método `Array.Sort()`.
11. Se muestra la lista de objetos `listaPersonas` ordenada en la consola.