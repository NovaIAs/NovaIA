```visual basic

' Declaración de variables
Dim contador As Integer
Dim suma As Integer
Dim máximo As Integer
Dim mínimo As Integer
Dim promedio As Double

' Función que solicita al usuario un número entero y lo devuelve
Function LeerEntero() As Integer
    Dim número As Integer
    Do
        Console.Write("Introduzca un número entero: ")
        número = Integer.Parse(Console.ReadLine())
    Loop While número < 0
    Return número
End Function

' Función que calcula la suma de los números introducidos por el usuario
Function CalcularSuma(números() As Integer) As Integer
    Dim suma As Integer
    For i As Integer = 0 To números.Length - 1
        suma += números(i)
    Next
    Return suma
End Function

' Función que calcula el máximo de los números introducidos por el usuario
Function CalcularMáximo(números() As Integer) As Integer
    Dim máximo As Integer = números(0)
    For i As Integer = 1 To números.Length - 1
        If números(i) > máximo Then
            máximo = números(i)
        End If
    Next
    Return máximo
End Function

' Función que calcula el mínimo de los números introducidos por el usuario
Function CalcularMínimo(números() As Integer) As Integer
    Dim mínimo As Integer = números(0)
    For i As Integer = 1 To números.Length - 1
        If números(i) < mínimo Then
            mínimo = números(i)
        End If
    Next
    Return mínimo
End Function

' Función que calcula el promedio de los números introducidos por el usuario
Function CalcularPromedio(números() As Integer) As Double
    Dim suma As Integer = CalcularSuma(números)
    Dim promedio As Double = suma / números.Length
    Return promedio
End Function

' Función que imprime los resultados en la consola
Function ImprimirResultados(números() As Integer, suma As Integer, máximo As Integer, mínimo As Integer, promedio As Double)
    Console.WriteLine("Números introducidos:")
    For i As Integer = 0 To números.Length - 1
        Console.Write($"{números(i)} ")
    Next
    Console.WriteLine()
    Console.WriteLine($"Suma: {suma}")
    Console.WriteLine($"Máximo: {máximo}")
    Console.WriteLine($"Mínimo: {mínimo}")
    Console.WriteLine($"Promedio: {promedio}")
End Function

' Programa principal
Console.WriteLine("Introduzca una serie de números enteros positivos, separados por espacios:")
Dim números() As Integer = Console.ReadLine().Split(" ").Select(Function(x) Integer.Parse(x)).ToArray()

Dim suma = CalcularSuma(números)
Dim máximo = CalcularMáximo(números)
Dim mínimo = CalcularMínimo(números)
Dim promedio = CalcularPromedio(números)

ImprimirResultados(números, suma, máximo, mínimo, promedio)

' Esperar a que el usuario pulse cualquier tecla para salir
Console.ReadKey()

```

**Explicación del código:**

1. **Declaración de variables:** Se declaran las variables que se utilizarán en el programa, incluyendo:
    * `contador`: Contador que se utilizará para llevar la cuenta de los números introducidos por el usuario.
    * `suma`: Variable que almacenará la suma de los números introducidos por el usuario.
    * `máximo`: Variable que almacenará el número máximo introducido por el usuario.
    * `mínimo`: Variable que almacenará el número mínimo introducido por el usuario.
    * `promedio`: Variable que almacenará el promedio de los números introducidos por el usuario.

2. **Función `LeerEntero`:** Esta función solicita al usuario que introduzca un número entero y lo devuelve. La función utiliza un bucle `Do While` para asegurarse de que el usuario introduce un número entero positivo.

3. **Función `CalcularSuma`:** Esta función calcula la suma de los números introducidos por el usuario. La función utiliza un bucle `For` para recorrer los números introducidos y sumar cada uno de ellos.

4. **Función `CalcularMáximo`:** Esta función calcula el número máximo introducido por el usuario. La función utiliza un bucle `For` para recorrer los números introducidos y almacenar el número máximo en la variable `máximo`.

5. **Función `CalcularMínimo`:** Esta función calcula el número mínimo introducido por el usuario. La función utiliza un bucle `For` para recorrer los números introducidos y almacenar el número mínimo en la variable `mínimo`.

6. **Función `CalcularPromedio`:** Esta función calcula el promedio de los números introducidos por el usuario. La función utiliza la variable `suma` para almacenar la suma de los números introducidos y la variable `números.Length` para almacenar el número de números introducidos. El promedio se calcula dividiendo la suma de los números por el número de números.

7. **Función `ImprimirResultados`:** Esta función imprime los resultados en la consola. La función utiliza la función `Console.WriteLine()` para imprimir los números introducidos, la suma, el máximo, el mínimo y el promedio.

8. **Programa principal:** El programa principal solicita al usuario que introduzca una serie de números enteros positivos, separados por espacios. A continuación, el programa llama a las funciones `LeerEntero`, `CalcularSuma`, `CalcularMáximo`, `CalcularMínimo` y `CalcularPromedio` para procesar los números introducidos por el usuario. Finalmente, el programa llama a la función `ImprimirResultados` para imprimir los resultados en la consola.