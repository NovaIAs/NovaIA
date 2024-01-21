```visual basic
' Declaración de variables
Dim listaNumeros As List(Of Integer)
Dim sumaTotal As Integer
Dim promedio As Double
Dim mediana As Double
Dim moda As Integer
Dim desviacionEstandar As Double

' Ingreso de datos
Console.WriteLine("Ingrese una lista de números separados por comas:")
Dim entrada As String = Console.ReadLine()
listaNumeros = entrada.Split(",").Select(Function(s) Integer.Parse(s)).ToList()

' Cálculo de la suma total
sumaTotal = listaNumeros.Sum()

' Cálculo de la media
promedio = listaNumeros.Average()

' Cálculo de la mediana
listaNumeros.Sort()
Dim mitad1 As Integer = listaNumeros.Count() / 2
Dim mitad2 As Integer = mitad1 + 1
If listaNumeros.Count() Mod 2 = 0 Then
    mediana = (listaNumeros(mitad1) + listaNumeros(mitad2)) / 2
Else
    mediana = listaNumeros(mitad1)
End If

' Cálculo de la moda
moda = listaNumeros.GroupBy(Function(numero) numero).OrderByDescending(Function(grupo) grupo.Count()).First().Key

' Cálculo de la desviación estándar
desviacionEstandar = Math.Sqrt(listaNumeros.Select(Function(numero) (numero - promedio) ^ 2).Sum() / listaNumeros.Count())

' Impresión de resultados
Console.WriteLine("Suma total: " & sumaTotal)
Console.WriteLine("Promedio: " & promedio)
Console.WriteLine("Mediana: " & mediana)
Console.WriteLine("Moda: " & moda)
Console.WriteLine("Desviación estándar: " & desviacionEstandar)

' Pausa para que el usuario vea los resultados
Console.ReadKey()
```

Explicación del código:

* Declaración de variables: Se declaran las variables necesarias para almacenar los datos y los resultados de los cálculos.
* Ingreso de datos: Se pide al usuario que introduzca una lista de números separados por comas.
* Cálculo de la suma total: Se utiliza la función `Sum()` para calcular la suma total de los números introducidos.
* Cálculo de la media: Se utiliza la función `Average()` para calcular la media de los números introducidos.
* Cálculo de la mediana: Se ordena la lista de números y se calcula la mediana en función de si el número de elementos de la lista es par o impar.
* Cálculo de la moda: Se agrupan los números por su valor y se selecciona el grupo con mayor número de elementos. El valor del elemento de este grupo es la moda.
* Cálculo de la desviación estándar: Se utiliza la función `Sqrt()` para calcular la desviación estándar de los números introducidos.
* Impresión de resultados: Se muestran los resultados de los cálculos en la consola.
* Pausa para que el usuario vea los resultados: Se pausa la consola para que el usuario tenga tiempo de ver los resultados antes de que se cierre la aplicación.