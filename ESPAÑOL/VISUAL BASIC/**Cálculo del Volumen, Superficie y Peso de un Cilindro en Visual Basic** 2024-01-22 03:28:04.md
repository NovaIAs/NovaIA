```visual basic
' Definir constantes
Const PI = 3.141592653589793
Const GRAVEDAD = 9.80665

' Definir variables
Dim radio As Double
Dim altura As Double
Dim volumen As Double
Dim superficie As Double
Dim masa As Double
Dim peso As Double

' Solicitar al usuario que introduzca los datos
Console.Write("Radio del cilindro: ")
radio = Console.ReadLine()

Console.Write("Altura del cilindro: ")
altura = Console.ReadLine()

Console.Write("Masa del cilindro: ")
masa = Console.ReadLine()

' Calcular el volumen del cilindro
volumen = PI * radio^2 * altura

' Calcular la superficie del cilindro
superficie = 2 * PI * radio * altura + 2 * PI * radio^2

' Calcular el peso del cilindro
peso = masa * GRAVEDAD

' Mostrar los resultados
Console.WriteLine("Volumen del cilindro: " & volumen)
Console.WriteLine("Superficie del cilindro: " & superficie)
Console.WriteLine("Peso del cilindro: " & peso)

' Esperar a que el usuario presione una tecla para salir
Console.ReadKey()
```

Explicación del código:

* La primera parte del código define dos constantes, `PI` y `GRAVEDAD`, que se utilizan en los cálculos.
* La segunda parte define las variables que se utilizarán en el programa.
* La tercera parte solicita al usuario que introduzca los datos necesarios para los cálculos.
* La cuarta parte realiza los cálculos del volumen, la superficie y el peso del cilindro.
* La quinta parte muestra los resultados de los cálculos en la consola.
* La sexta parte espera a que el usuario presione una tecla para salir del programa.