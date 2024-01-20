```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static void Main(string[] args)
    {
        // Crear una lista de 10 números enteros aleatorios
        List<int> numeros = new List<int>();
        Random random = new Random();
        for (int i = 0; i < 10; i++)
        {
            numeros.Add(random.Next(100));
        }

        // Mostrar la lista de números enteros aleatorios
        Console.WriteLine("Lista de números enteros aleatorios:");
        foreach (int numero in numeros)
        {
            Console.WriteLine(numero);
        }

        // Obtener la suma de los números enteros aleatorios
        int suma = 0;
        foreach (int numero in numeros)
        {
            suma += numero;
        }

        // Mostrar la suma de los números enteros aleatorios
        Console.WriteLine("Suma de los números enteros aleatorios:");
        Console.WriteLine(suma);

        // Obtener el promedio de los números enteros aleatorios
        double promedio = (double)suma / numeros.Count;

        // Mostrar el promedio de los números enteros aleatorios
        Console.WriteLine("Promedio de los números enteros aleatorios:");
        Console.WriteLine(promedio);

        // Ordenar la lista de números enteros aleatorios en orden ascendente
        numeros.Sort();

        // Mostrar la lista de números enteros aleatorios ordenada en orden ascendente
        Console.WriteLine("Lista de números enteros aleatorios ordenada en orden ascendente:");
        foreach (int numero in numeros)
        {
            Console.WriteLine(numero);
        }

        // Obtener el número entero aleatorio más grande
        int maximo = numeros[numeros.Count - 1];

        // Mostrar el número entero aleatorio más grande
        Console.WriteLine("Número entero aleatorio más grande:");
        Console.WriteLine(maximo);

        // Obtener el número entero aleatorio más pequeño
        int minimo = numeros[0];

        // Mostrar el número entero aleatorio más pequeño
        Console.WriteLine("Número entero aleatorio más pequeño:");
        Console.WriteLine(minimo);
    }
}
```

Este código realiza una serie de operaciones con una lista de 10 números enteros aleatorios. Primero, crea la lista y la llena con números enteros aleatorios. Luego, muestra la lista de números aleatorios, obtiene la suma de los números, el promedio de los números, ordena la lista en orden ascendente, obtiene el número entero aleatorio más grande y el número entero aleatorio más pequeño. Finalmente, muestra los resultados de todas las operaciones.

El código utiliza una serie de funciones y métodos de la biblioteca estándar de C# para realizar las operaciones. Por ejemplo, la función `Random.Next()` se utiliza para generar números aleatorios, el método `List.Add()` se utiliza para agregar elementos a una lista, el método `List.Sort()` se utiliza para ordenar una lista y el método `List.Count` se utiliza para obtener el número de elementos en una lista.