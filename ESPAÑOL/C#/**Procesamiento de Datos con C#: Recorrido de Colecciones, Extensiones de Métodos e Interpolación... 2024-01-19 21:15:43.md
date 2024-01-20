```c#
// Clase principal
public class Program
{
    // Método principal
    public static void Main(string[] args)
    {
        // Variables locales
        string[] nombres = new string[] { "Juan", "María", "Pedro", "Ana", "Luis" };
        List<int> números = new List<int> { 1, 2, 3, 4, 5 };
        Dictionary<string, int> edades = new Dictionary<string, int> {
            { "Juan", 20 },
            { "María", 25 },
            { "Pedro", 30 },
            { "Ana", 35 },
            { "Luis", 40 }
        };

        // Bucle foreach para recorrer el arreglo de nombres
        Console.WriteLine("Nombres:");
        foreach (string nombre in nombres)
        {
            Console.WriteLine(nombre);
        }

        // Bucle foreach para recorrer la lista de números
        Console.WriteLine("\nNúmeros:");
        foreach (int número in números)
        {
            Console.WriteLine(número);
        }

        // Bucle foreach para recorrer el diccionario de edades
        Console.WriteLine("\nEdades:");
        foreach (KeyValuePair<string, int> edad in edades)
        {
            Console.WriteLine($"{edad.Key}: {edad.Value}");
        }

        // Uso de la extensión "Where" para filtrar los números pares de la lista
        var númerosPares = números.Where(número => número % 2 == 0);

        // Uso de la extensión "Select" para obtener los nombres en mayúsculas del arreglo
        var nombresEnMayúsculas = nombres.Select(nombre => nombre.ToUpper());

        // Uso de la extensión "Join" para concatenar los nombres y las edades
        var nombresYEdades = nombres.Join(edades, nombre => nombre, edad => edad.Key, (nombre, edad) => $"{nombre}: {edad.Value}");

        // Impresión de los resultados
        Console.WriteLine("\nNúmeros pares:");
        foreach (int númeroPar in númerosPares)
        {
            Console.WriteLine(númeroPar);
        }

        Console.WriteLine("\nNombres en mayúsculas:");
        foreach (string nombreMayúscula in nombresEnMayúsculas)
        {
            Console.WriteLine(nombreMayúscula);
        }

        Console.WriteLine("\nNombres y edades:");
        foreach (string nombreYEdad in nombresYEdades)
        {
            Console.WriteLine(nombreYEdad);
        }
    }
}
```

**Explicación:**

Este código en C# es un ejemplo de cómo utilizar diferentes características del lenguaje, incluyendo la sintaxis de tipos genéricos, la extensión de métodos "Where", "Select" y "Join", y la interpolación de cadenas. El código realiza varias operaciones sobre conjuntos de datos, como filtrar, transformar y unir datos, y luego muestra los resultados en la consola.

**1. Declaración de variables:**

* `string[] nombres = new string[] { "Juan", "María", "Pedro", "Ana", "Luis" }`: se declara un arreglo de cadenas llamado `nombres` que contiene cinco nombres.
* `List<int> números = new List<int> { 1, 2, 3, 4, 5 }`: se declara una lista de enteros llamada `números` que contiene cinco números.
* `Dictionary<string, int> edades = new Dictionary<string, int> { ... }`: se declara un diccionario de cadenas a enteros llamado `edades` que contiene cinco pares de claves y valores que asocian nombres con edades.

**2. Recorrido de colecciones:**

* `foreach (string nombre in nombres)`: se utiliza un bucle `foreach` para recorrer el arreglo `nombres` y mostrar cada nombre en la consola.
* `foreach (int número in números)`: se utiliza un bucle `foreach` para recorrer la lista `números` y mostrar cada número en la consola.
* `foreach (KeyValuePair<string, int> edad in edades)`: se utiliza un bucle `foreach` para recorrer el diccionario `edades` y mostrar cada par de clave y valor en la consola.

**3. Uso de extensiones de métodos:**

* `números.Where(número => número % 2 == 0)`: se utiliza la extensión de método `Where` para filtrar los números pares de la lista `números`.
* `nombres.Select(nombre => nombre.ToUpper())`: se utiliza la extensión de método `Select` para obtener los nombres en mayúsculas del arreglo `nombres`.
* `nombres.Join(edades, nombre => nombre, edad => edad.Key, (nombre, edad) => $"{nombre}: {edad.Value}")`: se utiliza la extensión de método `Join` para concatenar los nombres y las edades de los dos conjuntos de datos.

**4. Interpolación de cadenas:**

* `$"{nombre}: {edad.Value}"`: se utiliza la interpolación de cadenas para concatenar el nombre y la edad de cada par de clave y valor en el diccionario `edades`.

**5. Impresión de resultados:**

* `Console.WriteLine()` se utiliza para mostrar los resultados de las operaciones en la consola.