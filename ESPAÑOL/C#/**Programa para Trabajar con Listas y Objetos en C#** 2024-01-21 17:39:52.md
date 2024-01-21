```c#
// Clase principal del programa
public class ProgramaPrincipal
{
    // Método principal del programa
    public static void Main(string[] args)
    {
        // Crear una lista de números enteros
        List<int> listaNumeros = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

        // Crear una lista de cadenas de texto
        List<string> listaCadenas = new List<string>() { "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez" };

        // Crear una lista de objetos de la clase Persona
        List<Persona> listaPersonas = new List<Persona>() { new Persona("Juan", 20), new Persona("María", 25), new Persona("Pedro", 30) };

        // Imprimir la lista de números enteros
        Console.WriteLine("Lista de números enteros:");
        foreach (int numero in listaNumeros)
        {
            Console.WriteLine(numero);
        }

        // Imprimir la lista de cadenas de texto
        Console.WriteLine("Lista de cadenas de texto:");
        foreach (string cadena in listaCadenas)
        {
            Console.WriteLine(cadena);
        }

        // Imprimir la lista de objetos de la clase Persona
        Console.WriteLine("Lista de objetos de la clase Persona:");
        foreach (Persona persona in listaPersonas)
        {
            Console.WriteLine($"{persona.Nombre} - {persona.Edad}");
        }

        // Ordenar la lista de números enteros en orden ascendente
        listaNumeros.Sort();

        // Ordenar la lista de cadenas de texto en orden alfabético
        listaCadenas.Sort();

        // Ordenar la lista de objetos de la clase Persona por edad en orden ascendente
        listaPersonas.Sort((p1, p2) => p1.Edad.CompareTo(p2.Edad));

        // Imprimir la lista de números enteros ordenados
        Console.WriteLine("Lista de números enteros ordenados:");
        foreach (int numero in listaNumeros)
        {
            Console.WriteLine(numero);
        }

        // Imprimir la lista de cadenas de texto ordenadas
        Console.WriteLine("Lista de cadenas de texto ordenadas:");
        foreach (string cadena in listaCadenas)
        {
            Console.WriteLine(cadena);
        }

        // Imprimir la lista de objetos de la clase Persona ordenados
        Console.WriteLine("Lista de objetos de la clase Persona ordenados:");
        foreach (Persona persona in listaPersonas)
        {
            Console.WriteLine($"{persona.Nombre} - {persona.Edad}");
        }
    }
}

// Clase Persona
public class Persona
{
    public string Nombre { get; set; }
    public int Edad { get; set; }

    public Persona(string nombre, int edad)
    {
        Nombre = nombre;
        Edad = edad;
    }
}
```

Este código es un programa completo en C# que crea tres listas: una de números enteros, una de cadenas de texto y una de objetos de la clase Persona. Luego, el programa imprime las tres listas, ordena cada una de ellas y vuelve a imprimirlas.

El código está dividido en varias partes:

* La clase `Persona` define un objeto que tiene dos propiedades: `Nombre` y `Edad`.
* El método `Main` es el método principal del programa. Aquí se crean las tres listas, se imprimen, se ordenan y se vuelven a imprimir.
* El método `Sort` ordena una lista en orden ascendente.
* El método `Sort` con un comparador compara dos objetos y devuelve un entero que indica si el primer objeto es menor, igual o mayor que el segundo objeto.

Este código es un ejemplo de cómo utilizar listas y objetos en C#. También muestra cómo ordenar listas utilizando el método `Sort`.