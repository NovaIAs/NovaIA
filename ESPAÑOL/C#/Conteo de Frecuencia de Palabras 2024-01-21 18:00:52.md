```c#
// Programa para calcular la frecuencia de cada palabra en un texto.

// Clase para almacenar la frecuencia de una palabra.
class PalabraFrecuencia
{
    public string Palabra { get; set; }
    public int Frecuencia { get; set; }

    public PalabraFrecuencia(string palabra, int frecuencia)
    {
        Palabra = palabra;
        Frecuencia = frecuencia;
    }
}

// Clase principal del programa.
class Program
{
    // Método principal del programa.
    static void Main(string[] args)
    {
        // Obtener el texto del usuario.
        Console.WriteLine("Introduce el texto:");
        string texto = Console.ReadLine();

        // Dividir el texto en palabras.
        string[] palabras = texto.Split(' ', ';', '.', '!', '?', ',', '\n', '\t');

        // Crear un diccionario para almacenar la frecuencia de cada palabra.
        Dictionary<string, int> frecuencias = new Dictionary<string, int>();

        // Recorrer las palabras del texto.
        foreach (string palabra in palabras)
        {
            // Obtener la palabra en minúsculas.
            string palabraMinusculas = palabra.ToLower();

            // Si la palabra no está en el diccionario, añadirla con una frecuencia de 1.
            if (!frecuencias.ContainsKey(palabraMinusculas))
            {
                frecuencias.Add(palabraMinusculas, 1);
            }
            // Si la palabra ya está en el diccionario, incrementar su frecuencia.
            else
            {
                frecuencias[palabraMinusculas]++;
            }
        }

        // Crear una lista de objetos PalabraFrecuencia.
        List<PalabraFrecuencia> palabrasFrecuencia = new List<PalabraFrecuencia>();

        // Recorrer el diccionario de frecuencias.
        foreach (KeyValuePair<string, int> frecuencia in frecuencias)
        {
            // Añadir un objeto PalabraFrecuencia a la lista.
            palabrasFrecuencia.Add(new PalabraFrecuencia(frecuencia.Key, frecuencia.Value));
        }

        // Ordenar la lista de objetos PalabraFrecuencia por frecuencia.
        palabrasFrecuencia.Sort((x, y) => y.Frecuencia.CompareTo(x.Frecuencia));

        // Mostrar la frecuencia de cada palabra.
        Console.WriteLine("Frecuencia de cada palabra:");
        foreach (PalabraFrecuencia palabraFrecuencia in palabrasFrecuencia)
        {
            Console.WriteLine($"{palabraFrecuencia.Palabra}: {palabraFrecuencia.Frecuencia}");
        }
    }
}
```

Explicación del código:

* La clase `PalabraFrecuencia` se utiliza para almacenar la frecuencia de una palabra. Tiene dos propiedades: `Palabra` y `Frecuencia`.
* La clase `Program` es la clase principal del programa. Contiene el método `Main`, que es el punto de entrada del programa.
* El método `Main` primero obtiene el texto del usuario y luego lo divide en palabras.
* El método `Main` crea un diccionario para almacenar la frecuencia de cada palabra. El diccionario utiliza la palabra en minúsculas como clave y la frecuencia de la palabra como valor.
* El método `Main` recorre las palabras del texto y actualiza el diccionario de frecuencias. Si una palabra no está en el diccionario, se añade con una frecuencia de 1. Si una palabra ya está en el diccionario, su frecuencia se incrementa.
* El método `Main` crea una lista de objetos `PalabraFrecuencia`. Cada objeto `PalabraFrecuencia` contiene una palabra y su frecuencia.
* El método `Main` ordena la lista de objetos `PalabraFrecuencia` por frecuencia.
* El método `Main` muestra la frecuencia de cada palabra.