```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Clase principal del programa
public class ProgramaComplejo
{
    // Método principal del programa
    public static void Main(string[] args)
    {
        // Declaración de variables y objetos
        List<Persona> personas = new List<Persona>();
        Persona persona1 = new Persona("Juan", "Pérez", 25);
        Persona persona2 = new Persona("María", "López", 30);
        Persona persona3 = new Persona("Pedro", "García", 35);

        // Adición de las personas a la lista
        personas.Add(persona1);
        personas.Add(persona2);
        personas.Add(persona3);

        // Búsqueda de una persona por su nombre y apellido
        Persona personaBuscada = personas.Find(p => p.Nombre == "Juan" && p.Apellido == "Pérez");

        // Impresión de los datos de la persona buscada
        Console.WriteLine("Datos de la persona buscada:");
        Console.WriteLine("Nombre: {0}", personaBuscada.Nombre);
        Console.WriteLine("Apellido: {0}", personaBuscada.Apellido);
        Console.WriteLine("Edad: {0}", personaBuscada.Edad);

        // Ordenamiento de la lista de personas por edad
        personas.Sort((p1, p2) => p1.Edad.CompareTo(p2.Edad));

        // Impresión de los datos de las personas ordenadas por edad
        Console.WriteLine("Datos de las personas ordenadas por edad:");
        foreach (Persona persona in personas)
        {
            Console.WriteLine("Nombre: {0}, Apellido: {1}, Edad: {2}", persona.Nombre, persona.Apellido, persona.Edad);
        }

        // Creación de una nueva lista de personas con las personas mayores de 30 años
        List<Persona> personasMayoresDe30 = personas.Where(p => p.Edad > 30).ToList();

        // Impresión de los datos de las personas mayores de 30 años
        Console.WriteLine("Datos de las personas mayores de 30 años:");
        foreach (Persona persona in personasMayoresDe30)
        {
            Console.WriteLine("Nombre: {0}, Apellido: {1}, Edad: {2}", persona.Nombre, persona.Apellido, persona.Edad);
        }

        // Creación de una cadena de texto con los nombres de las personas
        string nombresPersonas = string.Join(", ", personas.Select(p => p.Nombre));

        // Impresión de la cadena de texto con los nombres de las personas
        Console.WriteLine("Nombres de las personas: {0}", nombresPersonas);
    }
}

// Clase que representa a una persona
public class Persona
{
    // Propiedades de la clase
    public string Nombre { get; set; }
    public string Apellido { get; set; }
    public int Edad { get; set; }

    // Constructor de la clase
    public Persona(string nombre, string apellido, int edad)
    {
        Nombre = nombre;
        Apellido = apellido;
        Edad = edad;
    }
}
```

Explicación del código:

* La clase `Persona` representa a una persona con propiedades para su nombre, apellido y edad.
* La clase `ProgramaComplejo` es la clase principal del programa y contiene el método `Main` que es el punto de entrada del programa.
* En el método `Main` se declara una lista de personas, se agregan tres personas a la lista y luego se busca una persona por su nombre y apellido.
* A continuación, la lista de personas se ordena por edad y se imprimen los datos de las personas ordenadas por edad.
* Luego se crea una nueva lista de personas con las personas mayores de 30 años y se imprimen los datos de las personas mayores de 30 años.
* Por último, se crea una cadena de texto con los nombres de las personas y se imprime la cadena de texto con los nombres de las personas.