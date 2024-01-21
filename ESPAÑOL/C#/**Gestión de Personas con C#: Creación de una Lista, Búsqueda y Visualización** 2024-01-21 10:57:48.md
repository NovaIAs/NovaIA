```c#
// Clase principal del programa
public class ProgramaPrincipal
{
    // Método principal del programa
    public static void Main(string[] args)
    {
        // Creamos una lista de objetos de tipo 'Persona'
        List<Persona> personas = new List<Persona>();

        // Creamos algunas personas y las añadimos a la lista
        personas.Add(new Persona("Juan", "Pérez", 25));
        personas.Add(new Persona("María", "Gómez", 30));
        personas.Add(new Persona("Pedro", "Rodríguez", 35));

        // Mostramos la lista de personas
        Console.WriteLine("Lista de personas:");
        foreach (Persona persona in personas)
        {
            Console.WriteLine("{0} {1} ({2} años)", persona.Nombre, persona.Apellido, persona.Edad);
        }

        // Buscamos una persona en la lista por su nombre y apellido
        Console.WriteLine("Ingrese el nombre y apellido de la persona que desea buscar:");
        string nombre = Console.ReadLine();
        string apellido = Console.ReadLine();

        Persona personaBuscada = personas.Find(p => p.Nombre == nombre && p.Apellido == apellido);

        // Mostramos la persona buscada
        if (personaBuscada != null)
        {
            Console.WriteLine("Persona encontrada:");
            Console.WriteLine("{0} {1} ({2} años)", personaBuscada.Nombre, personaBuscada.Apellido, personaBuscada.Edad);
        }
        else
        {
            Console.WriteLine("Persona no encontrada.");
        }
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
        this.Nombre = nombre;
        this.Apellido = apellido;
        this.Edad = edad;
    }
}
```

Explicación del código:

El código crea una lista de objetos de tipo 'Persona'. Luego, crea algunas personas y las añade a la lista.

A continuación, muestra la lista de personas en la consola.

Luego, pide al usuario que introduzca el nombre y apellido de la persona que desea buscar.

A continuación, busca la persona en la lista utilizando el método 'Find()' y la muestra en la consola.

Si la persona no se encuentra, muestra un mensaje en la consola.

El código utiliza las siguientes características de C#:

* Clases y objetos
* Listas
* Métodos
* Propiedades
* Constructores
* Operadores lambda
* Expresiones LINQ