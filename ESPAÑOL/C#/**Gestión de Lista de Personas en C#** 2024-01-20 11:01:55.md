```c#
// Clase que representa una persona.
public class Persona
{
    public string Nombre { get; set; }
    public string Apellido { get; set; }
    public int Edad { get; set; }

    // Constructor.
    public Persona(string nombre, string apellido, int edad)
    {
        Nombre = nombre;
        Apellido = apellido;
        Edad = edad;
    }

    // Método que devuelve el nombre completo de la persona.
    public string GetNombreCompleto()
    {
        return Nombre + " " + Apellido;
    }
}

// Clase que representa una lista de personas.
public class ListaPersonas
{
    private List<Persona> personas;

    // Constructor.
    public ListaPersonas()
    {
        personas = new List<Persona>();
    }

    // Método que añade una persona a la lista.
    public void AgregarPersona(Persona persona)
    {
        personas.Add(persona);
    }

    // Método que devuelve la lista de personas.
    public List<Persona> GetPersonas()
    {
        return personas;
    }
}

// Clase que representa un programa para gestionar una lista de personas.
public class Programa
{
    // Método principal.
    public static void Main(string[] args)
    {
        // Creamos una lista de personas.
        ListaPersonas listaPersonas = new ListaPersonas();

        // Añadimos algunas personas a la lista.
        listaPersonas.AgregarPersona(new Persona("Juan", "García", 25));
        listaPersonas.AgregarPersona(new Persona("María", "López", 30));
        listaPersonas.AgregarPersona(new Persona("Pedro", "Sánchez", 35));

        // Recorremos la lista de personas.
        foreach (Persona persona in listaPersonas.GetPersonas())
        {
            // Mostramos el nombre completo de la persona.
            Console.WriteLine(persona.GetNombreCompleto());
        }
    }
}
```

Este código es un programa sencillo para gestionar una lista de personas. El programa crea una lista de personas, añade algunas personas a la lista y luego recorre la lista y muestra el nombre completo de cada persona.

El código está bien documentado y es fácil de seguir. El uso de la clase `ListaPersonas` y del método `GetPersonas()` permite gestionar la lista de personas de forma sencilla y eficiente.

El código también es extensible. Por ejemplo, se podría añadir un método para eliminar personas de la lista, o un método para buscar personas en la lista.