```c#
// Definición de clases

public class Persona
{
    public string Nombre { get; set; }
    public string Apellido { get; set; }
    public int Edad { get; set; }
}

public class Direccion
{
    public string Calle { get; set; }
    public int Numero { get; set; }
    public string Ciudad { get; set; }
}

public class Telefono
{
    public string Tipo { get; set; }
    public string Numero { get; set; }
}

// Definición de la clase principal

public class ProgramaPrincipal
{
    // Método principal (punto de entrada)

    public static void Main(string[] args)
    {
        // Creación de instancias de las clases

        Persona persona = new Persona();
        Direccion direccion = new Direccion();
        Telefono telefono = new Telefono();

        // Asignación de valores a las propiedades

        persona.Nombre = "Juan";
        persona.Apellido = "García";
        persona.Edad = 30;

        direccion.Calle = "Calle Principal";
        direccion.Numero = 123;
        direccion.Ciudad = "Madrid";

        telefono.Tipo = "Móvil";
        telefono.Numero = "600 123 456";

        // Impresión de los valores de las propiedades

        Console.WriteLine("Nombre: {0} {1}", persona.Nombre, persona.Apellido);
        Console.WriteLine("Edad: {0}", persona.Edad);

        Console.WriteLine("Dirección:");
        Console.WriteLine("\tCalle: {0}", direccion.Calle);
        Console.WriteLine("\tNúmero: {0}", direccion.Numero);
        Console.WriteLine("\tCiudad: {0}", direccion.Ciudad);

        Console.WriteLine("Teléfono:");
        Console.WriteLine("\tTipo: {0}", telefono.Tipo);
        Console.WriteLine("\tNúmero: {0}", telefono.Numero);
    }
}
```

Explicación del código:

* Se definen las clases `Persona`, `Direccion` y `Telefono` con sus respectivas propiedades.
* En el método principal `Main`, se crea una instancia de cada clase y se asignan valores a sus propiedades.
* Finalmente, se imprimen los valores de las propiedades de las instancias en la consola.