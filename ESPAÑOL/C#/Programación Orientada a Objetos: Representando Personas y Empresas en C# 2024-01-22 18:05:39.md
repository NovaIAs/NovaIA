```csharp
// Clase que representa una persona.
public class Persona
{
    // Propiedades de la persona.
    public string Nombre { get; set; }
    public int Edad { get; set; }
    public decimal Salario { get; set; }

    // Constructor de la persona.
    public Persona(string nombre, int edad, decimal salario)
    {
        Nombre = nombre;
        Edad = edad;
        Salario = salario;
    }

    // Método que devuelve una cadena con la información de la persona.
    public override string ToString()
    {
        return $"Nombre: {Nombre}, Edad: {Edad}, Salario: {Salario}";
    }
}

// Clase que representa una empresa.
public class Empresa
{
    // Propiedades de la empresa.
    public string Nombre { get; set; }
    public List<Persona> Empleados { get; set; }

    // Constructor de la empresa.
    public Empresa(string nombre)
    {
        Nombre = nombre;
        Empleados = new List<Persona>();
    }

    // Método que añade un empleado a la empresa.
    public void AñadirEmpleado(Persona empleado)
    {
        Empleados.Add(empleado);
    }

    // Método que devuelve una cadena con la información de la empresa y sus empleados.
    public override string ToString()
    {
        StringBuilder sb = new StringBuilder();
        sb.AppendLine($"Nombre: {Nombre}");
        sb.AppendLine("Empleados:");
        foreach (Persona empleado in Empleados)
        {
            sb.AppendLine(empleado.ToString());
        }
        return sb.ToString();
    }
}

// Clase principal del programa.
public class Program
{
    // Método principal del programa.
    public static void Main(string[] args)
    {
        // Crear una lista de personas.
        List<Persona> personas = new List<Persona>
        {
            new Persona("Juan", 25, 1000),
            new Persona("María", 30, 1500),
            new Persona("Pedro", 35, 2000)
        };

        // Crear una empresa.
        Empresa empresa = new Empresa("Acme Corporation");

        // Añadir las personas a la empresa.
        foreach (Persona persona in personas)
        {
            empresa.AñadirEmpleado(persona);
        }

        // Mostrar la información de la empresa y sus empleados.
        Console.WriteLine(empresa.ToString());
    }
}
```

Este código define dos clases, `Persona` y `Empresa`, que representan una persona y una empresa, respectivamente. Cada clase tiene sus propias propiedades y métodos. La clase `Persona` tiene las propiedades `Nombre`, `Edad` y `Salario`, y el método `ToString()` que devuelve una cadena con la información de la persona. La clase `Empresa` tiene las propiedades `Nombre` y `Empleados`, y los métodos `AñadirEmpleado()` y `ToString()`.

El método `Main()` del programa crea una lista de personas y una empresa. Luego, añade las personas a la empresa y muestra la información de la empresa y sus empleados.

Este código es complejo porque define dos clases con sus propias propiedades y métodos, utiliza una lista para almacenar las personas y utiliza un bucle `foreach` para recorrer la lista de personas y añadirlas a la empresa. También utiliza el método `ToString()` para devolver una cadena con la información de la persona y la empresa. Este código es un ejemplo de cómo se pueden utilizar las clases y los objetos en C# para representar y manipular datos.