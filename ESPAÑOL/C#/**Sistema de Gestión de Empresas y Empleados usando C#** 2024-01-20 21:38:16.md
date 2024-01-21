```c#
// Clase de Persona
public class Persona
{
    public string Nombre { get; set; }
    public string Apellido { get; set; }
    public int Edad { get; set; }
}

// Clase de Dirección
public class Direccion
{
    public string Calle { get; set; }
    public string Ciudad { get; set; }
    public string País { get; set; }
}

// Clase de Empresa
public class Empresa
{
    public string Nombre { get; set; }
    public string Dirección { get; set; }
    public List<Persona> Empleados { get; set; }
}

// Clase de Programa Principal
public class Program
{
    public static void Main(string[] args)
    {
        // Crear una lista de personas
        List<Persona> personas = new List<Persona>()
        {
            new Persona() { Nombre = "Juan", Apellido = "Pérez", Edad = 25 },
            new Persona() { Nombre = "María", Apellido = "Gómez", Edad = 30 },
            new Persona() { Nombre = "Pedro", Apellido = "Rodríguez", Edad = 35 }
        };

        // Crear una lista de direcciones
        List<Direccion> direcciones = new List<Direccion>()
        {
            new Direccion() { Calle = "Calle 1", Ciudad = "Ciudad 1", País = "País 1" },
            new Direccion() { Calle = "Calle 2", Ciudad = "Ciudad 2", País = "País 2" },
            new Direccion() { Calle = "Calle 3", Ciudad = "Ciudad 3", País = "País 3" }
        };

        // Crear una lista de empresas
        List<Empresa> empresas = new List<Empresa>()
        {
            new Empresa() { Nombre = "Empresa 1", Dirección = "Dirección 1", Empleados = personas },
            new Empresa() { Nombre = "Empresa 2", Dirección = "Dirección 2", Empleados = personas },
            new Empresa() { Nombre = "Empresa 3", Dirección = "Dirección 3", Empleados = personas }
        };

        // Imprimir la información de las empresas
        foreach (Empresa empresa in empresas)
        {
            Console.WriteLine("Empresa: {0}", empresa.Nombre);
            Console.WriteLine("Dirección: {0}", empresa.Dirección);
            Console.WriteLine("Empleados:");
            foreach (Persona empleado in empresa.Empleados)
            {
                Console.WriteLine("\tNombre: {0} {1}", empleado.Nombre, empleado.Apellido);
                Console.WriteLine("\tEdad: {0}", empleado.Edad);
            }
        }
    }
}
```

Este código es una aplicación que crea una lista de personas, una lista de direcciones y una lista de empresas. Cada empresa tiene una lista de empleados, cada empleado tiene un nombre, un apellido y una edad, y cada dirección tiene una calle, una ciudad y un país.

El código recorre la lista de empresas y para cada empresa imprime el nombre, la dirección y la lista de empleados.

Este código es un ejemplo de cómo se puede utilizar el lenguaje de programación C# para crear una aplicación compleja.