```c#
// Definición de la clase Persona
public class Persona
{
    public string Nombre { get; set; }
    public int Edad { get; set; }
    public string Profesion { get; set; }

    public Persona(string nombre, int edad, string profesion)
    {
        Nombre = nombre;
        Edad = edad;
        Profesion = profesion;
    }

    public override string ToString()
    {
        return $"Nombre: {Nombre}, Edad: {Edad}, Profesión: {Profesion}";
    }
}

// Definición de la clase Empresa
public class Empresa
{
    public string Nombre { get; set; }
    public List<Persona> Empleados { get; set; }

    public Empresa(string nombre)
    {
        Nombre = nombre;
        Empleados = new List<Persona>();
    }

    public void AgregarEmpleado(Persona empleado)
    {
        Empleados.Add(empleado);
    }

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

// Código principal
// Crear una lista de personas
List<Persona> personas = new List<Persona>()
{
    new Persona("Juan", 25, "Ingeniero"),
    new Persona("María", 30, "Abogada"),
    new Persona("Pedro", 35, "Médico"),
};

// Crear una empresa y agregar empleados
Empresa empresa = new Empresa("Acme Corporation");
empresa.AgregarEmpleado(personas[0]);
empresa.AgregarEmpleado(personas[1]);
empresa.AgregarEmpleado(personas[2]);

// Imprimir la información de la empresa
Console.WriteLine(empresa);
```

Explicación:

* La clase `Persona` representa a una persona con su nombre, edad y profesión.
* La clase `Empresa` representa a una empresa con su nombre y una lista de empleados.
* El código principal crea una lista de personas, crea una empresa y agrega los empleados a la empresa.
* Por último, se imprime la información de la empresa, que incluye el nombre de la empresa y la lista de empleados.

 Este es un ejemplo de un código más complejo en C# que incluye múltiples clases, listas y un método para agregar empleados a una empresa. El código también utiliza la sobrecarga de métodos para imprimir la información de una persona y una empresa.