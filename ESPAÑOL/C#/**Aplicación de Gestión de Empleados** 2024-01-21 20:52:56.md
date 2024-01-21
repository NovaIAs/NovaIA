```c#
// Crear una clase para representar un empleado
public class Empleado
{
    // Atributos de la clase
    public int Id { get; set; }
    public string Nombre { get; set; }
    public string Apellido { get; set; }
    public double Salario { get; set; }
    public DateTime FechaContratacion { get; set; }

    // Constructor de la clase
    public Empleado(int id, string nombre, string apellido, double salario, DateTime fechaContratacion)
    {
        Id = id;
        Nombre = nombre;
        Apellido = apellido;
        Salario = salario;
        FechaContratacion = fechaContratacion;
    }

    // Métodos de la clase
    public override string ToString()
    {
        return $"Id: {Id}, Nombre: {Nombre}, Apellido: {Apellido}, Salario: {Salario}";
    }
}

// Crear una lista de empleados
List<Empleado> empleados = new List<Empleado>();

// Agregar algunos empleados a la lista
empleados.Add(new Empleado(1, "Juan", "Pérez", 2000, new DateTime(2022, 1, 1)));
empleados.Add(new Empleado(2, "María", "González", 3000, new DateTime(2021, 3, 1)));
empleados.Add(new Empleado(3, "Pedro", "García", 4000, new DateTime(2020, 5, 1)));

// Ordenar la lista de empleados por salario
empleados.Sort((e1, e2) => e1.Salario.CompareTo(e2.Salario));

// Imprimir la lista de empleados
Console.WriteLine("Lista de Empleados:");
foreach (Empleado empleado in empleados)
{
    Console.WriteLine(empleado);
}

// Filtrar la lista de empleados por salario mayor a 3000
var empleadosFiltrados = empleados.Where(e => e.Salario > 3000);

// Imprimir la lista de Empleados filtrados
Console.WriteLine("Lista de Empleados con Salario Mayor a 3000:");
foreach (Empleado empleado in empleadosFiltrados)
{
    Console.WriteLine(empleado);
}

// Agrupar la lista de empleados por año de contratación
var empleadosAgrupados = empleados.GroupBy(e => e.FechaContratacion.Year);

// Imprimir la lista de Empleados agrupados por año de contratación
Console.WriteLine("Lista de Empleados Agrupados por Año de Contratación:");
foreach (var agrupacion in empleadosAgrupados)
{
    Console.WriteLine($"Año: {agrupacion.Key}");
    foreach (Empleado empleado in agrupacion)
    {
        Console.WriteLine(empleado);
    }
}
```

Explicación:

* El código crea una clase `Empleado` que representa a un empleado con atributos como `Id`, `Nombre`, `Apellido`, `Salario` y `FechaContratacion`.
* Luego, se crea una lista de empleados y se agregan algunos empleados a la lista.
* A continuación, se ordena la lista de empleados por salario.
* A continuación, se filtra la lista de empleados por salario mayor a 3000.
* Luego, se agrupa la lista de empleados por año de contratación.
* Finalmente, se imprime la lista de empleados, la lista filtrada y la lista agrupada.