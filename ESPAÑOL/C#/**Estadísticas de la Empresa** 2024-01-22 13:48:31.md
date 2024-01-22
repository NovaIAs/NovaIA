```c#
using System;
using System.Collections.Generic;

class Program
{
    public class Empresa
    {
        public string Nombre { get; set; }
        public List<Empleado> Empleados { get; set; }
    }

    public class Empleado
    {
        public string Nombre { get; set; }
        public string Apellidos { get; set; }
        public DateTime FechaNacimiento { get; set; }
        public string Puesto { get; set; }
        public double Sueldo { get; set; }
    }

    static void Main(string[] args)
    {
        Empresa empresa = new Empresa
        {
            Nombre = "Acme Corporation",
            Empleados = new List<Empleado>
            {
                new Empleado
                {
                    Nombre = "Juan",
                    Apellidos = "Pérez",
                    FechaNacimiento = new DateTime(1980, 1, 1),
                    Puesto = "Gerente",
                    Sueldo = 50000
                },
                new Empleado
                {
                    Nombre = "María",
                    Apellidos = "Martínez",
                    FechaNacimiento = new DateTime(1985, 2, 1),
                    Puesto = "Ingeniero",
                    Sueldo = 40000
                },
                new Empleado
                {
                    Nombre = "Pedro",
                    Apellidos = "López",
                    FechaNacimiento = new DateTime(1990, 3, 1),
                    Puesto = "Contador",
                    Sueldo = 30000
                }
            }
        };

        // Obtener el empleado con mayor sueldo
        Empleado empleadoConMayorSueldo = empresa.Empleados.OrderByDescending(e => e.Sueldo).First();

        // Obtener la lista de empleados que ganan más de 40000
        List<Empleado> empleadosConSueldoMayorA40000 = empresa.Empleados.Where(e => e.Sueldo > 40000).ToList();

        // Obtener el número total de empleados
        int numeroTotalDeEmpleados = empresa.Empleados.Count;

        // Obtener el sueldo total de todos los empleados
        double sueldoTotalDeTodosLosEmpleados = empresa.Empleados.Sum(e => e.Sueldo);

        // Obtener el sueldo medio de todos los empleados
        double sueldoMedioDeTodosLosEmpleados = sueldoTotalDeTodosLosEmpleados / numeroTotalDeEmpleados;

        // Mostrar los resultados por consola
        Console.WriteLine("El empleado con mayor sueldo es:");
        Console.WriteLine($"{empleadoConMayorSueldo.Nombre} {empleadoConMayorSueldo.Apellidos}");

        Console.WriteLine("La lista de empleados que ganan más de 40000 es:");
        foreach (Empleado empleado in empleadosConSueldoMayorA40000)
        {
            Console.WriteLine($"{empleado.Nombre} {empleado.Apellidos}");
        }

        Console.WriteLine("El número total de empleados es:");
        Console.WriteLine(numeroTotalDeEmpleados);

        Console.WriteLine("El sueldo total de todos los empleados es:");
        Console.WriteLine(sueldoTotalDeTodosLosEmpleados);

        Console.WriteLine("El sueldo medio de todos los empleados es:");
        Console.WriteLine(sueldoMedioDeTodosLosEmpleados);
    }
}
```

Explicación del código:

* Se definen las clases `Empresa` y `Empleado` que representan una empresa y un empleado, respectivamente.
* Se crea una instancia de la clase `Empresa` llamada `empresa`.
* Se añade una lista de empleados a la empresa.
* Se obtienen los datos del empleado con mayor sueldo, la lista de empleados que ganan más de 40000, el número total de empleados, el sueldo total de todos los empleados y el sueldo medio de todos los empleados.
* Se muestran los resultados por consola.