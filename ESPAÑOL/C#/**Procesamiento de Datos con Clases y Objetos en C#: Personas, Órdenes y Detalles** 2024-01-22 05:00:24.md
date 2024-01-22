```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static void Main(string[] args)
    {
        // Creación de una lista de objetos de tipo Persona.
        List<Persona> personas = new List<Persona>();

        // Añadiendo algunos objetos de tipo Persona a la lista.
        personas.Add(new Persona("Juan", "Pérez", 25));
        personas.Add(new Persona("Ana", "Gómez", 30));
        personas.Add(new Persona("Luis", "Rodríguez", 35));

        // Creación de una lista de objetos de tipo Orden.
        List<Orden> ordenes = new List<Orden>();

        // Añadiendo algunos objetos de tipo Orden a la lista.
        ordenes.Add(new Orden(1, "Orden 1", "Cliente 1", 100));
        ordenes.Add(new Orden(2, "Orden 2", "Cliente 2", 200));
        ordenes.Add(new Orden(3, "Orden 3", "Cliente 3", 300));

        // Creación de una lista de objetos de tipo DetalleOrden.
        List<DetalleOrden> detallesOrdenes = new List<DetalleOrden>();

        // Añadiendo algunos objetos de tipo DetalleOrden a la lista.
        detallesOrdenes.Add(new DetalleOrden(1, 1, "Producto 1", 10, 100));
        detallesOrdenes.Add(new DetalleOrden(2, 1, "Producto 2", 20, 200));
        detallesOrdenes.Add(new DetalleOrden(3, 2, "Producto 3", 30, 300));

        // Creación de una clase Persona.
        class Persona
        {
            public string Nombre { get; set; }
            public string Apellido { get; set; }
            public int Edad { get; set; }

            public Persona(string nombre, string apellido, int edad)
            {
                Nombre = nombre;
                Apellido = apellido;
                Edad = edad;
            }
        }

        // Creación de una clase Orden.
        class Orden
        {
            public int Id { get; set; }
            public string Nombre { get; set; }
            public string Cliente { get; set; }
            public double Total { get; set; }

            public Orden(int id, string nombre, string cliente, double total)
            {
                Id = id;
                Nombre = nombre;
                Cliente = cliente;
                Total = total;
            }
        }

        // Creación de una clase DetalleOrden.
        class DetalleOrden
        {
            public int Id { get; set; }
            public int OrdenId { get; set; }
            public string Producto { get; set; }
            public int Cantidad { get; set; }
            public double Precio { get; set; }

            public DetalleOrden(int id, int ordenId, string producto, int cantidad, double precio)
            {
                Id = id;
                OrdenId = ordenId;
                Producto = producto;
                Cantidad = cantidad;
                Precio = precio;
            }
        }

        // Mostrando los datos de las personas, órdenes y detalles de órdenes.
        Console.WriteLine("Personas:");
        foreach (Persona persona in personas)
        {
            Console.WriteLine($"{persona.Nombre} {persona.Apellido} ({persona.Edad} años)");
        }

        Console.WriteLine("\nÓrdenes:");
        foreach (Orden orden in ordenes)
        {
            Console.WriteLine($"Orden {orden.Id}: {orden.Nombre} ({orden.Cliente} - ${orden.Total})");
        }

        Console.WriteLine("\nDetalles de Órdenes:");
        foreach (DetalleOrden detalleOrden in detallesOrdenes)
        {
            Console.WriteLine($"Detalle Orden {detalleOrden.Id}: {detalleOrden.Producto} ({detalleOrden.Cantidad} x ${detalleOrden.Precio})");
        }
    }
}
```

**Explicación del código:**

Este código es un programa en C# que crea tres clases: Persona, Orden y DetalleOrden. Estas clases se utilizan para crear objetos que representan personas, órdenes y detalles de órdenes.

El programa también crea tres listas: personas, ordenes y detallesOrdenes. Estas listas se utilizan para almacenar objetos de los tipos Persona, Orden y DetalleOrden, respectivamente.

El programa luego añade algunos objetos de cada tipo a las listas.

Finalmente, el programa muestra los datos de las personas, órdenes y detalles de órdenes.

Este código es un ejemplo de cómo se pueden utilizar las clases y los objetos en C# para representar datos y realizar operaciones sobre esos datos.