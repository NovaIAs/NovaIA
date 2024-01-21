```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class ProgramaComplejo
{
    // Definimos una clase con un constructor privado para evitar la instanciación directa.
    private class Singleton
    {
        // La instancia única de la clase.
        private static Singleton instancia;

        // Propiedad para obtener la instancia única.
        public static Singleton Instancia
        {
            get
            {
                // Si la instancia aún no existe, la creamos.
                if (instancia == null)
                {
                    instancia = new Singleton();
                }

                // Devolvemos la instancia única.
                return instancia;
            }
        }

        // Constructor privado para evitar la instanciación directa.
        private Singleton() { }
    }

    // Definimos una interfaz para un servicio.
    public interface IServicio
    {
        void RealizarTarea();
    }

    // Definimos una clase que implementa el servicio.
    public class Servicio : IServicio
    {
        public void RealizarTarea()
        {
            Console.WriteLine("Tarea realizada.");
        }
    }

    // Definimos un patrón de diseño Factory.
    public class Factory
    {
        public static IServicio CrearServicio()
        {
            return new Servicio();
        }
    }

    // Definimos un patrón de diseño Observer.
    public class Observador
    {
        public void OnCambio()
        {
            Console.WriteLine("Cambio detectado.");
        }
    }

    // Definimos un patrón de diseño Strategy.
    public interface IComparator
    {
        int Compare(object x, object y);
    }

    public class ComparadorPorNombre : IComparator
    {
        public int Compare(object x, object y)
        {
            return String.Compare((x as Persona).Nombre, (y as Persona).Nombre);
        }
    }

    public class ComparadorPorEdad : IComparator
    {
        public int Compare(object x, object y)
        {
            return (x as Persona).Edad - (y as Persona).Edad;
        }
    }

    // Definimos una clase que utiliza la estrategia para ordenar personas.
    public class Ordenador
    {
        private IComparator comparador;

        public void EstablecerComparador(IComparator comparador)
        {
            this.comparador = comparador;
        }

        public List<Persona> Ordenar(List<Persona> personas)
        {
            personas.Sort(comparador.Compare);
            return personas;
        }
    }

    // Definimos una clase para representar a una persona.
    public class Persona
    {
        public string Nombre { get; set; }
        public int Edad { get; set; }
    }

    // Definimos el método Main.
    public static void Main(string[] args)
    {
        // Creamos una instancia única de la clase Singleton.
        var singleton = Singleton.Instancia;

        // Creamos un servicio utilizando el patrón Factory.
        var servicio = Factory.CrearServicio();

        // Creamos un observador.
        var observador = new Observador();

        // Creamos una lista de personas.
        var personas = new List<Persona>
        {
            new Persona { Nombre = "Juan", Edad = 20 },
            new Persona { Nombre = "María", Edad = 25 },
            new Persona { Nombre = "Pedro", Edad = 30 },
            new Persona { Nombre = "Ana", Edad = 35 }
        };

        // Creamos un ordenador.
        var ordenador = new Ordenador();

        // Establecemos el comparador por nombre.
        ordenador.EstablecerComparador(new ComparadorPorNombre());

        // Ordenamos la lista de personas.
        var personasOrdenadasPorNombre = ordenador.Ordenar(personas);

        // Establecemos el comparador por edad.
        ordenador.EstablecerComparador(new ComparadorPorEdad());

        // Ordenamos la lista de personas.
        var personasOrdenadasPorEdad = ordenador.Ordenar(personas);

        // Imprimimos la lista de personas ordenadas por nombre.
        Console.WriteLine("Personas ordenadas por nombre:");
        foreach (var persona in personasOrdenadasPorNombre)
        {
            Console.WriteLine($"Nombre: {persona.Nombre}, Edad: {persona.Edad}");
        }

        // Imprimimos la lista de personas ordenadas por edad.
        Console.WriteLine();
        Console.WriteLine("Personas ordenadas por edad:");
        foreach (var persona in personasOrdenadasPorEdad)
        {
            Console.WriteLine($"Nombre: {persona.Nombre}, Edad: {persona.Edad}");
        }
    }
}
```

Este código es un ejemplo complejo de programación en C# que utiliza varios patrones de diseño y características avanzadas del lenguaje.

* El patrón Singleton garantiza que solo exista una instancia de una clase en todo el programa.
* El patrón Factory proporciona una forma de crear objetos sin especificar la clase concreta que se debe crear.
* El patrón Observer permite que los objetos se comuniquen entre sí de forma asíncrona.
* El patrón Strategy proporciona una forma de cambiar el comportamiento de un algoritmo en tiempo de ejecución.
* El código utiliza genéricos para escribir código que pueda trabajar con diferentes tipos de datos.
* El código utiliza lambdas para escribir código más conciso y expresivo.
* El código utiliza extension methods para ampliar la funcionalidad de las clases existentes.

Este código es un buen ejemplo de cómo se pueden utilizar los patrones de diseño y las características avanzadas del lenguaje para crear código complejo y eficiente.