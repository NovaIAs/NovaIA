```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CódigoComplejoCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Definir una clase personalizada para representar una persona.
            public class Persona
            {
                public string Nombre { get; set; }
                public int Edad { get; set; }
                public string Ocupación { get; set; }

                public Persona(string nombre, int edad, string ocupación)
                {
                    Nombre = nombre;
                    Edad = edad;
                    Ocupación = ocupación;
                }
            }

            // Definir una clase personalizada para representar una lista de personas.
            public class ListaDePersonas
            {
                private List<Persona> _listaDePersonas;

                public ListaDePersonas()
                {
                    _listaDePersonas = new List<Persona>();
                }

                public void AgregarPersona(Persona persona)
                {
                    _listaDePersonas.Add(persona);
                }

                public void EliminarPersona(Persona persona)
                {
                    _listaDePersonas.Remove(persona);
                }

                public Persona ObtenerPersonaPorNombre(string nombre)
                {
                    return _listaDePersonas.FirstOrDefault(p => p.Nombre == nombre);
                }

                public List<Persona> ObtenerTodasLasPersonas()
                {
                    return _listaDePersonas;
                }
            }

            // Crear una instancia de la clase ListaDePersonas.
            ListaDePersonas listaDePersonas = new ListaDePersonas();

            // Agregar algunas personas a la lista.
            listaDePersonas.AgregarPersona(new Persona("Juan", 25, "Ingeniero"));
            listaDePersonas.AgregarPersona(new Persona("María", 30, "Doctora"));
            listaDePersonas.AgregarPersona(new Persona("Pedro", 35, "Abogado"));

            // Obtener una persona de la lista por su nombre.
            Persona persona = listaDePersonas.ObtenerPersonaPorNombre("Juan");

            // Mostrar la información de la persona en la consola.
            Console.WriteLine("Nombre: {0}", persona.Nombre);
            Console.WriteLine("Edad: {0}", persona.Edad);
            Console.WriteLine("Ocupación: {0}", persona.Ocupación);

            // Obtener todas las personas de la lista.
            List<Persona> todasLasPersonas = listaDePersonas.ObtenerTodasLasPersonas();

            // Mostrar la información de todas las personas en la consola.
            foreach (Persona persona in todasLasPersonas)
            {
                Console.WriteLine("Nombre: {0}", persona.Nombre);
                Console.WriteLine("Edad: {0}", persona.Edad);
                Console.WriteLine("Ocupación: {0}", persona.Ocupación);
                Console.WriteLine();
            }

            // Eliminar una persona de la lista.
            listaDePersonas.EliminarPersona(persona);

            // Obtener todas las personas de la lista.
            todasLasPersonas = listaDePersonas.ObtenerTodasLasPersonas();

            // Mostrar la información de todas las personas en la consola.
            foreach (Persona persona in todasLasPersonas)
            {
                Console.WriteLine("Nombre: {0}", persona.Nombre);
                Console.WriteLine("Edad: {0}", persona.Edad);
                Console.WriteLine("Ocupación: {0}", persona.Ocupación);
                Console.WriteLine();
            }

            // Esperar a que el usuario presione una tecla para salir.
            Console.ReadKey();
        }
    }
}
```

Explicación:

1. **Clases personalizadas:** Se definen dos clases personalizadas: `Persona` y `ListaDePersonas`. La clase `Persona` representa a una persona con sus atributos básicos como nombre, edad y ocupación. La clase `ListaDePersonas` representa una lista de personas y proporciona métodos para agregar, eliminar y obtener personas de la lista.
2. **Instanciar la clase `ListaDePersonas`:** Se crea una instancia de la clase `ListaDePersonas` llamada `listaDePersonas`.
3. **Agregar personas a la lista:** Se agregan tres personas a la lista utilizando el método `AgregarPersona`.
4. **Obtener una persona de la lista:** Se obtiene una persona de la lista por su nombre utilizando el método `ObtenerPersonaPorNombre`.
5. **Mostrar la información de la persona:** Se muestra la información de la persona obtenida en la consola.
6. **Obtener todas las personas de la lista:** Se obtienen todas las personas de la lista utilizando el método `ObtenerTodasLasPersonas`.
7. **Mostrar la información de todas las personas:** Se muestra la información de todas las personas obtenidas en la consola.
8. **Eliminar una persona de la lista:** Se elimina una persona de la lista utilizando el método `EliminarPersona`.
9. **Obtener todas las personas de la lista:** Se obtienen todas las personas de la lista utilizando el método `ObtenerTodasLasPersonas`.
10. **Mostrar la información de todas las personas:** Se muestran la información de todas las personas obtenidas en la consola.
11. **Esperar a que el usuario presione una tecla para salir:** Se espera a que el usuario presione una tecla para salir del programa.