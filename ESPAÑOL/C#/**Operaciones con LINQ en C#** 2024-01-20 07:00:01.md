```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace CodigoComplejo
{
    class Program
    {
        static void Main(string[] args)
        {
            // Crear una lista de objetos de tipo "Persona"
            List<Persona> personas = new List<Persona>
            {
                new Persona { Nombre = "Juan", Apellido = "García", Edad = 25 },
                new Persona { Nombre = "María", Apellido = "Pérez", Edad = 30 },
                new Persona { Nombre = "Pedro", Apellido = "López", Edad = 35 },
                new Persona { Nombre = "Ana", Apellido = "Fernández", Edad = 40 },
                new Persona { Nombre = "José", Apellido = "González", Edad = 45 }
            };

            // Utilizar LINQ para filtrar la lista de personas mayores de 30 años
            var personasMayoresDe30 = personas.Where(p => p.Edad > 30);

            // Imprimir los nombres de las personas mayores de 30 años
            Console.WriteLine("Personas mayores de 30 años:");
            foreach (var persona in personasMayoresDe30)
            {
                Console.WriteLine($"Nombre: {persona.Nombre} {persona.Apellido}");
            }

            // Utilizar LINQ para ordenar la lista de personas por edad
            var personasOrdenadasPorEdad = personas.OrderBy(p => p.Edad);

            // Imprimir los nombres de las personas ordenadas por edad
            Console.WriteLine("\nPersonas ordenadas por edad:");
            foreach (var persona in personasOrdenadasPorEdad)
            {
                Console.WriteLine($"Nombre: {persona.Nombre} {persona.Apellido}");
            }

            // Utilizar LINQ para agrupar la lista de personas por apellido
            var personasAgrupadasPorApellido = personas.GroupBy(p => p.Apellido);

            // Imprimir los apellidos y los nombres de las personas agrupadas por apellido
            Console.WriteLine("\nPersonas agrupadas por apellido:");
            foreach (var grupo in personasAgrupadasPorApellido)
            {
                Console.WriteLine($"Apellido: {grupo.Key}");
                foreach (var persona in grupo)
                {
                    Console.WriteLine($"Nombre: {persona.Nombre}");
                }
            }

            // Utilizar LINQ para calcular la edad promedio de las personas
            var edadPromedio = personas.Average(p => p.Edad);

            // Imprimir la edad promedio de las personas
            Console.WriteLine($"\nEdad promedio: {edadPromedio:N2} años");

            // Utilizar LINQ para encontrar la persona más joven y la más vieja
            var personaMasJoven = personas.MinBy(p => p.Edad);
            var personaMasVieja = personas.MaxBy(p => p.Edad);

            // Imprimir el nombre de la persona más joven y de la más vieja
            Console.WriteLine("\nPersona más joven:");
            Console.WriteLine($"Nombre: {personaMasJoven.Nombre} {personaMasJoven.Apellido}");
            Console.WriteLine("\nPersona más vieja:");
            Console.WriteLine($"Nombre: {personaMasVieja.Nombre} {personaMasVieja.Apellido}");
        }

        // Definir la clase "Persona"
        public class Persona
        {
            public string Nombre { get; set; }
            public string Apellido { get; set; }
            public int Edad { get; set; }
        }
    }
}
```

Este código utiliza LINQ para realizar varias operaciones sobre una lista de objetos de tipo "Persona".

En primer lugar, se utiliza LINQ para filtrar la lista de personas mayores de 30 años. Esto se hace utilizando el método `Where()` de LINQ, que toma una expresión lambda como argumento. La expresión lambda comprueba si la edad de la persona es mayor que 30 años. Las personas que cumplen esta condición se añaden a la lista de personas mayores de 30 años.

A continuación, se utiliza LINQ para ordenar la lista de personas por edad. Esto se hace utilizando el método `OrderBy()` de LINQ, que toma una expresión lambda como argumento. La expresión lambda devuelve la edad de la persona. Las personas se ordenan en orden ascendente por edad.

Luego, se utiliza LINQ para agrupar la lista de personas por apellido. Esto se hace utilizando el método `GroupBy()` de LINQ, que toma una expresión lambda como argumento. La expresión lambda devuelve el apellido de la persona. Las personas se agrupan en función de su apellido.

Después, se utiliza LINQ para calcular la edad promedio de las personas. Esto se hace utilizando el método `Average()` de LINQ, que toma una expresión lambda como argumento. La expresión lambda devuelve la edad de la persona. La edad promedio se calcula sumando las edades de todas las personas y dividiendo el resultado por el número de personas.

Finalmente, se utiliza LINQ para encontrar la persona más joven y la más vieja. Esto se hace utilizando los métodos `MinBy()` y `MaxBy()` de LINQ, que toman una expresión lambda como argumento. La expresión lambda devuelve la edad de la persona. La persona más joven es la que tiene la edad más baja, mientras que la persona más vieja es la que tiene la edad más alta.

El código se imprime la lista de personas mayores de 30 años, la lista de personas ordenadas por edad, la lista de personas agrupadas por apellido, la edad promedio de las personas, y la persona más joven y la más vieja.