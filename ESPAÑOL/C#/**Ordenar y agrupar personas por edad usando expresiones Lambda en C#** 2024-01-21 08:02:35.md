```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejoCsharp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Definimos una clase Persona con sus propiedades Nombre y Edad
            class Persona
            {
                public string Nombre { get; set; }
                public int Edad { get; set; }
            }

            //Creamos una lista de personas
            List<Persona> personas = new List<Persona>()
            {
                new Persona { Nombre = "Juan", Edad = 20 },
                new Persona { Nombre = "María", Edad = 25 },
                new Persona { Nombre = "Pedro", Edad = 30 },
                new Persona { Nombre = "Ana", Edad = 35 },
                new Persona { Nombre = "Luis", Edad = 40 }
            };

            //Definimos una función para ordenar la lista por edad de forma descendente
            Func<Persona, Persona, int> ordenarPorEdad = (p1, p2) => p2.Edad.CompareTo(p1.Edad);

            //Ordenamos la lista usando la función definida anteriormente
            personas.Sort(ordenarPorEdad);

            //Imprimimos la lista ordenada por edad
            foreach (Persona persona in personas)
            {
                Console.WriteLine($"{persona.Nombre} tiene {persona.Edad} años");
            }

            //Definimos una función para agrupar la lista por rangos de edad
            Func<Persona, int> agruparPorEdad = (p) =>
            {
                if (p.Edad < 20)
                {
                    return 0;
                }
                else if (p.Edad >= 20 && p.Edad < 30)
                {
                    return 1;
                }
                else if (p.Edad >= 30 && p.Edad < 40)
                {
                    return 2;
                }
                else
                {
                    return 3;
                }
            };

            //Agrupamos la lista por rangos de edad
            var grupos = personas.GroupBy(agruparPorEdad);

            //Imprimimos los grupos de personas por rangos de edad
            foreach (var grupo in grupos)
            {
                Console.WriteLine($"Rango de edad: {grupo.Key}");
                foreach (Persona persona in grupo)
                {
                    Console.WriteLine($"{persona.Nombre} tiene {persona.Edad} años");
                }
                Console.WriteLine();
            }

        }
    }
}
```

Explicación del código:

1. Definimos una clase `Persona` con dos propiedades: `Nombre` y `Edad`.

2. Creamos una lista de personas y la llenamos con algunos datos.

3. Definimos una función para ordenar la lista de personas por edad de forma descendente.

4. Ordenamos la lista usando la función definida anteriormente.

5. Imprimimos la lista ordenada por edad.

6. Definimos una función para agrupar la lista de personas por rangos de edad.

7. Agrupamos la lista usando la función definida anteriormente.

8. Imprimimos los grupos de personas por rangos de edad.