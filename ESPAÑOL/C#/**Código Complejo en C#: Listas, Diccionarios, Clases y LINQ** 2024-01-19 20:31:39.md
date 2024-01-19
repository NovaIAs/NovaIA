```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejo
{
    class Program
    {
        static void Main(string[] args)
        {
            // Definir una lista de números enteros.
            List<int> numeros = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

            // Definir un diccionario de claves y valores de string.
            Dictionary<string, string> diccionario = new Dictionary<string, string>()
            {
                { "Clave1", "Valor1" },
                { "Clave2", "Valor2" },
                { "Clave3", "Valor3" }
            };

            // Definir una clase con propiedades y métodos.
            class Persona
            {
                public string Nombre { get; set; }
                public int Edad { get; set; }

                public string ObtenerNombreCompleto()
                {
                    return Nombre + " " + Edad;
                }
            }

            // Crear un objeto de la clase Persona.
            Persona persona1 = new Persona() { Nombre = "Juan", Edad = 25 };

            // Usar LINQ para filtrar la lista de números enteros.
            var numerosFiltrados = numeros.Where(n => n % 2 == 0);

            // Usar LINQ para ordenar el diccionario de claves y valores de string.
            var diccionarioOrdenado = diccionario.OrderBy(d => d.Key);

            // Usar LINQ para agrupar la lista de números enteros por su valor.
            var numerosAgrupados = numeros.GroupBy(n => n);

            // Usar LINQ para unir la lista de números enteros y el diccionario de claves y valores de string.
            var numerosYDiccionario = numeros.Join(diccionario, n => n, d => d.Key, (n, d) => new { Numero = n, Clave = d.Key, Valor = d.Value });

            // Usar LINQ para seleccionar propiedades de una lista de objetos de la clase Persona.
            var nombresPersonas = persona1.Select(p => p.Nombre);

            // Usar LINQ para ejecutar una consulta compleja en una lista de objetos de la clase Persona.
            var personasFiltradas = persona1.Where(p => p.Edad > 18).Select(p => p.Nombre);

            // Usar LINQ para ejecutar una consulta compleja en una lista de objetos de la clase Persona y un diccionario de claves y valores de string.
            var personasYDiccionario = persona1.Join(diccionario, p => p.Nombre, d => d.Key, (p, d) => new { Nombre = p.Nombre, Edad = p.Edad, Clave = d.Key, Valor = d.Value });

            // Mostrar los resultados de las consultas LINQ.
            Console.WriteLine("Números filtrados:");
            foreach (int numero in numerosFiltrados)
            {
                Console.WriteLine(numero);
            }

            Console.WriteLine("Diccionario ordenado:");
            foreach (KeyValuePair<string, string> parClaveValor in diccionarioOrdenado)
            {
                Console.WriteLine(parClaveValor.Key + " = " + parClaveValor.Value);
            }

            Console.WriteLine("Números agrupados:");
            foreach (IGrouping<int, int> grupo in numerosAgrupados)
            {
                Console.WriteLine("Grupo " + grupo.Key + ":");
                foreach (int numero in grupo)
                {
                    Console.WriteLine(numero);
                }
            }

            Console.WriteLine("Números y diccionario:");
            foreach (var numeroYDiccionario in numerosYDiccionario)
            {
                Console.WriteLine("Número: " + numeroYDiccionario.Numero + ", Clave: " + numeroYDiccionario.Clave + ", Valor: " + numeroYDiccionario.Valor);
            }

            Console.WriteLine("Nombres de personas:");
            foreach (string nombre in nombresPersonas)
            {
                Console.WriteLine(nombre);
            }

            Console.WriteLine("Personas filtradas:");
            foreach (string nombre in personasFiltradas)
            {
                Console.WriteLine(nombre);
            }

            Console.WriteLine("Personas y diccionario:");
            foreach (var personaYDiccionario in personasYDiccionario)
            {
                Console.WriteLine("Nombre: " + personaYDiccionario.Nombre + ", Edad: " + personaYDiccionario.Edad + ", Clave: " + personaYDiccionario.Clave + ", Valor: " + personaYDiccionario.Valor);
            }

            // Esperar a que el usuario presione una tecla para cerrar la aplicación.
            Console.ReadKey();
        }
    }
}
```

Este código es un ejemplo de código complejo en C# que incluye una variedad de características y técnicas de programación avanzadas. El código incluye:

* Una lista de números enteros.
* Un diccionario de claves y valores de string.
* Una clase con propiedades y métodos.
* LINQ para filtrar, ordenar, agrupar y unir datos.
* Una consulta compleja de LINQ que une una lista de objetos y un diccionario.

El código muestra cómo utilizar estas características y técnicas para crear programas complejos y eficientes en C#.