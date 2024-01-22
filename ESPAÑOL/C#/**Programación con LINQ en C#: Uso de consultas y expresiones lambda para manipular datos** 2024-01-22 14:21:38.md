```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejoCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Declaración de variables
            int[] numeros = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
            List<string> nombres = new List<string>() { "Juan", "María", "Pedro", "Ana", "Luis" };
            Dictionary<int, string> diccionario = new Dictionary<int, string>() { { 1, "Uno" }, { 2, "Dos" }, { 3, "Tres" } };

            // Uso de expresiones lambda para filtrar colecciones
            var numerosPares = numeros.Where(n => n % 2 == 0);
            var nombresConA = nombres.Where(n => n.Contains("a"));
            var elementosDiccionario = diccionario.Where(d => d.Key > 1);

            // Uso de LINQ para realizar operaciones de agregación
            var sumaNumeros = numeros.Sum();
            var promedioNombres = nombres.Average(n => n.Length);
            var maximoDiccionario = diccionario.Max(d => d.Key);

            // Uso de LINQ para realizar operaciones de ordenación
            var numerosOrdenados = numeros.OrderBy(n => n);
            var nombresOrdenados = nombres.OrderByDescending(n => n);
            var elementosDiccionarioOrdenados = diccionario.OrderBy(d => d.Value);

            // Uso de LINQ para realizar operaciones de agrupación
            var numerosAgrupados = numeros.GroupBy(n => n % 2 == 0);
            var nombresAgrupados = nombres.GroupBy(n => n.StartsWith("A"));
            var elementosDiccionarioAgrupados = diccionario.GroupBy(d => d.Value.Length);

            // Impresión de los resultados
            Console.WriteLine("Números pares:");
            foreach (var numero in numerosPares)
            {
                Console.WriteLine(numero);
            }

            Console.WriteLine("\nNombres con la letra 'a':");
            foreach (var nombre in nombresConA)
            {
                Console.WriteLine(nombre);
            }

            Console.WriteLine("\nElementos del diccionario con clave mayor a 1:");
            foreach (var elemento in elementosDiccionario)
            {
                Console.WriteLine("{0} - {1}", elemento.Key, elemento.Value);
            }

            Console.WriteLine("\nSuma de los números:");
            Console.WriteLine(sumaNumeros);

            Console.WriteLine("\nPromedio de la longitud de los nombres:");
            Console.WriteLine(promedioNombres);

            Console.WriteLine("\nClave máxima del diccionario:");
            Console.WriteLine(maximoDiccionario);

            Console.WriteLine("\nNúmeros ordenados:");
            foreach (var numero in numerosOrdenados)
            {
                Console.WriteLine(numero);
            }

            Console.WriteLine("\nNombres ordenados descendentemente:");
            foreach (var nombre in nombresOrdenados)
            {
                Console.WriteLine(nombre);
            }

            Console.WriteLine("\nElementos del diccionario ordenados por valor:");
            foreach (var elemento in elementosDiccionarioOrdenados)
            {
                Console.WriteLine("{0} - {1}", elemento.Key, elemento.Value);
            }

            Console.WriteLine("\nNúmeros agrupados por si son pares o impares:");
            foreach (var grupo in numerosAgrupados)
            {
                Console.WriteLine("Grupo {0}:", grupo.Key);
                foreach (var numero in grupo)
                {
                    Console.WriteLine(numero);
                }
            }

            Console.WriteLine("\nNombres agrupados por si empiezan con la letra 'A':");
            foreach (var grupo in nombresAgrupados)
            {
                Console.WriteLine("Grupo {0}:", grupo.Key);
                foreach (var nombre in grupo)
                {
                    Console.WriteLine(nombre);
                }
            }

            Console.WriteLine("\nElementos del diccionario agrupados por la longitud de su valor:");
            foreach (var grupo in elementosDiccionarioAgrupados)
            {
                Console.WriteLine("Grupo {0}:", grupo.Key);
                foreach (var elemento in grupo)
                {
                    Console.WriteLine("{0} - {1}", elemento.Key, elemento.Value);
                }
            }

            Console.ReadKey();
        }
    }
}
```

Explicación del código:

* En primer lugar, se declaran varias variables de distintos tipos: un array de números enteros, una lista de cadenas de texto y un diccionario que mapea claves enteras con valores de cadena de texto.


* A continuación, se utilizan expresiones lambda (en conjunto con LINQ) para filtrar las colecciones, realizar operaciones de agregación, ordenación y agrupación. 


* Las expresiones lambda permiten definir funciones anónimas, que se pueden utilizar para filtrar o transformar los elementos de una colección. Por ejemplo, la expresión `n => n % 2 == 0` se utiliza para filtrar los números pares del array `numeros`.


* Las operaciones de agregación se utilizan para reducir una colección a un único valor. Por ejemplo, la operación `Sum()` se utiliza para sumar todos los números del array `numeros`.


* Las operaciones de ordenación se utilizan para ordenar los elementos de una colección. Por ejemplo, la operación `OrderBy()` se utiliza para ordenar los números del array `numeros` en orden ascendente.


* Las operaciones de agrupación se utilizan para dividir una colección en subconjuntos. Por ejemplo, la operación `GroupBy()` se utiliza para dividir el array `numeros` en dos grupos: los números pares y los números impares.


* Por último, se imprimen los resultados de las operaciones en la consola.