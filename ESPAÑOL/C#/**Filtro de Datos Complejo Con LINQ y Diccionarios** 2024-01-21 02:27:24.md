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
            // Creamos una lista de números enteros.
            List<int> numeros = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

            // Utilizamos la expresión LINQ para filtrar los números que son mayores que 5.
            var numerosMayoresQue5 = numeros.Where(n => n > 5);

            // Imprimimos los números filtrados en la consola.
            foreach (var numero in numerosMayoresQue5)
            {
                Console.WriteLine(numero);
            }

            // Creamos una lista de cadenas de texto.
            List<string> cadenas = new List<string>() { "Hola", "Mundo", "Cómo", "Estás" };

            // Utilizamos la expresión LINQ para filtrar las cadenas que contienen la letra "o".
            var cadenasConLetraO = cadenas.Where(c => c.Contains("o"));

            // Imprimimos las cadenas filtradas en la consola.
            foreach (var cadena in cadenasConLetraO)
            {
                Console.WriteLine(cadena);
            }

            // Creamos un diccionario de claves y valores.
            Dictionary<string, int> diccionario = new Dictionary<string, int>() { { "Uno", 1 }, { "Dos", 2 }, { "Tres", 3 } };

            // Utilizamos la expresión LINQ para filtrar los pares clave-valor cuyo valor es mayor que 2.
            var paresClaveValorConValorMayorQue2 = diccionario.Where(par => par.Value > 2);

            // Imprimimos los pares clave-valor filtrados en la consola.
            foreach (var parClaveValor in paresClaveValorConValorMayorQue2)
            {
                Console.WriteLine("{0} = {1}", parClaveValor.Key, parClaveValor.Value);
            }
        }
    }
}
```

Este código es complejo porque utiliza expresiones LINQ para filtrar datos de diferentes tipos de colecciones. También utiliza un diccionario para almacenar pares clave-valor. El código está bien documentado con comentarios que explican lo que hace cada parte del código.

El código es útil porque se puede utilizar para filtrar datos de diferentes tipos de colecciones de manera eficiente. También se puede utilizar para crear diccionarios de claves y valores.