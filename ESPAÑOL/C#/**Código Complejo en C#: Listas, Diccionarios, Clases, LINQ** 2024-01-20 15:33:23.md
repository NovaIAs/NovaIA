```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Código_Complejo_en_Csharp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Crear una lista de números enteros
            List<int> números = new List<int>();
            números.Add(1);
            números.Add(2);
            números.Add(3);
            números.Add(4);
            números.Add(5);

            // Crear una lista de cadenas de texto
            List<string> cadenas = new List<string>();
            cadenas.Add("Hola");
            cadenas.Add("Mundo");
            cadenas.Add("!");

            // Crear un diccionario de clave-valor
            Dictionary<string, int> diccionario = new Dictionary<string, int>();
            diccionario.Add("Uno", 1);
            diccionario.Add("Dos", 2);
            diccionario.Add("Tres", 3);

            // Crear una clase personalizada
            public class Persona
            {
                public string Nombre { get; set; }
                public int Edad { get; set; }

                public Persona(string nombre, int edad)
                {
                    Nombre = nombre;
                    Edad = edad;
                }
            }

            // Crear una instancia de la clase personalizada
            Persona persona = new Persona("Juan", 25);

            // Utilizar LINQ para consultar los datos
            var resultado = from número in números
                           where número > 2
                           select número;

            // Mostrar los resultados en la consola
            foreach (var item in resultado)
            {
                Console.WriteLine(item);
            }
        }
    }
}
```

Explicación del código:

1. **Creación de listas y diccionario:**
    - Se crean dos listas, una de números enteros y otra de cadenas de texto.
    - También se crea un diccionario de clave-valor, donde las claves son cadenas de texto y los valores son números enteros.


2. **Creación de clase personalizada:**
    - Se crea una clase personalizada llamada `Persona`, que tiene dos propiedades: `Nombre` y `Edad`.
    - También se define un constructor para la clase, que toma un nombre y una edad como argumentos.


3. **Creación de instancia de clase personalizada:**
    - Se crea una instancia de la clase `Persona` llamada `persona`, con el nombre "Juan" y la edad 25.


4. **Utilización de LINQ para consultar datos:**
    - Se utiliza LINQ (Language Integrated Query) para consultar los datos de la lista de números enteros.
    - La consulta selecciona todos los números mayores de 2.


5. **Mostrar resultados en la consola:**
    - Se utiliza un bucle `foreach` para recorrer los resultados de la consulta y mostrarlos en la consola.