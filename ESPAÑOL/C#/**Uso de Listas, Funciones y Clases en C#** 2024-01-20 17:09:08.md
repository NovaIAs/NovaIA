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
            // Creamos una lista de enteros.
            List<int> numeros = new List<int>();

            // Agregamos algunos elementos a la lista.
            numeros.Add(1);
            numeros.Add(2);
            numeros.Add(3);
            numeros.Add(4);
            numeros.Add(5);

            // Creamos una lista de strings.
            List<string> palabras = new List<string>();

            // Agregamos algunos elementos a la lista.
            palabras.Add("Hola");
            palabras.Add("Mundo");
            palabras.Add("!");

            // Creamos una clase Persona.
            class Persona
            {
                public string Nombre { get; set; }
                public int Edad { get; set; }

                public Persona(string nombre, int edad)
                {
                    Nombre = nombre;
                    Edad = edad;
                }
            }

            // Creamos una lista de personas.
            List<Persona> personas = new List<Persona>();

            // Agregamos algunas personas a la lista.
            personas.Add(new Persona("Juan", 20));
            personas.Add(new Persona("María", 25));
            personas.Add(new Persona("Pedro", 30));

            // Creamos una función para imprimir una lista de enteros.
            void ImprimirListaEnteros(List<int> lista)
            {
                foreach (int numero in lista)
                {
                    Console.WriteLine(numero);
                }
            }

            // Creamos una función para imprimir una lista de strings.
            void ImprimirListaStrings(List<string> lista)
            {
                foreach (string palabra in lista)
                {
                    Console.WriteLine(palabra);
                }
            }

            // Creamos una función para imprimir una lista de personas.
            void ImprimirListaPersonas(List<Persona> lista)
            {
                foreach (Persona persona in lista)
                {
                    Console.WriteLine(persona.Nombre + " - " + persona.Edad);
                }
            }

            // Imprimimos las listas.
            ImprimirListaEnteros(numeros);
            ImprimirListaStrings(palabras);
            ImprimirListaPersonas(personas);
        }
    }
}
```

Explicación:

* Creamos una lista de enteros, una lista de strings y una lista de personas.
* Añadimos algunos elementos a cada una de las listas.
* Creamos una función para imprimir una lista de enteros, una función para imprimir una lista de strings y una función para imprimir una lista de personas.
* Imprimimos las listas.

Este código es complejo porque utiliza listas, funciones y clases. También utiliza la sintaxis de C# para crear objetos y llamar a métodos.