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
            // Definición de una clase genérica para manejar listas de objetos de cualquier tipo.
            class ListaGenerica<T>
            {
                private T[] _elementos;
                private int _longitudActual;

                public ListaGenerica(int capacidadInicial)
                {
                    _elementos = new T[capacidadInicial];
                    _longitudActual = 0;
                }

                public void Agregar(T elemento)
                {
                    if (_longitudActual == _elementos.Length)
                    {
                        // Aumentar la capacidad de la lista si es necesario.
                        Array.Resize(ref _elementos, _elementos.Length * 2);
                    }
                    _elementos[_longitudActual++] = elemento;
                }

                public T Obtener(int indice)
                {
                    if (indice < 0 || indice >= _longitudActual)
                    {
                        throw new IndexOutOfRangeException("Índice fuera de rango.");
                    }
                    return _elementos[indice];
                }

                public void Eliminar(int indice)
                {
                    if (indice < 0 || indice >= _longitudActual)
                    {
                        throw new IndexOutOfRangeException("Índice fuera de rango.");
                    }
                    for (int i = indice; i < _longitudActual - 1; i++)
                    {
                        _elementos[i] = _elementos[i + 1];
                    }
                    _longitudActual--;
                }

                public int Longitud
                {
                    get
                    {
                        return _longitudActual;
                    }
                }
            }

            // Uso de la clase ListaGenerica para crear una lista de números enteros.
            ListaGenerica<int> listaEnteros = new ListaGenerica<int>(10);
            listaEnteros.Agregar(1);
            listaEnteros.Agregar(2);
            listaEnteros.Agregar(3);
            listaEnteros.Agregar(4);
            listaEnteros.Agregar(5);

            // Impresión de los elementos de la lista de números enteros.
            Console.WriteLine("Lista de números enteros:");
            for (int i = 0; i < listaEnteros.Longitud; i++)
            {
                Console.WriteLine(listaEnteros.Obtener(i));
            }

            // Uso de la clase ListaGenerica para crear una lista de cadenas de texto.
            ListaGenerica<string> listaCadenas = new ListaGenerica<string>(10);
            listaCadenas.Agregar("Hola");
            listaCadenas.Agregar("Mundo");
            listaCadenas.Agregar("!");

            // Impresión de los elementos de la lista de cadenas de texto.
            Console.WriteLine("\nLista de cadenas de texto:");
            for (int i = 0; i < listaCadenas.Longitud; i++)
            {
                Console.WriteLine(listaCadenas.Obtener(i));
            }

            // Uso de la clase ListaGenerica para crear una lista de objetos Persona.
            class Persona
            {
                public string Nombre { get; set; }
                public int Edad { get; set; }
            }
            ListaGenerica<Persona> listaPersonas = new ListaGenerica<Persona>(10);
            listaPersonas.Agregar(new Persona { Nombre = "Juan", Edad = 25 });
            listaPersonas.Agregar(new Persona { Nombre = "María", Edad = 30 });
            listaPersonas.Agregar(new Persona { Nombre = "Pedro", Edad = 35 });

            // Impresión de los elementos de la lista de objetos Persona.
            Console.WriteLine("\nLista de objetos Persona:");
            for (int i = 0; i < listaPersonas.Longitud; i++)
            {
                Console.WriteLine("{0} ({1})", listaPersonas.Obtener(i).Nombre, listaPersonas.Obtener(i).Edad);
            }

            Console.ReadKey();
        }
    }
}
```

Explicación del código:

* Se define una clase genérica `ListaGenerica<T>` que puede almacenar objetos de cualquier tipo `T`.
* Se crean tres listas usando la clase `ListaGenerica`: una lista de números enteros, una lista de cadenas de texto y una lista de objetos `Persona`.
* Se agregan elementos a cada lista.
* Se imprimen los elementos de cada lista.

Este código es complejo porque utiliza programación genérica, que es una característica avanzada de C#. La programación genérica permite definir clases y métodos que pueden trabajar con diferentes tipos de datos sin tener que escribir código específico para cada tipo de datos. Esto hace que el código sea más reutilizable y flexible.