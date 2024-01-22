```c#
using System;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace ComplejoCodigoCSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Definir una lista de números enteros
            List<int> numeros = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

            // Utilizar LINQ para filtrar los números pares de la lista
            var numerosPares = from numero in numeros
                              where numero % 2 == 0
                              select numero;

            // Mostrar los números pares en la consola
            Console.WriteLine("Números pares:");
            foreach (var numero in numerosPares)
            {
                Console.WriteLine(numero);
            }

            // Utilizar expresiones regulares para buscar una cadena en un texto
            string texto = "Hola mundo, este es un ejemplo de texto.";
            string patron = "mundo";
            Regex regex = new Regex(patron);
            Match match = regex.Match(texto);

            // Mostrar la posición de la cadena encontrada en el texto
            if (match.Success)
            {
                Console.WriteLine("La cadena \"{0}\" se encontró en la posición {1}.", patron, match.Index);
            }
            else
            {
                Console.WriteLine("La cadena \"{0}\" no se encontró en el texto.", patron);
            }

            // Utilizar reflexión para obtener información de un tipo
            Type tipo = typeof(Program);
            Console.WriteLine("Información del tipo {0}:", tipo.Name);
            Console.WriteLine("Métodos:");
            foreach (var metodo in tipo.GetMethods())
            {
                Console.WriteLine("- {0}", metodo.Name);
            }
            Console.WriteLine("Propiedades:");
            foreach (var propiedad in tipo.GetProperties())
            {
                Console.WriteLine("- {0}", propiedad.Name);
            }

            // Crear un delegado para manejar un evento
            Action<string> delegado = (mensaje) =>
            {
                Console.WriteLine("Evento: {0}", mensaje);
            };

            // Suscribirse al evento utilizando el delegado
            Evento.Suscribirse(delegado);

            // Activar el evento
            Evento.Activar();
        }
    }

    public class Evento
    {
        public static event Action<string> EventoOcurrido;

        public static void Activar()
        {
            EventoOcurrido?.Invoke("¡Evento activado!");
        }

        public static void Suscribirse(Action<string> delegado)
        {
            EventoOcurrido += delegado;
        }

        public static void Desuscribirse(Action<string> delegado)
        {
            EventoOcurrido -= delegado;
        }
    }
}
```

Este código es complejo y diferenciado porque utiliza varias técnicas y conceptos de programación en C#, como LINQ, expresiones regulares, reflexión, delegados y eventos.

El código comienza definiendo una lista de números enteros y luego utiliza LINQ para filtrar los números pares de la lista.

A continuación, el código utiliza expresiones regulares para buscar una cadena en un texto.

Luego, el código utiliza reflexión para obtener información de un tipo.

Finalmente, el código crea un delegado para manejar un evento y suscribe el delegado al evento.

El código también define una clase llamada Evento que contiene un evento llamado EventoOcurrido. El evento se activa cuando se llama al método Activar() de la clase Evento.

El código también define un método llamado Suscribirse() que suscribe un delegado al evento EventoOcurrido.

El código también define un método llamado Desuscribirse() que desuscribe un delegado del evento EventoOcurrido.