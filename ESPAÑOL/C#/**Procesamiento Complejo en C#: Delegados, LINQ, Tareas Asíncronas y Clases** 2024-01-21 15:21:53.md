```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejoEnCSharp
{
    // Definición de la clase 'ProgramaPrincipal'.
    class ProgramaPrincipal
    {
        // Método 'Main'.
        static void Main(string[] args)
        {
            Console.WriteLine("¡Hola, mundo!"); // Muestra un mensaje en la consola.

            // Definición del delegado 'OperacionBinaria'.
            delegate double OperacionBinaria(double a, double b);

            // Creación de una lista de operaciones binarias.
            List<OperacionBinaria> operaciones = new List<OperacionBinaria>();
            operaciones.Add((a, b) => a + b); // Suma.
            operaciones.Add((a, b) => a - b); // Resta.
            operaciones.Add((a, b) => a * b); // Multiplicación.
            operaciones.Add((a, b) => a / b); // División.

            // Cálculo del resultado de cada operación binaria.
            foreach (var operacion in operaciones)
            {
                Console.WriteLine($"El resultado de la operación es: {operacion(10, 5)}");
            }

            // Definición de la clase 'Persona'.
            class Persona
            {
                public string Nombre { get; set; }
                public int Edad { get; set; }

                public Persona(string nombre, int edad)
                {
                    Nombre = nombre;
                    Edad = edad;
                }

                public override string ToString()
                {
                    return $"Nombre: {Nombre}, Edad: {Edad}";
                }
            }

            // Creación de una lista de personas.
            List<Persona> personas = new List<Persona>();
            personas.Add(new Persona("Juan", 25));
            personas.Add(new Persona("María", 30));
            personas.Add(new Persona("Pedro", 35));

            // Impresión de la información de cada persona.
            foreach (var persona in personas)
            {
                Console.WriteLine($"Persona: {persona}");
            }

            // Utilización de LINQ para filtrar la lista de personas.
            var personasMayoresDe30 = personas.Where(persona => persona.Edad > 30).ToList();

            // Impresión de la información de las personas mayores de 30 años.
            foreach (var persona in personasMayoresDe30)
            {
                Console.WriteLine($"Persona mayor de 30 años: {persona}");
            }

            // Creación de un objeto 'Task' para ejecutar una tarea asincrónica.
            Task<int> tareaAsincrona = Task.Run(() =>
            {
                // Simulación de una tarea asincrónica que tarda 5 segundos en ejecutarse.
                Thread.Sleep(5000);

                // Devolución del resultado de la tarea.
                return 10;
            });

            // Espera a que la tarea asincrónica se complete.
            int resultadoTareaAsincrona = tareaAsincrona.Result;

            // Impresión del resultado de la tarea asincrónica.
            Console.WriteLine($"El resultado de la tarea asincrónica es: {resultadoTareaAsincrona}");
        }
    }
}
```

**Explicación del código:**

* El código comienza con la definición de la clase 'ProgramaPrincipal', que contiene el método 'Main'.
* Se define un delegado 'OperacionBinaria' que representa una operación binaria que toma dos números dobles como entrada y devuelve un número doble como resultado.
* Se crea una lista de operaciones binarias, que incluye la suma, la resta, la multiplicación y la división.
* Se calculan los resultados de cada operación binaria para los números 10 y 5, y se muestran en la consola.
* Se definen una clase 'Persona' y una lista de personas.
* Se utilizan expresiones lambda para filtrar la lista de personas y seleccionar las personas mayores de 30 años.
* Se utiliza la clase 'Task' para crear una tarea asincrónica que simula una tarea que tarda 5 segundos en ejecutarse.
* Se espera a que la tarea asincrónica se complete y se muestra el resultado de la tarea en la consola.