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
            // Definimos una clase abstracta para representar una figura geométrica
            abstract class FiguraGeometrica
            {
                public abstract double CalcularArea();
            }

            // Definimos una clase que representa un círculo
            class Circulo : FiguraGeometrica
            {
                public double Radio { get; set; }

                public override double CalcularArea()
                {
                    return Math.PI * Radio * Radio;
                }
            }

            // Definimos una clase que representa un cuadrado
            class Cuadrado : FiguraGeometrica
            {
                public double Lado { get; set; }

                public override double CalcularArea()
                {
                    return Lado * Lado;
                }
            }

            // Definimos una clase que representa un triángulo
            class Triangulo : FiguraGeometrica
            {
                public double Base { get; set; }
                public double Altura { get; set; }

                public override double CalcularArea()
                {
                    return 0.5 * Base * Altura;
                }
            }

            // Creamos una lista de figuras geométricas
            List<FiguraGeometrica> figuras = new List<FiguraGeometrica>();

            // Añadimos un círculo a la lista
            figuras.Add(new Circulo() { Radio = 5 });

            // Añadimos un cuadrado a la lista
            figuras.Add(new Cuadrado() { Lado = 10 });

            // Añadimos un triángulo a la lista
            figuras.Add(new Triangulo() { Base = 15, Altura = 20 });

            // Calculamos el área de cada figura y la mostramos por consola
            foreach (var figura in figuras)
            {
                Console.WriteLine($"Área de la {figura.GetType().Name}: {figura.CalcularArea():F2}");
            }

            // Utilizamos LINQ para obtener el área total de todas las figuras
            double areaTotal = figuras.Sum(figura => figura.CalcularArea());

            // Mostramos el área total por consola
            Console.WriteLine($"Área total: {areaTotal:F2}");
        }
    }
}
```

Explicación del código:

* Definimos una clase abstracta `FiguraGeometrica` que representa una figura geométrica. Esta clase tiene un método abstracto `CalcularArea()` que debe ser implementado por las clases derivadas.
* Definimos tres clases concretas que heredan de `FiguraGeometrica`: `Circulo`, `Cuadrado` y `Triangulo`. Cada una de estas clases implementa el método `CalcularArea()` para calcular el área de la figura correspondiente.
* Creamos una lista de figuras geométricas y añadimos un círculo, un cuadrado y un triángulo a la lista.
* Calculamos el área de cada figura y la mostramos por consola.
* Utilizamos LINQ para obtener el área total de todas las figuras y la mostramos por consola.