```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodigoComplejo
{
    public class Program
    {
        public static void Main(string[] args)
        {
            // Definición de una clase compleja
            public class Complejo
            {
                // Atributos reales e imaginarios
                private double real;
                private double imaginario;

                // Constructores
                public Complejo()
                {
                    real = 0;
                    imaginario = 0;
                }

                public Complejo(double real, double imaginario)
                {
                    this.real = real;
                    this.imaginario = imaginario;
                }

                // Propiedades
                public double Real
                {
                    get { return real; }
                    set { real = value; }
                }

                public double Imaginario
                {
                    get { return imaginario; }
                    set { imaginario = value; }
                }

                // Métodos
                public Complejo Sumar(Complejo otroComplejo)
                {
                    return new Complejo(real + otroComplejo.real, imaginario + otroComplejo.imaginario);
                }

                public Complejo Restar(Complejo otroComplejo)
                {
                    return new Complejo(real - otroComplejo.real, imaginario - otroComplejo.imaginario);
                }

                public Complejo Multiplicar(Complejo otroComplejo)
                {
                    double real1 = real * otroComplejo.real;
                    double real2 = real * otroComplejo.imaginario;
                    double imaginario1 = imaginario * otroComplejo.real;
                    double imaginario2 = imaginario * otroComplejo.imaginario;

                    return new Complejo(real1 - imaginario2, real2 + imaginario1);
                }

                public Complejo Dividir(Complejo otroComplejo)
                {
                    double denominador = Math.Pow(otroComplejo.real, 2) + Math.Pow(otroComplejo.imaginario, 2);

                    double real1 = (real * otroComplejo.real + imaginario * otroComplejo.imaginario) / denominador;
                    double imaginario1 = (imaginario * otroComplejo.real - real * otroComplejo.imaginario) / denominador;

                    return new Complejo(real1, imaginario1);
                }

                public string ToString()
                {
                    return string.Format("({0}, {1})", real, imaginario);
                }
            }

            // Uso de la clase compleja
            Complejo complejo1 = new Complejo(3, 4);
            Complejo complejo2 = new Complejo(5, 2);

            Complejo suma = complejo1.Sumar(complejo2);
            Complejo resta = complejo1.Restar(complejo2);
            Complejo multiplicacion = complejo1.Multiplicar(complejo2);
            Complejo division = complejo1.Dividir(complejo2);

            Console.WriteLine("Complejo 1: {0}", complejo1.ToString());
            Console.WriteLine("Complejo 2: {0}", complejo2.ToString());

            Console.WriteLine("Suma: {0}", suma.ToString());
            Console.WriteLine("Resta: {0}", resta.ToString());
            Console.WriteLine("Multiplicación: {0}", multiplicacion.ToString());
            Console.WriteLine("División: {0}", division.ToString());
        }
    }
}
```

Explicación:

* Se define una clase `Complejo` que representa un número complejo.
* La clase tiene dos atributos privados, `real` e `imaginario`, que definen la parte real y la parte imaginaria del número complejo.
* La clase tiene varios constructores, que permiten crear números complejos con diferentes valores iniciales.
* La clase también tiene varias propiedades, que permiten obtener y establecer los valores de los atributos privados.
* La clase tiene varios métodos, que permiten realizar operaciones aritméticas con números complejos.
* En el método `Main`, se crean dos instancias de la clase `Complejo` y se realizan varias operaciones aritméticas con ellas.
* Los resultados de las operaciones se imprimen en la consola.