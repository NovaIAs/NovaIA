```c#
// Importamos las bibliotecas necesarias
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Definimos una clase
public class NumeroComplejo
{
    // Atributos
    private double real;
    private double imaginaria;

    // Constructores
    public NumeroComplejo()
    {
        real = 0;
        imaginaria = 0;
    }

    public NumeroComplejo(double real, double imaginaria)
    {
        this.real = real;
        this.imaginaria = imaginaria;
    }

    // Propiedades
    public double Real
    {
        get { return real; }
        set { real = value; }
    }

    public double Imaginaria
    {
        get { return imaginaria; }
        set { imaginaria = value; }
    }

    // Métodos
    public NumeroComplejo Suma(NumeroComplejo otro)
    {
        return new NumeroComplejo(real + otro.real, imaginaria + otro.imaginaria);
    }

    public NumeroComplejo Resta(NumeroComplejo otro)
    {
        return new NumeroComplejo(real - otro.real, imaginaria - otro.imaginaria);
    }

    public NumeroComplejo Multiplicacion(NumeroComplejo otro)
    {
        return new NumeroComplejo(real * otro.real - imaginaria * otro.imaginaria, real * otro.imaginaria + imaginaria * otro.real);
    }

    public NumeroComplejo Division(NumeroComplejo otro)
    {
        double denominador = Math.Pow(otro.real, 2) + Math.Pow(otro.imaginaria, 2);
        return new NumeroComplejo((real * otro.real + imaginaria * otro.imaginaria) / denominador, (imaginaria * otro.real - real * otro.imaginaria) / denominador);
    }

    public override string ToString()
    {
        return $"{real} + {imaginaria}i";
    }
}

// Programa principal
class Program
{
    static void Main(string[] args)
    {
        // Creamos dos números complejos
        NumeroComplejo c1 = new NumeroComplejo(3, 4);
        NumeroComplejo c2 = new NumeroComplejo(5, -2);

        // Sumamos los números complejos
        NumeroComplejo suma = c1.Suma(c2);

        // Restamos los números complejos
        NumeroComplejo resta = c1.Resta(c2);

        // Multiplicamos los números complejos
        NumeroComplejo multiplicacion = c1.Multiplicacion(c2);

        // Dividimos los números complejos
        NumeroComplejo division = c1.Division(c2);

        // Mostramos los resultados
        Console.WriteLine($"Suma: {suma}");
        Console.WriteLine($"Resta: {resta}");
        Console.WriteLine($"Multiplicación: {multiplicacion}");
        Console.WriteLine($"División: {division}");
    }
}
```

**Explicación:**

* La clase `NumeroComplejo` representa un número complejo con atributos `real` e `imaginaria`.
* Los constructores inicializan los atributos de la clase.
* Las propiedades `Real` e `Imaginaria` permiten acceder a los atributos de la clase.
* Los métodos `Suma`, `Resta`, `Multiplicacion` y `Division` realizan las operaciones básicas entre números complejos.
* El método `ToString` devuelve una representación del número complejo en formato de texto.
* En el programa principal, creamos dos objetos de la clase `NumeroComplejo` y realizamos las operaciones básicas entre ellos.
* Mostramos los resultados por consola.

Este código es un ejemplo de cómo se pueden crear clases y objetos en C#. También muestra cómo se pueden realizar operaciones básicas con números complejos.