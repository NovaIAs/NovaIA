```c#
// Creamos una clase llamada "Complejo" que representa un número complejo.
public class Complejo
{
    private double real; // La parte real del número complejo.
    private double imaginaria; // La parte imaginaria del número complejo.

    // Constructor que crea un número complejo a partir de su parte real y su parte imaginaria.
    public Complejo(double real, double imaginaria)
    {
        this.real = real;
        this.imaginaria = imaginaria;
    }

    // Sobrecarga del operador + para sumar dos números complejos.
    public static Complejo operator +(Complejo c1, Complejo c2)
    {
        return new Complejo(c1.real + c2.real, c1.imaginaria + c2.imaginaria);
    }

    // Sobrecarga del operador - para restar dos números complejos.
    public static Complejo operator -(Complejo c1, Complejo c2)
    {
        return new Complejo(c1.real - c2.real, c1.imaginaria - c2.imaginaria);
    }

    // Sobrecarga del operador * para multiplicar dos números complejos.
    public static Complejo operator *(Complejo c1, Complejo c2)
    {
        return new Complejo(c1.real * c2.real - c1.imaginaria * c2.imaginaria,
                            c1.real * c2.imaginaria + c1.imaginaria * c2.real);
    }

    // Sobrecarga del operador / para dividir dos números complejos.
    public static Complejo operator /(Complejo c1, Complejo c2)
    {
        // Calculamos el denominador.
        double denominador = c2.real * c2.real + c2.imaginaria * c2.imaginaria;

        // Calculamos el numerador.
        double numeradorReal = c1.real * c2.real + c1.imaginaria * c2.imaginaria;
        double numeradorImaginario = c1.imaginaria * c2.real - c1.real * c2.imaginaria;

        // Devolvemos el resultado.
        return new Complejo(numeradorReal / denominador, numeradorImaginario / denominador);
    }

    // Sobrecarga del método ToString para devolver una representación en cadena del número complejo.
    public override string ToString()
    {
        return $"{real} + {imaginaria}i";
    }
}

// Creamos dos números complejos.
Complejo c1 = new Complejo(2, 3);
Complejo c2 = new Complejo(4, 5);

// Sumamos los dos números complejos.
Complejo c3 = c1 + c2;

// Restamos los dos números complejos.
Complejo c4 = c1 - c2;

// Multiplicamos los dos números complejos.
Complejo c5 = c1 * c2;

// Dividimos los dos números complejos.
Complejo c6 = c1 / c2;

// Mostramos los resultados.
Console.WriteLine($"c1: {c1}");
Console.WriteLine($"c2: {c2}");
Console.WriteLine($"c3: {c3}");
Console.WriteLine($"c4: {c4}");
Console.WriteLine($"c5: {c5}");
Console.WriteLine($"c6: {c6}");
```

Explicación del código:

* Creamos una clase llamada "Complejo" que representa un número complejo.
* La clase tiene dos propiedades privadas: "real" e "imaginaria".
* El constructor de la clase crea un número complejo a partir de su parte real y su parte imaginaria.
* Sobrecargamos los operadores +, -, *, / para sumar, restar, multiplicar y dividir números complejos, respectivamente.
* Sobrecargamos el método ToString para devolver una representación en cadena del número complejo.
* Creamos dos números complejos, c1 y c2.
* Sumamos, restamos, multiplicamos y dividimos los dos números complejos, y almacenamos los resultados en c3, c4, c5 y c6, respectivamente.
* Mostramos los resultados en la consola.