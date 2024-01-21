```c#
// Este código crea una clase llamada "Complejo" que representa un número complejo con partes real e imaginaria.

public class Complejo
{
    // Propiedades para las partes real e imaginaria del número complejo.
    public double Real { get; set; }
    public double Imaginario { get; set; }

    // Constructor que inicializa las partes real e imaginaria del número complejo.
    public Complejo(double real, double imaginario)
    {
        Real = real;
        Imaginario = imaginario;
    }

    // Operadores aritméticos para sumar, restar, multiplicar y dividir números complejos.
    public static Complejo operator +(Complejo a, Complejo b)
    {
        return new Complejo(a.Real + b.Real, a.Imaginario + b.Imaginario);
    }

    public static Complejo operator -(Complejo a, Complejo b)
    {
        return new Complejo(a.Real - b.Real, a.Imaginario - b.Imaginario);
    }

    public static Complejo operator *(Complejo a, Complejo b)
    {
        return new Complejo(a.Real * b.Real - a.Imaginario * b.Imaginario, a.Real * b.Imaginario + a.Imaginario * b.Real);
    }

    public static Complejo operator /(Complejo a, Complejo b)
    {
        double denominador = b.Real * b.Real + b.Imaginario * b.Imaginario;
        return new Complejo((a.Real * b.Real + a.Imaginario * b.Imaginario) / denominador, (a.Imaginario * b.Real - a.Real * b.Imaginario) / denominador);
    }

    // Implementacion de los métodos ToString y Equals.
    public override string ToString()
    {
        return $"{Real} + {Imaginario}i";
    }

    public override bool Equals(object obj)
    {
        if (obj is not Complejo otro)
        {
            return false;
        }

        return Real == otro.Real && Imaginario == otro.Imaginario;
    }

    // Define el operador de igualdad y desigualdad para números complejos.
    public static bool operator ==(Complejo a, Complejo b)
    {
        return a.Real == b.Real && a.Imaginario == b.Imaginario;
    }

    public static bool operator !=(Complejo a, Complejo b)
    {
        return !(a == b);
    }
}

// Código para probar la clase Complejo.
Console.WriteLine("Ejemplo de uso de la clase Complejo:");

Complejo z1 = new Complejo(3, 4);
Complejo z2 = new Complejo(5, -2);

Console.WriteLine("z1 = {0}", z1);
Console.WriteLine("z2 = {0}", z2);

Console.WriteLine("Suma: {0}", z1 + z2);
Console.WriteLine("Resta: {0}", z1 - z2);
Console.WriteLine("Multiplicación: {0}", z1 * z2);
Console.WriteLine("División: {0}", z1 / z2);

Console.WriteLine("¿Son iguales? {0}", z1 == z2);
```

Explicación del código:

* La clase `Complejo` representa un número complejo con partes real e imaginaria.
* El constructor `Complejo(double real, double imaginario)` inicializa las partes real e imaginaria del número complejo.
* Los operadores aritméticos `+`, `-`, `*` y `/` están sobrecargados para realizar operaciones aritméticas con números complejos.
* Los métodos `ToString` y `Equals` están sobrecargados para proporcionar una representación en cadena y una comparación de números complejos.
* El operador de igualdad `==` y el operador de desigualdad `!=` están sobrecargados para comparar números complejos.
* El código de prueba crea dos números complejos `z1` y `z2` y realiza operaciones aritméticas y comparaciones con ellos.