```csharp
// Proyecto: "Calculadora Científica en C#"

// Importar las bibliotecas necesarias
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

// Definir la clase "CalculadoraCientifica"
public class CalculadoraCientifica
{
    // Definir las propiedades de la calculadora
    public double Resultado { get; set; }
    public bool EsError { get; set; }
    public string MensajeDeError { get; set; }

    // Definir los métodos de la calculadora
    public void Sumar(double numero1, double numero2)
    {
        try
        {
            Resultado = numero1 + numero2;
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void Restar(double numero1, double numero2)
    {
        try
        {
            Resultado = numero1 - numero2;
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void Multiplicar(double numero1, double numero2)
    {
        try
        {
            Resultado = numero1 * numero2;
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void Dividir(double numero1, double numero2)
    {
        try
        {
            if (numero2 == 0)
            {
                throw new DivideByZeroException("No se puede dividir por cero.");
            }
            Resultado = numero1 / numero2;
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void Potencia(double numero, double exponente)
    {
        try
        {
            Resultado = Math.Pow(numero, exponente);
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void RaizCuadrada(double numero)
    {
        try
        {
            if (numero < 0)
            {
                throw new ArgumentOutOfRangeException("No se puede calcular la raíz cuadrada de un número negativo.");
            }
            Resultado = Math.Sqrt(numero);
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void Seno(double angulo)
    {
        try
        {
            Resultado = Math.Sin(angulo);
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void Coseno(double angulo)
    {
        try
        {
            Resultado = Math.Cos(angulo);
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void Tangente(double angulo)
    {
        try
        {
            if (Math.Cos(angulo) == 0)
            {
                throw new DivideByZeroException("No se puede calcular la tangente de un ángulo cuyo coseno es cero.");
            }
            Resultado = Math.Tan(angulo);
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void InversoSeno(double numero)
    {
        try
        {
            if (numero < -1 || numero > 1)
            {
                throw new ArgumentOutOfRangeException("El número debe estar entre -1 y 1.");
            }
            Resultado = Math.Asin(numero);
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void InversoCoseno(double numero)
    {
        try
        {
            if (numero < -1 || numero > 1)
            {
                throw new ArgumentOutOfRangeException("El número debe estar entre -1 y 1.");
            }
            Resultado = Math.Acos(numero);
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }

    public void InversoTangente(double numero)
    {
        try
        {
            Resultado = Math.Atan(numero);
            EsError = false;
            MensajeDeError = null;
        }
        catch (Exception ex)
        {
            EsError = true;
            MensajeDeError = ex.Message;
        }
    }
}

// Definir la clase "Program"
public class Program
{
    // Definir el método "Main"
    public static void Main(string[] args)
    {
        // Crear una calculadora científica
        CalculadoraCientifica calculadora = new CalculadoraCientifica();

        // Obtener los números del usuario
        Console.WriteLine("Ingrese el primer número:");
        double numero1 = double.Parse(Console.ReadLine());

        Console.WriteLine("Ingrese el segundo número:");
        double numero2 = double.Parse(Console.ReadLine());

        // Realizar la operación
        calculadora.Sumar(numero1, numero2);

        // Mostrar el resultado
        Console.WriteLine("Resultado:");
        if (calculadora.EsError)
        {
            Console.WriteLine(calculadora.MensajeDeError);
        }
        else
        {
            Console.WriteLine(calculadora.Resultado);
        }

        // Esperar a que el usuario presione una tecla
        Console.ReadKey();
    }
}
```

Explicación del código:

* La clase "CalculadoraCientifica" define las propiedades y métodos de la calculadora científica.
* Las propiedades son "Resultado", "EsError" y "MensajeDeError".
* Los métodos son los métodos matemáticos que se pueden realizar con la calculadora, como sumar, restar, multiplicar, dividir, potencia, raíz cuadrada, seno, coseno, tangente, inverso seno, inverso coseno e inverso tangente.
* La clase "Program" define el método "Main" que es el punto de entrada del programa.
* En el método "Main" se crea una calculadora científica, se obtienen los números del usuario, se realiza la operación y se muestra el resultado.
* El código está bien documentado con comentarios para que sea fácil de entender.