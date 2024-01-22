```
// Crear una clase llamada "Número Complejo" que represente números complejos.
public class NúmeroComplejo
{
    // Definir las propiedades de la clase.
    public double ParteReal { get; set; }
    public double ParteImaginaria { get; set; }

    // Definir el constructor de la clase.
    public NúmeroComplejo(double parteReal, double parteImaginaria)
    {
        ParteReal = parteReal;
        ParteImaginaria = parteImaginaria;
    }

    // Definir los métodos de la clase.
    public NúmeroComplejo Sumar(NúmeroComplejo otroNúmeroComplejo)
    {
        return new NúmeroComplejo(ParteReal + otroNúmeroComplejo.ParteReal, ParteImaginaria + otroNúmeroComplejo.ParteImaginaria);
    }

    public NúmeroComplejo Restar(NúmeroComplejo otroNúmeroComplejo)
    {
        return new NúmeroComplejo(ParteReal - otroNúmeroComplejo.ParteReal, ParteImaginaria - otroNúmeroComplejo.ParteImaginaria);
    }

    public NúmeroComplejo Multiplicar(NúmeroComplejo otroNúmeroComplejo)
    {
        return new NúmeroComplejo(ParteReal * otroNúmeroComplejo.ParteReal - ParteImaginaria * otroNúmeroComplejo.ParteImaginaria, ParteReal * otroNúmeroComplejo.ParteImaginaria + ParteImaginaria * otroNúmeroComplejo.ParteReal);
    }

    public NúmeroComplejo Dividir(NúmeroComplejo otroNúmeroComplejo)
    {
        double denominador = otroNúmeroComplejo.ParteReal * otroNúmeroComplejo.ParteReal + otroNúmeroComplejo.ParteImaginaria * otroNúmeroComplejo.ParteImaginaria;
        return new NúmeroComplejo((ParteReal * otroNúmeroComplejo.ParteReal + ParteImaginaria * otroNúmeroComplejo.ParteImaginaria) / denominador, (ParteImaginaria * otroNúmeroComplejo.ParteReal - ParteReal * otroNúmeroComplejo.ParteImaginaria) / denominador);
    }

    public string ToString()
    {
        return ParteReal + " + " + ParteImaginaria + "i";
    }
}

// Crear una clase llamada "Programa" que contenga el código principal.
public class Programa
{
    public static void Main(string[] args)
    {
        // Crear dos números complejos.
        NúmeroComplejo númeroComplejo1 = new NúmeroComplejo(3, 4);
        NúmeroComplejo númeroComplejo2 = new NúmeroComplejo(5, -2);

        // Sumar los dos números complejos.
        NúmeroComplejo suma = númeroComplejo1.Sumar(númeroComplejo2);

        // Restar los dos números complejos.
        NúmeroComplejo resta = númeroComplejo1.Restar(númeroComplejo2);

        // Multiplicar los dos números complejos.
        NúmeroComplejo multiplicación = númeroComplejo1.Multiplicar(númeroComplejo2);

        // Dividir los dos números complejos.
        NúmeroComplejo división = númeroComplejo1.Dividir(númeroComplejo2);

        // Mostrar los resultados.
        Console.WriteLine("Suma: " + suma.ToString());
        Console.WriteLine("Resta: " + resta.ToString());
        Console.WriteLine("Multiplicación: " + multiplicación.ToString());
        Console.WriteLine("División: " + división.ToString());
    }
}
```

Este código define una clase llamada "Número Complejo" que representa números complejos. La clase tiene dos propiedades: "ParteReal" y "ParteImaginaria". El constructor de la clase inicializa estas propiedades con los valores pasados como argumentos.

La clase tiene cuatro métodos: "Sumar", "Restar", "Multiplicar" y "Dividir". Estos métodos realizan las operaciones aritméticas básicas sobre números complejos.

El método "ToString" devuelve una cadena de caracteres que representa el número complejo en la forma "ParteReal + ParteImaginariai".

La clase "Programa" contiene el código principal. Este código crea dos números complejos, los suma, los resta, los multiplica y los divide. Los resultados se muestran en la consola.