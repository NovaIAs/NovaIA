```c#
// Programa que calcula el factorial de un número usando recursividad

// Función recursiva para calcular el factorial de un número
int Factorial(int numero)
{
    // Si el número es 0, entonces el factorial es 1
    if (numero == 0)
    {
        return 1;
    }
    // Si el número es mayor que 0, entonces el factorial es el número multiplicado por el factorial del número anterior
    else
    {
        return numero * Factorial(numero - 1);
    }
}

// Función principal
public static void Main(string[] args)
{
    // Se pide al usuario que ingrese un número
    Console.WriteLine("Ingrese un número:");
    int numero = int.Parse(Console.ReadLine());

    // Se calcula el factorial del número ingresado
    int factorial = Factorial(numero);

    // Se muestra el factorial del número ingresado en la consola
    Console.WriteLine("El factorial de {0} es {1}", numero, factorial);
}
```

Explicación del código:

* La función `Factorial` es una función recursiva que calcula el factorial de un número.
* La función `Factorial` recibe un número como argumento y devuelve el factorial del número.
* La función `Factorial` tiene dos casos base:
    * Si el número es 0, entonces el factorial es 1.
    * Si el número es mayor que 0, entonces el factorial es el número multiplicado por el factorial del número anterior.
* La función `Main` es la función principal del programa.
* La función `Main` pide al usuario que ingrese un número.
* La función `Main` llama a la función `Factorial` para calcular el factorial del número ingresado.
* La función `Main` muestra el factorial del número ingresado en la consola.