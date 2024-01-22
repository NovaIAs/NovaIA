```c#
// Programa para encontrar la suma de dos matrices cuadradas.

// Definir las matrices.
int[,] matriz1 = new int[3, 3] { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };
int[,] matriz2 = new int[3, 3] { { 1, 2, 3 }, { 4, 5, 6 }, { 7, 8, 9 } };

// Crear una matriz para almacenar la suma.
int[,] matrizSuma = new int[3, 3];

// Sumar las matrices.
for (int i = 0; i < 3; i++)
{
    for (int j = 0; j < 3; j++)
    {
        matrizSuma[i, j] = matriz1[i, j] + matriz2[i, j];
    }
}

// Mostrar la suma de las matrices.
Console.WriteLine("La suma de las matrices es:");
for (int i = 0; i < 3; i++)
{
    for (int j = 0; j < 3; j++)
    {
        Console.Write(matrizSuma[i, j] + " ");
    }
    Console.WriteLine();
}

// Función para calcular el factorial de un número.
int factorial(int n)
{
    if (n == 0)
    {
        return 1;
    }
    else
    {
        return n * factorial(n - 1);
    }
}

// Mostrar el factorial de 5.
Console.WriteLine("El factorial de 5 es: " + factorial(5));

// Función para calcular el máximo común divisor de dos números.
int mcd(int a, int b)
{
    if (b == 0)
    {
        return a;
    }
    else
    {
        return mcd(b, a % b);
    }
}

// Mostrar el máximo común divisor de 12 y 18.
Console.WriteLine("El máximo común divisor de 12 y 18 es: " + mcd(12, 18));

```

Este código es una combinación de tres programas diferentes:

* El primer programa suma dos matrices cuadradas y muestra el resultado.
* El segundo programa calcula el factorial de un número y muestra el resultado.
* El tercer programa calcula el máximo común divisor de dos números y muestra el resultado.

El código está escrito en C# y utiliza la biblioteca estándar de C#.

Cada programa está escrito en una función separada, lo que hace que el código sea más fácil de leer y mantener.

El código está bien documentado con comentarios, lo que hace que sea fácil entender lo que hace el código.