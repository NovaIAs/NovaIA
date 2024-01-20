```c#
// Definición de método para calcular el factorial de un número
int Factorial(int numero)
{
    if (numero == 0)
    {
        return 1;
    }
    else
    {
        return numero * Factorial(numero - 1);
    }
}

// Definición de método para calcular el área de un triángulo
double AreaTriangulo(double base, double altura)
{
    return (base * altura) / 2;
}

// Definición de método para determinar si un número es primo
bool EsPrimo(int numero)
{
    if (numero <= 1)
    {
        return false;
    }
    else if (numero == 2)
    {
        return true;
    }
    else
    {
        for (int i = 2; i <= Math.Sqrt(numero); i++)
        {
            if (numero % i == 0)
            {
                return false;
            }
        }
        return true;
    }
}

// Definición de método para invertir una cadena
string InvertirCadena(string cadena)
{
    char[] caracteres = cadena.ToCharArray();
    Array.Reverse(caracteres);
    return new string(caracteres);
}

// Definición de método para encontrar el máximo común divisor de dos números
int MaximoComunDivisor(int numero1, int numero2)
{
    int minimo = Math.Min(numero1, numero2);
    for (int i = 1; i <= minimo; i++)
    {
        if (numero1 % i == 0 && numero2 % i == 0)
        {
            return i;
        }
    }
    return 1;
}

// Definición de método para ordenar un array de números
int[] OrdenarArray(int[] array)
{
    Array.Sort(array);
    return array;
}

// Definición de método para buscar un elemento en un array
int BuscarElemento(int[] array, int elemento)
{
    for (int i = 0; i < array.Length; i++)
    {
        if (array[i] == elemento)
        {
            return i;
        }
    }
    return -1;
}

// Definición de método para eliminar un elemento de un array
int[] EliminarElemento(int[] array, int elemento)
{
    int index = BuscarElemento(array, elemento);
    if (index != -1)
    {
        int[] newArray = new int[array.Length - 1];
        for (int i = 0, j = 0; i < array.Length; i++)
        {
            if (i != index)
            {
                newArray[j++] = array[i];
            }
        }
        return newArray;
    }
    return array;
}

// Definición de método para insertar un elemento en un array
int[] InsertarElemento(int[] array, int elemento)
{
    int[] newArray = new int[array.Length + 1];
    for (int i = 0; i < array.Length; i++)
    {
        newArray[i] = array[i];
    }
    newArray[array.Length] = elemento;
    return newArray;
}

// Ejemplo de uso de los métodos definidos
int numero = 5;
int factorial = Factorial(numero);
Console.WriteLine($"El factorial de {numero} es {factorial}.");

double baseTriangulo = 10;
double alturaTriangulo = 5;
double areaTriangulo = AreaTriangulo(baseTriangulo, alturaTriangulo);
Console.WriteLine($"El área del triángulo es {areaTriangulo}.");

int numeroPrimo = 17;
bool esPrimo = EsPrimo(numeroPrimo);
Console.WriteLine($"El número {numeroPrimo} {(esPrimo ? "es primo" : "no es primo")}.");

string cadena = "Hola mundo";
string cadenaInvertida = InvertirCadena(cadena);
Console.WriteLine($"La cadena invertida es {cadenaInvertida}.");

int numero1 = 12;
int numero2 = 18;
int maximoComunDivisor = MaximoComunDivisor(numero1, numero2);
Console.WriteLine($"El máximo común divisor de {numero1} y {numero2} es {maximoComunDivisor}.");

int[] array = { 1, 3, 5, 7, 9 };
int[] arrayOrdenado = OrdenarArray(array);
Console.WriteLine("El array ordenado es:");
foreach (int elemento in arrayOrdenado)
{
    Console.Write($"{elemento} ");
}
Console.WriteLine();

int elementoABuscar = 7;
int indiceElemento = BuscarElemento(array, elementoABuscar);
Console.WriteLine($"El elemento {elementoABuscar} {(indiceElemento != -1 ? "se encuentra en el índice" : "no se encuentra en el array")}.");

int elementoAEliminar = 5;
int[] arraySinElemento = EliminarElemento(array, elementoAEliminar);
Console.WriteLine("El array sin el elemento eliminado es:");
foreach (int elemento in arraySinElemento)
{
    Console.Write($"{elemento} ");
}
Console.WriteLine();

int elementoAInsertar = 11;
int[] arrayConElementoInsertado = InsertarElemento(array, elementoAInsertar);
Console.WriteLine("El array con el elemento insertado es:");
foreach (int elemento in arrayConElementoInsertado)
```