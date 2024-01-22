```d
import std.stdio, std.algorithm, std.math, std.range, std.string;

// Definición de una función que encuentra el máximo común divisor de dos números.
int mcd(int a, int b) pure nothrow @safe {
    while (b) {
        int t = b;
        b = a % b;
        a = t;
    }
    return a;
}

// Definición de una función que encuentra el mínimo común múltiplo de dos números.
int mcm(int a, int b) pure nothrow @safe {
    return (a / mcd(a, b)) * b;
}

// Definición de una función que genera una lista de los primeros n números primos.
List<int> primos(int n) pure nothrow @safe {
    List<int> primos;
    int i = 2;
    while (primos.length < n) {
        bool esPrimo = true;
        for (int j in primos) {
            if (i % j == 0) {
                esPrimo = false;
                break;
            }
        }
        if (esPrimo) {
            primos ~= i;
        }
        i++;
    }
    return primos;
}

// Definición de una función que calcula la serie de Fibonacci hasta el término n.
List<int> fibonacci(int n) pure nothrow @safe {
    List<int> fibonacci = [0, 1];
    while (fibonacci.last < n) {
        int siguiente = fibonacci[-1] + fibonacci[-2];
        fibonacci ~= siguiente;
    }
    return fibonacci;
}

// Definición de una función que calcula la factorial de un número.
int factorial(int n) pure nothrow @safe {
    if (n == 0) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

// Definición de una función que calcula la combinación de n elementos tomados de k en k.
int combinacion(int n, int k) pure nothrow @safe {
    return factorial(n) / (factorial(k) * factorial(n - k));
}

// Definición de una función que calcula la permutación de n elementos tomados de k en k.
int permutacion(int n, int k) pure nothrow @safe {
    return factorial(n) / factorial(n - k);
}

// Definición de una función que calcula la probabilidad de un evento.
double probabilidad(int favorables, int posibles) pure nothrow @safe {
    return (double) favorables / posibles;
}

// Definición de una función que calcula la desviación estándar de una lista de números.
double desviacionEstandar(List<double> datos) pure nothrow @safe {
    double media = datos.sum / datos.length;
    double varianza = 0.0;
    foreach (double dato in datos) {
        varianza += math.pow(dato - media, 2);
    }
    varianza /= (datos.length - 1);
    return math.sqrt(varianza);
}

// Definición de una función que calcula la correlación entre dos listas de números.
double correlacion(List<double> datos1, List<double> datos2) pure nothrow @safe {
    double media1 = datos1.sum / datos1.length;
    double media2 = datos2.sum / datos2.length;
    double covarianza = 0.0;
    for (int i = 0;