```objective-c
// Definición de la función para calcular la factorial de un número

int factorial(int n) {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

// Función para verificar si un número es primo

BOOL esPrimo(int n) {
    if (n <= 1) {
        return NO;
    } else if (n == 2) {
        return YES;
    } else {
        for (int i = 2; i < n; i++) {
            if (n % i == 0) {
                return NO;
            }
        }
        return YES;
    }
}

// Función para generar una lista de números primos hasta un número dado

NSMutableArray *generarPrimosHasta(int n) {
    NSMutableArray *primos = [[NSMutableArray alloc] init];
    for (int i = 2; i <= n; i++) {
        if (esPrimo(i)) {
            [primos addObject:@(i)];
        }
    }
    return primos;
}

// Función para encontrar el máximo común divisor de dos números

int maximoComunDivisor(int a, int b) {
    int resto;
    while (b != 0) {
        resto = a % b;
        a = b;
        b = resto;
    }
    return a;
}

// Función para encontrar el mínimo común múltiplo de dos números

int minimoComunMultiplo(int a, int b) {
    return (a * b) / maximoComunDivisor(a, b);
}

// Función para realizar la suma de dos matrices bidimensionales

void sumarMatrices(int a[3][3], int b[3][3], int resultado[3][3]) {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            resultado[i][j] = a[i][j] + b[i][j];
        }
    }
}

// Función para generar una serie de Fibonacci hasta un número dado de términos

NSMutableArray *generarFibonacciHasta(int n) {
    NSMutableArray *fibonacci = [[NSMutableArray alloc] init];
    int a = 0;
    int b = 1;
    int c;
    for (int i = 0; i < n; i++) {
        c = a + b;
        [fibonacci addObject:@(a)];
        a = b;
        b = c;
    }
    return fibonacci;
}

// Función para invertir una cadena de caracteres

NSString *invertirCadena(NSString *cadena) {
    NSMutableString *cadenaInvertida = [[NSMutableString alloc] init];
    for (int i = (int)cadena.length - 1; i >= 0; i--) {
        [cadenaInvertida appendString:[NSString stringWithFormat:@"%c", [cadena characterAtIndex:i]]];
    }
    return cadenaInvertida;
}

// Función para ordenar una lista de cadenas de caracteres

void ordenarListaCadenas(NSMutableArray *lista) {
    [lista sortUsingComparator:^NSComparisonResult(NSString *a, NSString *b) {
        return [a compare:b];
    }];
}
```

Explicación del código:

1. La función `factorial()` calcula el factorial de un número dado.
2. La función `esPrimo()` verifica si un número dado es primo.
3. La función `generarPrimosHasta()` genera una lista de números primos hasta un número dado.
4. La función `maximoComunDivisor()` encuentra el máximo común divisor de dos números dados.
5. La función `minimoComunMultiplo()` encuentra el mínimo común múltiplo de dos números dados.
6. La función `sumarMatrices()` suma dos matrices bidimensionales dadas.
7. La función `generarFibonacciHasta()` genera una serie de Fibonacci hasta un número dado de términos.
8. La función `invertirCadena()` invierte una cadena de caracteres dada.
9. La función `ordenarListaCadenas()` ordena una lista de cadenas de caracteres dada.