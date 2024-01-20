```c++
// Programa para encontrar el máximo común divisor (MCD) de dos números enteros

// Función para encontrar el MCD de dos números enteros positivos
int mcd(int a, int b) {
    // Si b es 0, entonces el MCD es a
    if (b == 0) {
        return a;
    }
    // Si no, el MCD es el MCD de b y el resto de dividir a entre b
    else {
        return mcd(b, a % b);
    }
}

// Función para encontrar el mínimo común múltiplo (MCM) de dos números enteros positivos
int mcm(int a, int b) {
    // El MCM es el producto de los dos números dividido entre el MCD
    return (a * b) / mcd(a, b);
}

// Función para encontrar el inverso modular de un número entero en un módulo dado
int inverso_modular(int a, int m) {
    // Si a y m son coprimos, entonces el inverso modular es el valor de x tal que a*x % m = 1
    for (int x = 1; x < m; x++) {
        if ((a * x) % m == 1) {
            return x;
        }
    }
    // Si a y m no son coprimos, entonces el inverso modular no existe
    return -1;
}

// Función para encontrar la matriz adjunta de una matriz cuadrada de orden n
int** adjunta(int** A, int n) {
    // La matriz adjunta es la transpuesta de la matriz de cofactores
    int** adj = new int*[n];
    for (int i = 0; i < n; i++) {
        adj[i] = new int[n];
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            adj[i][j] = cofactor(A, i, j, n);
        }
    }
    return adj;
}

// Función para encontrar la matriz inversa de una matriz cuadrada de orden n
int** inversa(int** A, int n) {
    // Si la matriz no es invertible, entonces no existe la matriz inversa
    if (determinante(A, n) == 0) {
        return nullptr;
    }
    // La matriz inversa es el producto de la matriz adjunta y el inverso del determinante
    int** adj = adjunta(A, n);
    int det = determinante(A, n);
    int** inv = new int*[n];
    for (int i = 0; i < n; i++) {
        inv[i] = new int[n];
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            inv[i][j] = (adj[i][j] * inverso_modular(det, n)) % n;
        }
    }
    return inv;
}

// Función para encontrar el determinante de una matriz cuadrada de orden n
int determinante(int** A, int n) {
    // Si la matriz es de orden 1, entonces el determinante es el único elemento de la matriz
    if (n == 1) {
        return A[0][0];
    }
    // Si no, el determinante es la suma de los productos de los elementos de la primera fila y sus cofactores
    else {
        int det = 0;
        for (int i = 0; i < n; i++) {
            det += A[0][i] * cofactor(A, 0, i, n);
        }
        return det;
    }
}

// Función para encontrar el cofactor de un elemento de una matriz cuadrada de orden n
int cofactor(int** A, int i, int j, int n) {
    // El cofactor de un elemento es el determinante de la submatriz formada por eliminar la fila y columna del elemento
    int** submatriz = new int*[n - 1];
    for (int k = 0; k < n - 1; k++) {
        submatriz[k] = new int[n - 1];
    }
    int p = 0, q = 0;
    for (int k = 0; k < n; k++) {
        for (int l = 0; l < n; l++) {
            if (k != i && l != j) {
                submatriz[p][q] = A[k][l];
                q++;
            }
        }
        if (k != i) {
            p++;
            q = 0;
        }
    }
    return pow(-1, i + j) * determinante(submatriz, n - 1);
}

// Función para multiplicar dos matrices cuadradas de orden n
int** multiplicar_matrices(int** A, int** B, int n) {
    // La matriz resultante es una matriz cuadrada de orden n
    int** C = new int*[n];
    for (int i = 0; i < n; i++) {
        C[i] = new int[n];
    }
    // El elemento (i, j) de la matriz resultante es la suma de los productos de los elementos de la fila i de A y los elementos de la columna j de B
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            C[i][j] = 0;
            for (int k = 0; k < n; k++) {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }
    return C;
}

// Función para sumar dos matrices