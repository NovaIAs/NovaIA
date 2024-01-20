```rust
// Este código está diseñado para encontrar la solución a un sistema de ecuaciones lineales
// utilizando el método de Gauss-Jordan.

// Definimos la matriz de coeficientes y el vector de constantes.
let matriz = [[1.0, 2.0, 3.0],
              [4.0, 5.0, 6.0],
              [7.0, 8.0, 9.0]];
let constantes = [10.0, 11.0, 12.0];

// Creamos una matriz aumentada que combina la matriz de coeficientes y
// el vector de constantes en una sola matriz.
let matriz_aumentada = [matriz, constantes];

// Realizamos el método de Gauss-Jordan
// Creamos una nueva matriz que será donde se guardará la matriz aumentada
// transformada.
let matriz_transformada = matriz_aumentada.clone();

// Convertimos la matriz a una matriz triangular superior.
for i in 0..matriz_transformada.len() {
    // Hacemos que los valores de la diagonal principal sean 1.
    // Dividimos la fila actual por el elemento de la diagonal principal.
    let elemento_pivote = matriz_transformada[i][i];
    for j in 0..matriz_transformada[i].len() {
        matriz_transformada[i][j] /= elemento_pivote;
    }

    // Eliminamos los elementos de la columna actual que están por encima de la diagonal principal.
    // Para cada fila por encima de la fila actual, restamos un múltiplo de la fila actual de la fila superior.
    for k in 0..i {
        // Calculamos el multiplicador.
        let multiplicador = -matriz_transformada[k][i];

        // Restamos el múltiplo de la fila actual de la fila superior.
        for j in 0..matriz_transformada[i].len() {
            matriz_transformada[k][j] += multiplicador * matriz_transformada[i][j];
        }
    }

    // Eliminamos los elementos de la columna actual que están por debajo de la diagonal principal.
    // Para cada fila por debajo de la fila actual, restamos un múltiplo de la fila actual de la fila inferior.
    for k in i+1..matriz_transformada.len() {
        // Calculamos el multiplicador.
        let multiplicador = -matriz_transformada[k][i];

        // Restamos el múltiplo de la fila actual de la fila inferior.
        for j in 0..matriz_transformada[i].len() {
            matriz_transformada[k][j] += multiplicador * matriz_transformada[i][j];
        }
    }
}

// Extraemos el vector de soluciones de la matriz transformada.
let soluciones = [0.0; matriz.len()];
for i in 0..soluciones.len() {
    soluciones[i] = matriz_transformada[i][matriz_transformada[i].len() - 1];
}

// Imprimimos las soluciones.
println!("Soluciones:");
for solucion in soluciones {
    println!("{}", solucion);
}
```

Explicación del código:

* Definimos la matriz de coeficientes y el vector de constantes.
* Creamos una matriz aumentada que combina la matriz de coeficientes y el vector de constantes en una sola matriz.
* Realizamos el método de Gauss-Jordan:
  * Convertimos la matriz a una matriz triangular superior.
  * Extraemos el vector de soluciones de la matriz transformada.
* Imprimimos las soluciones.