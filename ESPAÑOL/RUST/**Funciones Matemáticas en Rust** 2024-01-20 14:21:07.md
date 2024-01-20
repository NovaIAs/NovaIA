```rust
// Definimos la función `factorial` que calcula el factorial de un número.
fn factorial(n: u64) -> u64 {
    // Si `n` es 0, devolvemos 1 (caso base).
    if n == 0 {
        return 1;
    }
    // En caso contrario, devolvemos `n` multiplicado por el factorial de `n-1`.
    n * factorial(n-1)
}

// Definimos la función `combinaciones` que calcula el número de combinaciones posibles de tomar `k` elementos de un conjunto de `n` elementos.
fn combinaciones(n: u64, k: u64) -> u64 {
    // Si `k` es 0, devolvemos 1 (caso base).
    if k == 0 {
        return 1;
    }
    // Si `k` es mayor que `n`, devolvemos 0 (caso base).
    if k > n {
        return 0;
    }
    // En caso contrario, devolvemos el número de combinaciones de tomar `k-1` elementos de un conjunto de `n-1` elementos, más el número de combinaciones de tomar `k` elementos de un conjunto de `n-1` elementos.
    combinaciones(n-1, k-1) + combinaciones(n-1, k)
}

// Definimos la función `permutaciones` que calcula el número de permutaciones posibles de tomar `k` elementos de un conjunto de `n` elementos.
fn permutaciones(n: u64, k: u64) -> u64 {
    // Si `k` es 0, devolvemos 1 (caso base).
    if k == 0 {
        return 1;
    }
    // Si `k` es mayor que `n`, devolvemos 0 (caso base).
    if k > n {
        return 0;
    }
    // En caso contrario, devolvemos `n` multiplicado por el número de permutaciones de tomar `k-1` elementos de un conjunto de `n-1` elementos.
    n * permutaciones(n-1, k-1)
}


// Creamos un nuevo bloque para declarar una función anónima.
// Esta función anónima recibe un argumento de tipo `u64` y devuelve un valor de tipo `u64`.
let calcular_factorial = |x| factorial(x);
// Llamamos a la función anónima con el argumento 5 y almacenamos el resultado en la variable `resultado`.
let resultado = calcular_factorial(5);
// Imprimimos el resultado en la consola.
println!("El factorial de 5 es {}", resultado);

// Creamos un nuevo bloque para declarar una función anónima.
// Esta función anónima recibe dos argumentos de tipo `u64` y devuelve un valor de tipo `u64`.
let calcular_combinaciones = |n, k| combinaciones(n, k);
// Llamamos a la función anónima con los argumentos 5 y 2 y almacenamos el resultado en la variable `resultado`.
let resultado = calcular_combinaciones(5, 2);
// Imprimimos el resultado en la consola.
println!("El número de combinaciones de tomar 2 elementos de un conjunto de 5 elementos es {}", resultado);

// Creamos un nuevo bloque para declarar una función anónima.
// Esta función anónima recibe dos argumentos de tipo `u64` y devuelve un valor de tipo `u64`.
let calcular_permutaciones = |n, k| permutaciones(n, k);
// Llamamos a la función anónima con los argumentos 5 y 2 y almacenamos el resultado en la variable `resultado`.
let resultado = calcular_permutaciones(5, 2);
// Imprimimos el resultado en la consola.
println!("El número de permutaciones de tomar 2 elementos de un conjunto de 5 elementos es {}", resultado);
```

Explicación:

* La función `factorial` calcula el factorial de un número.
* La función `combinaciones` calcula el número de combinaciones posibles de tomar `k` elementos de un conjunto de `n` elementos.
* La función `permutaciones` calcula el número de permutaciones posibles de tomar `k` elementos de un conjunto de `n` elementos.

En la parte final del código creamos tres funciones anónimas, una para cada una de las funciones definidas anteriormente.
Llamamos a las funciones anónimas con diferentes argumentos y almacenamos los resultados en variables.
Finalmente, imprimimos los resultados en la consola.