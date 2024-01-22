```rust
// Definimos una función que recibe un vector de números y devuelve el máximo.
fn max(mut v: &mut Vec<i32>) -> i32 {
    let mut max = v[0];
    for i in 1..v.len() {
        if v[i] > max {
            max = v[i];
        }
    }
    max
}

// Definimos una función que recibe un vector de números y devuelve el mínimo.
fn min(mut v: Vec<i32>) -> i32 {
    let mut min = v[0];
    for i in 1..v.len() {
        if v[i] < min {
            min = v[i];
        }
    }
    min
}

// Definimos una función que recibe un vector de números y devuelve la media.
fn mean(mut v: Vec<i32>) -> f32 {
    let sum = v.iter().sum();
    let mean = sum as f32 / v.len() as f32;
    mean
}

// Definimos una función que recibe un vector de números y devuelve la desviación estándar.
fn stddev(mut v: Vec<i32>) -> f32 {
    let mean = mean(v.clone());
    let sum_of_squares = v.iter().map(|x| (x - mean).pow(2)).sum();
    let variance = sum_of_squares as f32 / (v.len() - 1) as f32;
    let stddev = variance.sqrt();
    stddev
}

// Definimos una función que recibe un vector de números y devuelve el rango.
fn range(mut v: Vec<i32>) -> i32 {
    let max = max(&mut v);
    let min = min(v);
    max - min
}

// Definimos una función que recibe un vector de números y devuelve la mediana.
fn median(mut v: Vec<i32>) -> i32 {
    v.sort();
    let mid = v.len() / 2;
    if v.len() % 2 == 0 {
        (v[mid] + v[mid - 1]) / 2
    } else {
        v[mid]
    }
}

// Definimos una función que recibe un vector de números y devuelve la moda.
fn mode(mut v: Vec<i32>) -> i32 {
    let mut counts = HashMap::new();
    for num in v.iter() {
        *counts.entry(num).or_insert(0) += 1;
    }
    let max_count = counts.values().max().unwrap();
    let mut modes = Vec::new();
    for (num, count) in counts.iter() {
        if count == max_count {
            modes.push(*num);
        }
    }
    modes[0]
}

// Definimos una función que recibe un vector de números y devuelve los cuartiles.
fn quartiles(mut v: Vec<i32>) -> (i32, i32, i32) {
    v.sort();
    let mid = v.len() / 2;
    let q1 = median(v[0..mid].to_vec());
    let q2 = median(v.clone());
    let q3 = median(v[mid..].to_vec());
    (q1, q2, q3)
}

// Definimos una función que recibe un vector de números y devuelve el coeficiente de correlación de Pearson.
fn pearson_correlation_coefficient(x: Vec<i32>, y: Vec<i32>) -> f32 {
    let mean_x = mean(x.clone());
    let mean_y = mean(y.clone());
    let sum_of_products = x.iter().zip(y.iter()).map(|(x_i, y_i)| (x_i - mean_x) * (y_i - mean_y)).sum();
    let sum_of_squared_differences_x = x.iter().map(|x_i| (x_i - mean_x).pow(2)).sum();
    let sum_of_squared_differences_y = y.iter().map(|y_i| (y_i - mean_y).pow(2)).sum();
    sum_of_products as f32 / (sum_of_squared_differences_x as f32 * sum_of_squared_differences_y as f32).sqrt()
}

fn main() {
    // Creamos un vector de números.
    let v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Imprimimos el máximo.
    println!("Máximo: {}", max(&mut v));

    // Imprimimos el mínimo.
    println!("Mínimo: {}", min(v));

    // Imprimimos la media.
    println!("Media: {}", mean(v));

    // Imprimimos la desviación estándar.
    println!("Desviación estándar: {}", stddev(v));

    // Imprimimos el rango.
    println!("Rango: {}", range(v));

    // Imprimimos la mediana.
    println!("Mediana: {}", median(v));

    // Imprimimos la moda.
    println!("Moda: {}", mode(v));

    // Imprimimos los cuartiles.
    println!("Cuartiles: {:?}", quartiles(v));

    // Imprimimos el coeficiente de correlación de Pearson.
    println!("Coeficiente de correlación de Pearson: {}", pearson_correlation_coefficient(v.clone(), v));
}
```

Este código es un ejemplo de una función muy compleja en Rust que realiza una serie de cálculos estadísticos sobre un vector de números. La función incluye cálculos como el máximo, el mínimo, la media, la desviación estándar, el rango, la mediana, la moda, los cuartiles y el coeficiente de correlación de Pearson.

El código está bien documentado con comentarios que explican cada función y cada paso del cálculo. También está bien estructurado con funciones separadas para cada cálculo, lo que hace que sea fácil de leer y mantener.

Aquí hay algunas explicaciones adicionales sobre algunas de las funciones:

* La función `max` utiliza un bucle `for` para iterar sobre el vector y encontrar el máximo.
* La función `min` utiliza un bucle `for` para iterar sobre el vector y encontrar el mínimo.
* La función `mean` utiliza la función `sum` para sumar todos los números del vector y luego divide la suma entre el número de elementos en el vector para obtener la media.
* La función `stddev` utiliza la función `mean` para calcular la media del vector y luego calcula la desviación estándar utilizando la fórmula:

```
stddev = sqrt(sum_of_squares / (n - 1))
```

donde `sum_of_squares` es la suma de los cuadrados de las diferencias entre cada número del vector y la media, y `n` es el número de elementos en el vector.
* La función `range` utiliza las funciones `max` y `min` para calcular el rango del vector.
* La función `median` utiliza la función `sort` para ordenar el vector y luego calcula la mediana utilizando la fórmula:

```
median = (v[mid] + v[mid - 1]) / 2
```

donde `mid` es el índice del elemento medio del vector.
* La función `mode` utiliza un `HashMap` para contar el número de veces que cada número aparece en el vector y luego devuelve el número que aparece más veces.
* La función `quartiles` utiliza la función `sort` para ordenar el vector y luego calcula los cuartiles utilizando las fórmulas:

```
q1 = median(v[0..mid])
q2 = median(v)
q3 = median(v[mid..])
```

donde `mid` es el índice del elemento medio del vector.
* La función `pearson_correlation_coefficient` utiliza las funciones `mean` y `sum` para calcular el coeficiente de correlación de Pearson utilizando la fórmula:

```
r = sum_of_products / (sqrt(sum_of_squared_differences_x) * sqrt(sum_of_squared_differences_y))
```

donde `sum_of_products` es la suma de los productos de las diferencias entre cada número del vector `x` y la media de `x` y las diferencias entre cada número del vector `y` y la media de `y`, `sum_of_squared_differences_x` es la suma de los cuadrados de las diferencias entre cada número del vector `x` y la media de `x`, y `sum_of_squared_differences_y` es la suma de los cuadrados de las diferencias entre cada número del vector `y` y la media de `y`.