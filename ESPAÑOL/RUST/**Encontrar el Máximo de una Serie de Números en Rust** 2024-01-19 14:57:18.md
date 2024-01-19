```
// Programa para encontrar el máximo de una serie de números.

// Función para obtener el máximo de una lista de números.
fn max(lista: &[i32]) -> i32 {
    let mut max = lista[0];
    for &x in &lista[1..] {
        if x > max {
            max = x;
        }
    }
    max
}

// Función para leer una serie de números desde la entrada estándar.
fn leer_lista() -> Vec<i32> {
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Error al leer la entrada.");
    input.split(' ').map(|x| x.parse::<i32>().expect("Error al analizar el número.")).collect()
}

// Función principal.
fn main() {
    // Leer una serie de números desde la entrada estándar.
    let lista = leer_lista();

    // Encontrar el máximo de la lista.
    let maximo = max(&lista);

    // Imprimir el máximo.
    println!("El máximo es: {}", maximo);
}
```

Explicación:

* La función `max` toma una lista de números como argumento y devuelve el máximo de la lista.
* La función `leer_lista` lee una serie de números desde la entrada estándar y los devuelve en una lista.
* La función `main` llama a la función `leer_lista` para leer una serie de números desde la entrada estándar.
* La función `main` llama a la función `max` para encontrar el máximo de la lista.
* La función `main` imprime el máximo.