```
// Este programa calcula el factorial de un número usando recursividad.

// La función factorial() toma un número entero positivo como argumento y devuelve su factorial.
fn factorial(n: u32) -> u32 {
    if n == 0 {
        1
    } else {
        n * factorial(n - 1)
    }
}

// La función main() es el punto de entrada del programa.
fn main() {
    // Solicitar al usuario que introduzca un número.
    println!("Introduzca un número:");
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Error al leer la entrada");

    // Convertir la entrada a un número entero.
    let n = input.trim().parse::<u32>().expect("Error al convertir la entrada a un número");

    // Calcular el factorial del número.
    let factorial = factorial(n);

    // Mostrar el factorial del número.
    println!("El factorial de {} es {}", n, factorial);
}
```

Explicación:

* La función `factorial()` toma un número entero positivo como argumento y devuelve su factorial. La función utiliza recursividad para calcular el factorial. Si el número es 0, la función devuelve 1. De lo contrario, la función multiplica el número por el factorial del número anterior y devuelve el resultado.
* La función `main()` es el punto de entrada del programa. Esta función solicita al usuario que introduzca un número, convierte la entrada a un número entero y calcula el factorial del número utilizando la función `factorial()`. A continuación, la función muestra el factorial del número.