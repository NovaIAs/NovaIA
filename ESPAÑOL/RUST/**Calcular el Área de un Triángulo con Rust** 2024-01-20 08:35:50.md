```rust
// Ejercicio: Crear un programa que calcule el área de un triángulo.

// Primero, definimos la función que calculará el área del triángulo.
fn calcular_area_triángulo(base: f64, altura: f64) -> f64 {
    // El área de un triángulo es igual a la mitad de la base por la altura.
    return 0.5 * base * altura;
}

// Ahora, definimos la función principal del programa.
fn main() {
    // Pedimos al usuario que introduzca la base y la altura del triángulo.
    println!("Introduzca la base del triángulo:");
    let base = read_line().trim().parse::<f64>().unwrap();

    println!("Introduzca la altura del triángulo:");
    let altura = read_line().trim().parse::<f64>().unwrap();

    // Calculamos el área del triángulo usando la función que definimos anteriormente.
    let area = calcular_area_triángulo(base, altura);

    // Imprimimos el resultado en la consola.
    println!("El área del triángulo es: {}", area);
}
```

Explicación del código:

1. Primero, definimos la función `calcular_area_triángulo` que recibe la base y la altura del triángulo como parámetros y retorna el área del triángulo.

2. En la función `main`, primero pedimos al usuario que introduzca la base y la altura del triángulo usando `println!` y `read_line!`.

3. Luego, parseamos las entradas del usuario a números flotantes usando `parse::<f64>().unwrap()`.

4. Calculamos el área del triángulo llamando a la función `calcular_area_triángulo` con la base y la altura como argumentos.

5. Finalmente, imprimimos el resultado en la consola usando `println!`.