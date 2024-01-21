```rust
// Un programa que calcula el área de un triángulo.

use std::io;

fn main() {
    // Declaramos las variables que vamos a utilizar.

    // La base del triángulo.
    let mut base: f32;

    // La altura del triángulo.
    let mut altura: f32;

    // El área del triángulo.
    let mut area: f32;

    // Pedimos al usuario que nos introduzca la base del triángulo.

    println!("Introduce la base del triángulo:");
    let entrada_base = io::stdin().read_line().unwrap();
    base = entrada_base.trim().parse::<f32>().unwrap();

    // Pedimos al usuario que nos introduzca la altura del triángulo.

    println!("Introduce la altura del triángulo:");
    let entrada_altura = io::stdin().read_line().unwrap();
    altura = entrada_altura.trim().parse::<f32>().unwrap();

    // Calculamos el área del triángulo.

    area = 0.5 * base * altura;

    // Mostramos el área del triángulo al usuario.

    println!("El área del triángulo es: {}", area);
}

```

Explicación del código:

* El programa define primero las variables que va a utilizar: la base del triángulo (`base`), la altura del triángulo (`altura`) y el área del triángulo (`area`).
* A continuación, pide al usuario que introduzca la base y la altura del triángulo.
* El programa utiliza la función `parse` para convertir la entrada del usuario en un número de punto flotante.
* Una vez que el programa tiene la base y la altura del triángulo, calcula el área del triángulo utilizando la fórmula `area = 0.5 * base * altura`.
* Finalmente, el programa muestra el área del triángulo al usuario.