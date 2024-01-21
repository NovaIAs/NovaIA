```rust
// Este es un comentario en Rust.

// Esta es una función que imprime un mensaje en la consola.
fn imprimir_mensaje() {
    println!("Hola, mundo!");
}

// Esta es una estructura que representa un punto en el espacio 2D.
#[derive(Debug)] // Esta macro nos permite imprimir el contenido de la estructura de forma ordenada.
struct Punto2D {
    x: f64, // La coordenada x del punto.
    y: f64, // La coordenada y del punto.
}

// Esta es una estructura que representa una línea en el espacio 2D.
#[derive(Debug)]
struct Linea2D {
    punto1: Punto2D, // El primer punto de la línea.
    punto2: Punto2D, // El segundo punto de la línea.
}

// Esta es una función que calcula la distancia entre dos puntos en el espacio 2D.
fn distancia_entre_puntos(punto1: Punto2D, punto2: Punto2D) -> f64 {
    // Calculamos la diferencia entre las coordenadas x e y de los dos puntos.
    let dx = punto2.x - punto1.x;
    let dy = punto2.y - punto1.y;

    // Calculamos el cuadrado de la distancia entre los dos puntos.
    let distancia_cuadrado = dx * dx + dy * dy;

    // Devolvemos la raíz cuadrada de la distancia cuadrada.
    distancia_cuadrado.sqrt()
}

// Esta es una función que calcula la longitud de una línea en el espacio 2D.
fn longitud_de_linea(linea: Linea2D) -> f64 {
    // Calculamos la distancia entre los dos puntos de la línea.
    let distancia = distancia_entre_puntos(linea.punto1, linea.punto2);

    // Devolvemos la longitud de la línea.
    distancia
}

// Esta es la función principal del programa.
fn main() {
    // Imprimimos un mensaje en la consola.
    imprimir_mensaje();

    // Creamos un punto en el espacio 2D.
    let punto1 = Punto2D { x: 0.0, y: 0.0 };

    // Creamos otro punto en el espacio 2D.
    let punto2 = Punto2D { x: 1.0, y: 1.0 };

    // Creamos una línea entre los dos puntos.
    let linea = Linea2D { punto1: punto1, punto2: punto2 };

    // Imprimimos la longitud de la línea.
    println!("Longitud de la línea: {}", longitud_de_linea(linea));
}
```

Este código es un programa sencillo en Rust que calcula la longitud de una línea en el espacio 2D. El programa está dividido en varias funciones:

* `imprimir_mensaje()`: Esta función imprime un mensaje en la consola.
* `Punto2D`: Esta estructura representa un punto en el espacio 2D.
* `Linea2D`: Esta estructura representa una línea en el espacio 2D.
* `distancia_entre_puntos()`: Esta función calcula la distancia entre dos puntos en el espacio 2D.
* `longitud_de_linea()`: Esta función calcula la longitud de una línea en el espacio 2D.
* `main()`: Esta es la función principal del programa.

El programa primero imprime un mensaje en la consola. Luego, crea dos puntos en el espacio 2D y una línea entre ellos. Finalmente, calcula la longitud de la línea y la imprime en la consola.