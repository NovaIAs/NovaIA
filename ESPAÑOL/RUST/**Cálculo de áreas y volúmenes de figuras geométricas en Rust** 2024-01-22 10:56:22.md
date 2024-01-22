```rust
// Creamos un módulo para organizar nuestro código y mantenerlo limpio.
mod mi_modulo {

    // Función para calcular el área de un triángulo.
    pub fn area_triangulo(base: f64, altura: f64) -> f64 {
        (base * altura) / 2.0
    }

    // Función para calcular el área de un círculo.
    pub fn area_circulo(radio: f64) -> f64 {
        std::f64::consts::PI * radio.powi(2)
    }

    // Función para calcular el área de una esfera.
    pub fn area_esfera(radio: f64) -> f64 {
        4.0 * std::f64::consts::PI * radio.powi(2)
    }

    // Función para calcular el volumen de un cilindro.
    pub fn volumen_cilindro(radio: f64, altura: f64) -> f64 {
        std::f64::consts::PI * radio.powi(2) * altura
    }

    // Función para calcular el volumen de una esfera.
    pub fn volumen_esfera(radio: f64) -> f64 {
        (4.0 / 3.0) * std::f64::consts::PI * radio.powi(3)
    }
}

// Función principal del programa.
fn main() {
    // Imprimimos el área de un triángulo con base 5 y altura 10.
    println!("Área del triángulo: {}", mi_modulo::area_triangulo(5.0, 10.0));

    // Imprimimos el área de un círculo con radio 10.
    println!("Área del círculo: {}", mi_modulo::area_circulo(10.0));

    // Imprimimos el área de una esfera con radio 10.
    println!("Área de la esfera: {}", mi_modulo::area_esfera(10.0));

    // Imprimimos el volumen de un cilindro con radio 10 y altura 20.
    println!("Volumen del cilindro: {}", mi_modulo::volumen_cilindro(10.0, 20.0));

    // Imprimimos el volumen de una esfera con radio 10.
    println!("Volumen de la esfera: {}", mi_modulo::volumen_esfera(10.0));
}
```

Explicación:

* Creamos un módulo llamado `mi_modulo` para organizar nuestro código.
* Definimos varias funciones dentro del módulo para calcular el área y el volumen de diferentes figuras geométricas.
* Usamos el tipo `f64` para representar números de punto flotante de 64 bits.
* Importamos el módulo `std` para acceder a las constantes matemáticas como `PI`.
* La función `main` es el punto de entrada del programa.
* Llamamos a las funciones definidas en el módulo `mi_modulo` para calcular el área y el volumen de diferentes figuras geométricas.
* Imprimimos los resultados usando la función `println!`.