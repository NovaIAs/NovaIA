```rust
// Definición de los módulos.
mod modulo_1 {
    // Definición de las funciones del módulo 1.
    pub fn funcion_1() {
        println!("Esta es la función 1 del módulo 1.");
    }

    pub fn funcion_2() {
        println!("Esta es la función 2 del módulo 1.");
    }
}

mod modulo_2 {
    // Definición de las funciones del módulo 2.
    pub fn funcion_3() {
        println!("Esta es la función 3 del módulo 2.");
    }

    pub fn funcion_4() {
        println!("Esta es la función 4 del módulo 2.");
    }
}

// Función principal del programa.
fn main() {
    // Llamada a las funciones de los módulos.
    modulo_1::funcion_1();
    modulo_1::funcion_2();
    modulo_2::funcion_3();
    modulo_2::funcion_4();
}
```

**Explicación del código:**

* El código define dos módulos: `modulo_1` y `modulo_2`.
* Cada módulo contiene dos funciones: `funcion_1` y `funcion_2` en el caso de `modulo_1`, y `funcion_3` y `funcion_4` en el caso de `modulo_2`.
* La función `main` es la función principal del programa.
* En la función `main`, se llaman a las funciones de los módulos utilizando la sintaxis `nombre_del_módulo::nombre_de_la_función`.
* El código utiliza la biblioteca estándar de Rust para imprimir mensajes en la consola utilizando la función `println!`.

El código es complejo porque:

* Utiliza módulos para organizar el código.
* Utiliza funciones para dividir el código en partes más pequeñas y manejables.
* Utiliza la biblioteca estándar de Rust para realizar tareas comunes, como imprimir mensajes en la consola.