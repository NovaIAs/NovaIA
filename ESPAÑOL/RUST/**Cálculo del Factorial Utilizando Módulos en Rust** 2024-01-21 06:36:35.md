```rust
// Definimos un módulo para encapsular nuestro código.
mod mi_modulo {
    // Definimos una función que calcula el factorial de un número.
    pub fn factorial(n: u32) -> u32 {
        if n == 0 {
            1
        } else {
            n * factorial(n - 1)
        }
    }
}

// Definimos una función principal.
fn main() {
    // Imprimimos el factorial de 5.
    println!("El factorial de 5 es {}", mi_modulo::factorial(5));
}
```

Explicación:

* El código comienza definiendo un módulo llamado `mi_modulo`. Los módulos se utilizan para encapsular código relacionado.
* Dentro del módulo, definimos una función llamada `factorial`. Esta función calcula el factorial de un número.
* La función `factorial` es una función recursiva, lo que significa que se llama a sí misma. Esto le permite calcular el factorial de un número grande dividiéndolo en factoriales de números más pequeños.
* La función `factorial` utiliza la cláusula `if` para comprobar si el número pasado es 0. Si es así, devuelve 1. De lo contrario, multiplica el número por el factorial del número anterior.
* La función principal del programa es `main`. Esta función imprime el factorial de 5.
* Para utilizar la función `factorial` definida en el módulo `mi_modulo`, utilizamos el operador `::`. Este operador nos permite acceder a las funciones y variables definidas en el módulo.