```rust
// Módulo principal
mod main {
    // Función principal
    fn main() {
        // Vamos a solicitar al usuario que ingrese una lista de números separados por comas
        let mut input = String::new();
        println!("Ingrese una lista de números separados por comas:");
        io::stdin().read_line(&mut input).expect("Error al leer la entrada");

        // Eliminamos los espacios en blanco de la entrada
        let input = input.trim();

        // Convertimos la entrada en un vector de strings
        let numbers: Vec<&str> = input.split(',').collect();

        // Creamos un vector de enteros a partir del vector de strings
        let mut numbers_int: Vec<i32> = Vec::new();
        for number in numbers {
            let number_int: i32 = number.parse().expect("Error al convertir a entero");
            numbers_int.push(number_int);
        }

        // Calculamos la suma de los números
        let suma = numbers_int.iter().sum();

        // Calculamos el promedio de los números
        let promedio = suma / numbers_int.len() as i32;

        // Imprimimos la suma y el promedio de los números
        println!("La suma de los números es: {}", suma);
        println!("El promedio de los números es: {}", promedio);
    }
}
```

**Explicación:**

* El código anterior es un programa en Rust que solicita al usuario que ingrese una lista de números separados por comas, y luego calcula la suma y el promedio de los números ingresados.
* El programa utiliza la función `read_line()` para leer la entrada del usuario. Esta función toma una variable de tipo `String` como argumento y la llena con la entrada del usuario.
* El programa utiliza la función `trim()` para eliminar los espacios en blanco de la entrada del usuario.
* El programa utiliza la función `split()` para dividir la entrada del usuario en un vector de strings. La función `split()` toma un carácter separador como argumento y divide la cadena en substrings en cada ocurrencia del separador.
* El programa utiliza un bucle `for` para convertir cada string del vector de strings en un entero.
* El programa utiliza la función `sum()` para calcular la suma de los números en el vector de enteros.
* El programa utiliza la función `len()` para obtener la longitud del vector de enteros.
* El programa utiliza la operación `/` para calcular el promedio de los números en el vector de enteros.
* El programa utiliza la función `println()` para imprimir la suma y el promedio de los números en la consola.