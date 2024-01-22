```rust
// Crear una macro que tome dos argumentos y los imprima en la consola.
macro_rules! imprimir {
    ($a:expr, $b:expr) => {
        println!("El primer argumento es {} y el segundo argumento es {}", $a, $b);
    };
}

// Crear una función que tome un vector de números y devuelva la suma de los mismos.
fn sumar(numeros: &[i32]) -> i32 {
    // Utilizar la función `iter()` para convertir el vector en un iterador.
    let iterador = numeros.iter();

    // Utilizar la función `fold()` para sumar los elementos del iterador.
    let suma = iterador.fold(0, |acumulador, &elemento| acumulador + elemento);

    // Devolver la suma.
    suma
}

// Crear una función que reciba como argumento un número y devuelva el factorial del mismo.
fn factorial(n: i32) -> i32 {
    // Casos base:
    if n == 0 {
        return 1;
    }
    if n == 1 {
        return 1;
    }

    // Caso recursivo:
    n * factorial(n - 1)
}

// Crear una función que tome un string y devuelva una lista con las palabras que contiene.
fn split_palabras(s: &str) -> Vec<String> {
    // Utilizar la función `split()` para dividir el string en una lista de substrings.
    let palabras = s.split(' ').collect();

    // Devolver la lista de palabras.
    palabras
}

// Crear una función que tome una lista de palabras y devuelva el número de palabras que empiezan por una vocal.
fn contar_palabras_empezando_por_vocal(palabras: &[String]) -> usize {
    // Utilizar la función `iter()` para convertir la lista en un iterador.
    let iterador = palabras.iter();

    // Utilizar la función `filter()` para filtrar las palabras que empiezan por una vocal.
    let palabras_que_empiezan_por_vocal = iterador.filter(|palabra| {
        let primera_letra = palabra.chars().next().unwrap();
        primera_letra == 'a' ||
        primera_letra == 'e' ||
        primera_letra == 'i' ||
        primera_letra == 'o' ||
        primera_letra == 'u'
    });

    // Contar el número de palabras que empiezan por una vocal.
    palabras_que_empiezan_por_vocal.count()
}

// Crear una función principal.
fn main() {
    // Utilizar la macro `imprimir!` para imprimir dos argumentos en la consola.
    imprimir!("Hola", "mundo!");

    // Crear un vector de números.
    let numeros = vec![1, 2, 3, 4, 5];

    // Utilizar la función `sumar()` para sumar los elementos del vector.
    let suma = sumar(&numeros);

    // Imprimir la suma en la consola.
    println!("La suma de los números es {}", suma);

    // Crear un número.
    let n = 5;

    // Utilizar la función `factorial()` para calcular el factorial del número.
    let factorial_n = factorial(n);

    // Imprimir el factorial en la consola.
    println!("El factorial de {} es {}", n, factorial_n);

    // Crear un string.
    let s = "Hola mundo!";

    // Utilizar la función `split_palabras()` para dividir el string en una lista de palabras.
    let palabras = split_palabras(s);

    // Imprimir la lista de palabras en la consola.
    println!("Las palabras del string son:");
    for palabra in palabras {
        println!("{}", palabra);
    }

    // Utilizar la función `contar_palabras_empezando_por_vocal()` para contar el número de palabras que empiezan por una vocal.
    let numero_palabras_que_empiezan_por_vocal = contar_palabras_empezando_por_vocal(&palabras);

    // Imprimir el número de palabras que empiezan por una vocal en la consola.
    println!("El número de palabras que empiezan por una vocal es {}", numero_palabras_que_empiezan_por_vocal);
}
```

Explicación del código:

* La macro `imprimir!` toma dos argumentos y los imprime en la consola. Se utiliza para imprimir mensajes en la consola.
* La función `sumar()` toma un vector de números y devuelve la suma de los mismos. Se utiliza para sumar los elementos de un vector.
* La función `factorial()` toma un número y devuelve el factorial del mismo. Se utiliza para calcular el factorial de un número.
* La función `split_palabras()` toma un string y devuelve una lista con las palabras que contiene. Se utiliza para dividir un string en una lista de palabras.
* La función `contar_palabras_empezando_por_vocal()` toma una lista de palabras y devuelve el número de palabras que empiezan por una vocal. Se utiliza para contar el número de palabras que empiezan por una vocal en una lista de palabras.
* La función `main()` es la función principal del programa. Se llama cuando se ejecuta el programa. En esta función se llama a las demás funciones para realizar las operaciones deseadas.