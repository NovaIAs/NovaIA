```
// Este es un comentario de línea.

/*
Este es un comentario
de múltiples líneas.
*/

// fn es la palabra clave para definir una función.
// main es el nombre de la función.
// -> () significa que la función no devuelve ningún valor.
fn main() {

    // println! es una macro que imprime texto en la consola.
    // println!("Hola, mundo!");

    // ; es el carácter de final de línea.
    // Las instrucciones en Rust deben terminar con un punto y coma.

    // Puedes definir variables con la palabra clave let.
    // El tipo de la variable se infiere automáticamente.
    let nombre = "Juan";

    // También puedes definir variables con un tipo explícito.
    let apellido: String = String::from("García");

    // Puedes usar operadores aritméticos para realizar operaciones matemáticas.
    let suma = 1 + 2;
    let resta = 3 - 4;
    let multiplicación = 5 * 6;
    let división = 7 / 8;

    // Puedes usar operadores de comparación para comparar dos valores.
    // El resultado de un operador de comparación es siempre un valor booleano.
    let es_mayor_que = 9 > 10;
    let es_menor_que = 11 < 12;
    let es_igual_que = 13 == 13;
    let es_diferente_que = 14 != 15;

    // Puedes usar operadores lógicos para combinar valores booleanos.
    // El operador && significa "y".
    // El operador || significa "o".
    // El operador ! significa "no".
    let es_verdadero = true;
    let es_falso = false;
    let es_verdadero_y_verdadero = true && true;
    let es_verdadero_o_falso = true || false;
    let es_no_verdadero = !true;

    // Puedes usar bucles para repetir código.
    // El bucle for recorre los elementos de una colección.
    let números = vec![1, 2, 3, 4, 5];
    for número in números {
        println!("{}", número);
    }

    // El bucle while se ejecuta mientras una condición sea cierta.
    let mut contador = 0;
    while contador < 10 {
        println!("{}", contador);
        contador += 1;
    }

    // El bucle until se ejecuta hasta que una condición sea cierta.
    let mut contador = 0;
    until contador == 10 {
        println!("{}", contador);
        contador += 1;
    }

    // Puedes usar condicionales para ejecutar código diferente dependiendo de las condiciones.
    // La instrucción if se ejecuta si una condición es cierta.
    if es_verdadero {
        println!("Es verdadero");
    }

    // La instrucción else if se ejecuta si una condición es cierta y la condición anterior fue falsa.
    else if es_falso {
        println!("Es falso");
    }

    // La instrucción else se ejecuta si todas las condiciones anteriores fueron falsas.
    else {
        println!("Ninguna condición es cierta");
    }

    // Puedes usar match para comparar un valor con varios patrones.
    let valor = 10;
    match valor {
        1 => println!("Es uno"),
        2 => println!("Es dos"),
        3 => println!("Es tres"),
        _ => println!("No es uno, dos ni tres"),
    }

    // Puedes usar estructuras para agrupar datos relacionados.
    struct Persona {
        nombre: String,
        apellido: String,
        edad: