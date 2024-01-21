```
// Programa que calcula el área de un triángulo y llama a una función para imprimir el resultado.

// Definimos una función para imprimir el resultado.
fn imprimir_resultado(area: f64) {
    // Imprimimos el área con dos decimales.
    println!("El área del triángulo es {:.2} metros cuadrados.", area);
}

// Definimos una función para calcular el área de un triángulo a partir de su base y su altura.
fn calcular_area(base: f64, altura: f64) -> f64 {
    // Calculamos el área utilizando la fórmula base * altura / 2.
    let area = (base * altura) / 2.0;

    // Devolvemos el área calculada.
    area
}

// Pedimos al usuario que introduzca la base y la altura del triángulo.
println!("Introduce la base del triángulo en metros:");
let base = f64::from_str(&mut io::stdin().read_line().unwrap()).unwrap();
println!("Introduce la altura del triángulo en metros:");
let altura = f64::from_str(&mut io::stdin().read_line().unwrap()).unwrap();

// Calculamos el área del triángulo.
let area = calcular_area(base, altura);

// Imprimimos el área del triángulo.
imprimir_resultado(area);
```

Explicación:

* El programa comienza definiendo una función llamada `imprimir_resultado()`, que toma como parámetro un número de punto flotante y lo imprime con dos decimales.
* A continuación, se define una función llamada `calcular_area()`, que toma como parámetros la base y la altura de un triángulo y devuelve el área del mismo.
* El programa solicita al usuario que introduzca la base y la altura del triángulo.
* A continuación, el programa llama a la función `calcular_area()` para calcular el área del triángulo.
* Finalmente, el programa llama a la función `imprimir_resultado()` para imprimir el área del triángulo.