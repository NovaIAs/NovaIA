```rust
// Este código es un ejemplo de cómo crear una red neuronal artificial simple en Rust.
// La red neuronal se entrena en un conjunto de datos de dígitos escritos a mano,
// y luego se utiliza para clasificar nuevos dígitos.

// Primero, necesitamos importar las bibliotecas necesarias.
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

// A continuación, definimos la estructura de la red neuronal.
struct RedNeuronal {
    // La red neuronal tiene tres capas: una capa de entrada, una capa oculta y una capa de salida.
    // La capa de entrada tiene 784 neuronas, una por cada píxel de una imagen de 28x28.
    // La capa oculta tiene 100 neuronas.
    // La capa de salida tiene 10 neuronas, una por cada dígito.

    // Los pesos de la red neuronal son almacenados en dos matrices:
    // - `pesos_entrada_oculta` almacena los pesos entre la capa de entrada y la capa oculta.
    // - `pesos_oculta_salida` almacena los pesos entre la capa oculta y la capa de salida.

    pesos_entrada_oculta: Vec<Vec<f32>>,
    pesos_oculta_salida: Vec<Vec<f32>>,
}

// Implementamos el método `new` para crear una nueva red neuronal.
impl RedNeuronal {
    fn new() -> RedNeuronal {
        // Creamos las matrices de pesos con valores aleatorios.

        let pesos_entrada_oculta: Vec<Vec<f32>> = (0..100).map(|_| {
            (0..784).map(|_| rand::random::<f32>()).collect()
        }).collect();

        let pesos_oculta_salida: Vec<Vec<f32>> = (0..10).map(|_| {
            (0..100).map(|_| rand::random::<f32>()).collect()
        }).collect();

        // Devolvemos la nueva red neuronal.

        RedNeuronal {
            pesos_entrada_oculta,
            pesos_oculta_salida,
        }
    }
}

// A continuación, definimos la función `entrenar` para entrenar la red neuronal.
// La función toma como argumento un conjunto de datos de dígitos escritos a mano.
fn entrenar(red_neuronal: &mut RedNeuronal, datos_entrenamiento: &[(Vec<f32>, Vec<f32>)]) {
    // Iteramos sobre el conjunto de datos de entrenamiento.

    for (imagen, etiqueta) in datos_entrenamiento {
        // Propagamos la imagen a través de la red neuronal.

        let salida = propagar(red_neuronal, &imagen);

        // Calculamos el error entre la salida de la red neuronal y la etiqueta correcta.

        let error = etiqueta - salida;

        // Actualizamos los pesos de la red neuronal.

        actualizar_pesos(red_neuronal, &imagen, &error);
    }
}

// Definimos la función `propagar` para propagar una imagen a través de la red neuronal.
// La función toma como argumento una red neuronal y una imagen.
// Devuelve la salida de la red neuronal.
fn propagar(red_neuronal: &RedNeuronal, imagen: &[f32]) -> Vec<f32> {
    // Calculamos la salida de la capa oculta.

    let salida_oculta = red_neuronal.pesos_entrada_oculta.iter()
        .map(|pesos| {
            pesos.iter()
                .zip(imagen)
                .map(|(peso, valor)| peso * valor)
                .sum()
        })
        .map(|valor| valor.tanh())
        .collect();

    // Calculamos la salida de la capa de salida.

    let salida_salida = red_neuronal.pesos_oculta_salida.iter()
        .map(|pesos| {
            pesos.iter()
                .zip(&salida_oculta)
                .map(|(peso, valor)| peso * valor)
                .sum()
        })
        .map(|valor| valor.tanh())
        .collect();

    // Devolvemos la salida de la capa de salida.

    salida_salida
}

// Definimos la función `actualizar_pesos` para actualizar los pesos de la red neuronal.
// La función toma como argumento una red neuronal, una imagen y un error.
fn actualizar_pesos(red_neuronal: &mut RedNeuronal, imagen: &[f32], error: &[f32]) {
    // Calculamos el gradiente de los pesos entre la capa oculta y la capa de salida.

    let gradiente_salida = error.iter()
        .zip(&red_neuronal