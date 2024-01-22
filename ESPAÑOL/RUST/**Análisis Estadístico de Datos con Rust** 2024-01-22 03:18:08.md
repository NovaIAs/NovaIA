```rust
// Código complejo en Rust:

// Definimos una función que recibe un vector de números y devuelve su suma.
fn suma_vector(vector: &[f64]) -> f64 {
    let mut suma = 0.0;
    for numero in vector {
        suma += *numero;
    }
    return suma;
}

// Definimos una función que recibe un vector de números y devuelve su media.
fn media_vector(vector: &[f64]) -> f64 {
    let suma = suma_vector(vector);
    let longitud = vector.len() as f64;
    return suma / longitud;
}

// Definimos una función que recibe un vector de números y devuelve su desviación estándar.
fn desviacion_estandar_vector(vector: &[f64]) -> f64 {
    let media = media_vector(vector);
    let mut varianza = 0.0;
    for numero in vector {
        varianza += (*numero - media).powf(2.0);
    }
    let longitud = vector.len() as f64;
    return (varianza / longitud).sqrt();
}

// Definimos una función que recibe un vector de números y devuelve su correlación con otro vector de números.
fn correlacion_vectores(vector1: &[f64], vector2: &[f64]) -> f64 {
    if vector1.len() != vector2.len() {
        panic!("Los vectores deben tener la misma longitud.");
    }
    let media1 = media_vector(vector1);
    let media2 = media_vector(vector2);
    let mut covarianza = 0.0;
    for i in 0..vector1.len() {
        covarianza += (vector1[i] - media1) * (vector2[i] - media2);
    }
    let longitud = vector1.len() as f64;
    return covarianza / (desviacion_estandar_vector(vector1) * desviacion_estandar_vector(vector2));
}

// Definimos una función que recibe un vector de números y devuelve su regresión lineal con otro vector de números.
fn regresion_lineal_vectores(vector1: &[f64], vector2: &[f64]) -> (f64, f64) {
    if vector1.len() != vector2.len() {
        panic!("Los vectores deben tener la misma longitud.");
    }
    let media1 = media_vector(vector1);
    let media2 = media_vector(vector2);
    let covarianza = correlacion_vectores(vector1, vector2) * desviacion_estandar_vector(vector1) * desviacion_estandar_vector(vector2);
    let pendiente = covarianza / desviacion_estandar_vector(vector1).powf(2.0);
    let intercepto = media2 - pendiente * media1;
    return (pendiente, intercepto);
}

// Ejemplo de uso:

let vector1 = vec![1.0, 2.0, 3.0, 4.0, 5.0];
let vector2 = vec![2.0, 4.0, 6.0, 8.0, 10.0];

// Calculamos la suma de los vectores.
