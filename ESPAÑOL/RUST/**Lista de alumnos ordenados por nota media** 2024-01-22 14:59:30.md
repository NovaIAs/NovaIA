```rust
// Definición de una estructura Alumno con campos nombre, edad y calificaciones
#[derive(Debug)]
struct Alumno {
    nombre: String,
    edad: u8,
    calificaciones: Vec<f32>,
}

// Función principal del programa
fn main() {
    // Crear una lista de objetos Alumno
    let mut alumnos = Vec::new();

    // Añadir objetos Alumno a la lista
    alumnos.push(Alumno {
        nombre: "Juan".to_string(),
        edad: 18,
        calificaciones: vec![8.5, 7.2, 9.1],
    });
    alumnos.push(Alumno {
        nombre: "María".to_string(),
        edad: 19,
        calificaciones: vec![9.8, 8.7, 8.3],
    });
    alumnos.push(Alumno {
        nombre: "Pedro".to_string(),
        edad: 20,
        calificaciones: vec![7.6, 6.4, 8.9],
    });

    // Calcular la nota media de cada alumno
    for alumno in &mut alumnos {
        let suma_calificaciones = alumno.calificaciones.iter().sum::<f32>();
        let nota_media = suma_calificaciones / alumno.calificaciones.len() as f32;
        alumno.nota_media = nota_media;
    }

    // Ordenar la lista de alumnos por nota media
    alumnos.sort_by_key(|alumno| alumno.nota_media);

    // Imprimir el nombre, la edad y la nota media de cada alumno
    println!("Lista de alumnos ordenados por nota media:");
    for alumno in alumnos {
        println!("- {} ({} años) - Nota media: {:.2}", alumno.nombre, alumno.edad, alumno.nota_media);
    }
}
```

Explicación del código:

1. **Definición de la estructura Alumno:** Creamos una estructura llamada `Alumno` que contiene los siguientes campos:

    - `nombre`: El nombre del alumno como una cadena de caracteres.
    - `edad`: La edad del alumno como un número entero sin signo de 8 bits.
    - `calificaciones`: Una lista de calificaciones del alumno como números de punto flotante.

2. **Función principal del programa:** La función `main` es la función de entrada del programa.

3. **Crear una lista de objetos Alumno:** Creamos una lista mutable de objetos `Alumno` llamada `alumnos`.

4. **Añadir objetos Alumno a la lista:** Añadimos tres objetos `Alumno` a la lista `alumnos` utilizando la función `push`.

5. **Calcular la nota media de cada alumno:** Iteramos sobre cada objeto `Alumno` en la lista `alumnos` y calculamos la nota media del alumno sumando todas sus calificaciones y dividiendo el resultado por el número de calificaciones. Guardamos la nota media en el campo `nota_media` del objeto `Alumno`.

6. **Ordenar la lista de alumnos por nota media:** Ordenamos la lista `alumnos` por nota media utilizando la función `sort_by_key`.

7. **Imprimir el nombre, la edad y la nota media de cada alumno:** Iteramos sobre cada objeto `Alumno` en la lista `alumnos` e imprimimos su nombre, edad y nota media.