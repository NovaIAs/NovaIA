```rust
// Declaración de una estructura para representar un libro
#[derive(Debug)]
struct Libro {
    titulo: String,
    autor: String,
    año_publicación: u32,
    precio: f32,
}

// Función para crear un nuevo libro
fn crear_libro(titulo: &str, autor: &str, año_publicación: u32, precio: f32) -> Libro {
    Libro {
        titulo: titulo.to_string(),
        autor: autor.to_string(),
        año_publicación,
        precio,
    }
}

// Función para imprimir la información de un libro
fn imprimir_libro(libro: &Libro) {
    println!("Título: {}", libro.titulo);
    println!("Autor: {}", libro.autor);
    println!("Año de publicación: {}", libro.año_publicación);
    println!("Precio: {:.2}€", libro.precio);
    println!();
}

// Función principal
fn main() {
    // Creación de una lista de libros
    let libros = vec![
        crear_libro("El Quijote", "Miguel de Cervantes", 1605, 15.99),
        crear_libro("El Señor de los Anillos", "J.R.R. Tolkien", 1954, 30.99),
        crear_libro("Cien años de soledad", "Gabriel García Márquez", 1967, 20.99),
        crear_libro("El Principito", "Antoine de Saint-Exupéry", 1943, 12.99),
        crear_libro("Matar a un ruiseñor", "Harper Lee", 1960, 18.99),
    ];

    // Iteración sobre la lista de libros para imprimir su información
    for libro in libros {
        imprimir_libro(&libro);
    }

    // Creación de una biblioteca (hashmap) para almacenar los libros
    let mut biblioteca: HashMap<String, Libro> = HashMap::new();

    // Inserción de los libros en la biblioteca
    for libro in libros {
        biblioteca.insert(libro.titulo.clone(), libro);
    }

    // Búsqueda de un libro por su título
    let titulo_libro_buscado = "El Señor de los Anillos";
    if let Some(libro_buscado) = biblioteca.get(titulo_libro_buscado) {
        println!("Libro encontrado:");
        imprimir_libro(libro_buscado);
    } else {
        println!("Lo siento, no he podido encontrar el libro \"{}\".", titulo_libro_buscado);
    }
}
```

Este código crea una estructura Libro para representar los libros, con campos para el título, autor, año de publicación y precio. También define dos funciones, crear_libro() para crear un nuevo libro e imprimir_libro() para imprimir la información de un libro.

La función main() crea una lista de libros para empezar, luego itera sobre la lista para imprimir la información de cada libro usando la función imprimir_libro().

A continuación, crea una biblioteca (hashmap) para almacenar los libros, e inserta los libros en la biblioteca.

Por último, busca un libro por su título y, si lo encuentra, lo imprime. Si no lo encuentra, imprime un mensaje de error.

Este código es complejo porque utiliza varias características avanzadas de Rust, incluyendo estructuras, funciones, bucles, colecciones y coincidencia de patrones. También demuestra cómo se pueden utilizar las bibliotecas estándar de Rust para crear aplicaciones más complejas y robustas.