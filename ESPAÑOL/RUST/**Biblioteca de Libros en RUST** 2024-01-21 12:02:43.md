```rust
// Definir una estructura que represente un libro
#[derive(Debug)]
struct Libro {
    titulo: String,
    autor: String,
    paginas: u32,
    precio: f32,
}

// Función para crear un nuevo libro
fn crear_libro(titulo: &str, autor: &str, paginas: u32, precio: f32) -> Libro {
    Libro {
        titulo: titulo.to_string(),
        autor: autor.to_string(),
        paginas,
        precio,
    }
}

// Función para mostrar los detalles de un libro
fn mostrar_libro(libro: &Libro) {
    println!("Título: {}", libro.titulo);
    println!("Autor: {}", libro.autor);
    println!("Páginas: {}", libro.paginas);
    println!("Precio: ${:.2}", libro.precio);
}

// Función principal
fn main() {
    // Crear algunos libros
    let libro1 = crear_libro("El Quijote", "Miguel de Cervantes", 1023, 12.99);
    let libro2 = crear_libro("Cien años de soledad", "Gabriel García Márquez", 417, 10.99);
    let libro3 = crear_libro("El Principito", "Antoine de Saint-Exupéry", 96, 8.99);

    // Mostrar los detalles de cada libro
    mostrar_libro(&libro1);
    println!();
    mostrar_libro(&libro2);
    println!();
    mostrar_libro(&libro3);
}
```

Explicación del código:

1. **Estructura `Libro`**: Define una estructura llamada `Libro` que representa un libro y tiene los siguientes campos: `titulo`, `autor`, `paginas` y `precio`.
2. **Función `crear_libro`**: Define una función llamada `crear_libro` que toma cuatro parámetros: `titulo`, `autor`, `paginas` y `precio` y devuelve un nuevo objeto `Libro`.
3. **Función `mostrar_libro`**: Define una función llamada `mostrar_libro` que toma un objeto `Libro` como parámetro y muestra los detalles del libro en la consola.
4. **Función `main`**: La función `main` es el punto de entrada al programa.
5. **Crear libros**: En la función `main`, se crean tres objetos `Libro` utilizando la función `crear_libro`.
6. **Mostrar libros**: Se llama a la función `mostrar_libro` para mostrar los detalles de cada libro en la consola.