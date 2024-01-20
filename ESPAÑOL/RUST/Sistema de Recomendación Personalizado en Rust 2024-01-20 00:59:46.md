```
// Construyendo un Sistema de Recomendación Personalizado en Rust

// Importando las librerías necesarias
extern crate serde;
extern crate serde_json;
extern crate similar;

// Esta es una estructura que representa un usuario.
#[derive(Serialize, Deserialize)]
struct Usuario {
    nombre: String,
    preferencias: Vec<String>,
}

// Ésta es una estructura que representa un producto.
#[derive(Serialize, Deserialize)]
struct Producto {
    nombre: String,
    atributos: Vec<String>,
}

// Esta es una función que calcula la similitud entre dos usuarios.
fn similitud_usuarios(usuario1: &Usuario, usuario2: &Usuario) -> f32 {
    // Convertimos las listas de atributos en arrays de enteros.
    let u1_attrs = usuario1.preferencias.iter().map(|a| a.parse::<i32>().unwrap()).collect::<Vec<i32>>();
    let u2_attrs = usuario2.preferencias.iter().map(|a| a.parse::<i32>().unwrap()).collect::<Vec<i32>>();

    // Calculamos la similitud entre los arrays usando el índice de similitud de Coseno.
    similar::dot(&u1_attrs, &u2_attrs)
}

// Esta es una función que calcula las recomendaciones para un usuario.
fn recomendar_productos(usuario: &Usuario, productos: &Vec<Producto>) -> Vec<String> {
    // Obtenemos una lista de los productos que ya tiene el usuario.
    let usuario_productos = usuario.preferencias.iter().map(|p| p.to_owned()).collect::<Vec<String>>();

    // Calculamos la similitud entre el usuario y todos los demás usuarios.
    let similitudes = productos.iter().map(|p| similitud_usuarios(usuario, p)).collect::<Vec<f32>>();

    // Ordenamos los productos en orden descendente de similitud.
    let productos_ordenados = productos.iter().zip(similitudes).sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

    // Filtramos los productos que ya tiene el usuario.
    let productos_filtrados = productos_ordenados.iter().filter(|p| !usuario_productos.contains(&p.0.nombre)).collect::<Vec<(&Producto, f32)>>();

    // Devolvemos una lista de los nombres de los productos recomendados.
    productos_filtrados.iter().map(|p| p.0.nombre.to_owned()).collect::<Vec<String>>()
}

// Función principal
fn main() {
    // Creamos algunos usuarios y productos de ejemplo.
    let usuarios = vec![
        Usuario {
            nombre: "Juan".to_owned(),
            preferencias: vec!["Música".to_owned(), "Películas".to_owned(), "Libros".to_owned()],
        },
        Usuario {
            nombre: "María".to_owned(),
            preferencias: vec!["Películas".to_owned(), "Videojuegos".to_owned(), "Moda".to_owned()],
        },
        Usuario {
            nombre: "Pedro".to_owned(),
            preferencias: vec!["Libros".to_owned(), "Música".to_owned(), "Tecnología".to_owned()],
        },
    ];

    let productos = vec![
        Producto {
            nombre: "iPhone".to_owned(),
            atributos: vec!["Tecnología".to_owned(), "Electrónica".to_owned()],
        },
        Producto {
            nombre: "Samsung Galaxy".to_owned(),
            atributos: vec!["Tecnología".to_owned(), "Electrónica".to_owned()],
        },
        Producto {
            nombre: "PlayStation 5".to_owned(),
            atributos: vec!["Videojuegos".to_owned(), "Electrónica".to_owned()],
        },
        Producto {
            nombre: "Xbox Series X".to_owned(),
            atributos: vec!["Videojuegos".to_owned(), "Electrónica".to_owned()],
        },
        Producto {
            nombre: "Harry Potter y la Piedra Filosofal".to_owned(),
            atributos: vec!["Libros".to_owned(), "Fantasía".to_owned()],
        },
        Producto {
            nombre: "El Señor de los Anillos".to_owned(),
            atributos: vec!["Libros".to_owned(), "Fantasía".to_owned()],
        },
        Producto {
            nombre: "La Guerra de las Galaxias".to_owned(),
            atributos: vec!["Películas".to_owned(), "Ciencia Ficción".to_owned()],
        },
        Producto {
            nombre: "El Padrino".to_owned(),
            atributos: vec!["Películas".to_owned(), "Drama".to_owned()],
        },
        Producto {
            nombre: "Queen".to_owned(),
            atributos: vec!["Música".to_owned(), "Rock".to_owned()],
        },
        Producto {
            nombre: "The Beatles".to_owned(),