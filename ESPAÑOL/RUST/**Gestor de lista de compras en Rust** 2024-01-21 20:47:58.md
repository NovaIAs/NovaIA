```rust
// Objetivo: Crear un programa que permita a los usuarios agregar y eliminar elementos de una lista de compras, y que muestre la lista en formato de texto o HTML.

// Importamos las bibliotecas necesarias.
use std::io;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

// Creamos una estructura para representar un elemento de la lista de compras.
#[derive(Debug)]
struct ListaDeCompras {
    nombre: String,
    cantidad: i32,
    unidad: String,
}

// Creamos una variable global para almacenar la lista de compras.
let mut lista_de_compras: HashMap<String, ListaDeCompras> = HashMap::new();

// Función que permite agregar un elemento a la lista de compras.
fn agregar_elemento(nombre: &str, cantidad: i32, unidad: &str) {
    lista_de_compras.insert(
        nombre.to_string(),
        ListaDeCompras {
            nombre: nombre.to_string(),
            cantidad: cantidad,
            unidad: unidad.to_string(),
        },
    );
}

// Función que permite eliminar un elemento de la lista de compras.
fn eliminar_elemento(nombre: &str) {
    lista_de_compras.remove(nombre);
}

// Función que permite mostrar la lista de compras en formato de texto.
fn mostrar_lista_texto() {
    println!("Lista de compras:");
    for (nombre, elemento) in lista_de_compras.iter() {
        println!("{}: {} {}.", elemento.nombre, elemento.cantidad, elemento.unidad);
    }
}

// Función que permite mostrar la lista de compras en formato HTML.
fn mostrar_lista_html() {
    let mut file = File::create("lista_de_compras.html").unwrap();
    write!(&mut file, "<h1>Lista de compras</h1>\n<ul>\n").unwrap();
    for (nombre, elemento) in lista_de_compras.iter() {
        write!(&mut file, "<li>{}: {} {}.</li>\n", elemento.nombre, elemento.cantidad, elemento.unidad).unwrap();
    }
    write!(&mut file, "</ul>").unwrap();
}

// Función principal.
fn main() {
    // Agregamos algunos elementos a la lista de compras.
    agregar_elemento("Manzanas", 3, "piezas");
    agregar_elemento("Naranjas", 2, "piezas");
    agregar_elemento("Leche", 1, "litro");

    // Mostramos la lista de compras en formato de texto.
    mostrar_lista_texto();

    // Eliminamos un elemento de la lista de compras.
    eliminar_elemento("Naranjas");

    // Mostramos la lista de compras en formato HTML.
    mostrar_lista_html();
}
```

Explicación del código:

* **Estructura `ListaDeCompras`:** Esta estructura representa un elemento de la lista de compras. Tiene tres campos: `nombre`, `cantidad` y `unidad`.
* **Variable global `lista_de_compras`:** Esta variable almacena la lista de compras. Es un diccionario que contiene los elementos de la lista de compras, donde la clave es el nombre del elemento y el valor es una instancia de la estructura `ListaDeCompras`.
* **Función `agregar_elemento`:** Esta función permite agregar un elemento a la lista de compras.
* **Función `eliminar_elemento`:** Esta función permite eliminar un elemento de la lista de compras.
* **Función `mostrar_lista_texto`:** Esta función permite mostrar la lista de compras en formato de texto.
* **Función `mostrar_lista_html`:** Esta función permite mostrar la lista de compras en formato HTML.
* **Función `main`:** Esta es la función principal del programa. Aquí se agregan algunos elementos a la lista de compras, se muestra la lista en formato de texto, se elimina un elemento de la lista y se muestra la lista en formato HTML.