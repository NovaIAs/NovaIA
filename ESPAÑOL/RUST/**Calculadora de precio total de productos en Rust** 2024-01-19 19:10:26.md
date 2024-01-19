```rust
// Definimos una estructura de datos para representar un producto.
#[derive(Debug)]
struct Producto {
    nombre: String,
    precio: f32,
    cantidad: i32,
}

// Creamos una función para crear un nuevo producto.
fn crear_producto(nombre: &str, precio: f32, cantidad: i32) -> Producto {
    Producto {
        nombre: nombre.to_string(),
        precio: precio,
        cantidad: cantidad,
    }
}

// Creamos una función para calcular el precio total de una lista de productos.
fn calcular_precio_total(productos: &[Producto]) -> f32 {
    productos.iter().map(|producto| producto.precio * producto.cantidad as f32).sum()
}

// Creamos una lista de productos.
let productos = vec![
    crear_producto("Manzana", 1.50, 3),
    crear_producto("Naranja", 2.00, 2),
    crear_producto("Plátano", 1.00, 5),
];

// Calculamos el precio total de la lista de productos.
let precio_total = calcular_precio_total(&productos);

// Mostramos el precio total.
println!("El precio total es: {}", precio_total);
```

Explicación del código:

1. Definimos una estructura de datos llamada `Producto` para representar un producto. La estructura tiene tres campos: `nombre`, `precio` y `cantidad`.
2. Creamos una función llamada `crear_producto` que toma tres parámetros: `nombre`, `precio` y `cantidad` y devuelve un nuevo producto.
3. Creamos una función llamada `calcular_precio_total` que toma una lista de productos como parámetro y devuelve el precio total de la lista.
4. Creamos una lista de productos y la asignamos a la variable `productos`.
5. Llamamos a la función `calcular_precio_total` con la lista de productos y asignamos el resultado a la variable `precio_total`.
6. Mostramos el precio total en la consola.