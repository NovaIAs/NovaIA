```typescript
// Definir un tipo de dato personalizado para representar un producto
interface IProducto {
  nombre: string;
  descripcion: string;
  precio: number;
  cantidad: number;
}

// Definir un tipo de dato personalizado para representar un carrito de compras
interface ICarrito {
  productos: IProducto[];
  total: number;
}

// Crear una función para agregar un producto al carrito de compras
const agregarProductoAlCarrito = (carrito: ICarrito, producto: IProducto): void => {
  // Actualizar la cantidad del producto en el carrito
  const productoExistente = carrito.productos.find((p) => p.nombre === producto.nombre);
  if (productoExistente) {
    productoExistente.cantidad += producto.cantidad;
  } else {
    // Agregar el producto al carrito
    carrito.productos.push(producto);
  }

  // Actualizar el total del carrito
  carrito.total += producto.precio * producto.cantidad;
};

// Crear una función para eliminar un producto del carrito de compras
const eliminarProductoDelCarrito = (carrito: ICarrito, producto: IProducto): void => {
  // Buscar el producto en el carrito
  const indiceProducto = carrito.productos.findIndex((p) => p.nombre === producto.nombre);
  if (indiceProducto !== -1) {
    // Eliminar el producto del carrito
    carrito.productos.splice(indiceProducto, 1);

    // Actualizar el total del carrito
    carrito.total -= producto.precio * producto.cantidad;
  }
};

// Crear una función para vaciar el carrito de compras
const vaciarCarrito = (carrito: ICarrito): void => {
  // Eliminar todos los productos del carrito
  carrito.productos = [];

  // Actualizar el total del carrito
  carrito.total = 0;
};

// Crear una función para mostrar el contenido del carrito de compras
const mostrarCarrito = (carrito: ICarrito): void => {
  // Recorrer los productos del carrito
  for (const producto of carrito.productos) {
    // Mostrar el nombre, la cantidad y el precio total del producto
    console.log(`${producto.nombre} (${producto.cantidad}) - ${producto.precio * producto.cantidad}`);
  }

  // Mostrar el total del carrito
  console.log(`Total: ${carrito.total}`);
};

// Crear un carrito de compras inicial
const carrito: ICarrito = {
  productos: [],
  total: 0,
};

// Agregar algunos productos al carrito
agregarProductoAlCarrito(carrito, { nombre: 'Manzanas', descripcion: 'Manzanas rojas y jugosas', precio: 1.50, cantidad: 3 });
agregarProductoAlCarrito(carrito, { nombre: 'Peras', descripcion: 'Peras dulces y refrescantes', precio: 2.00, cantidad: 2 });
agregarProductoAlCarrito(carrito, { nombre: 'Uvas', descripcion: 'Uvas negras y sin semillas', precio: 3.00, cantidad: 1 });

// Mostrar el contenido del carrito
mostrarCarrito(carrito);

// Eliminar un producto del carrito
eliminarProductoDelCarrito(carrito, { nombre: 'Peras', descripcion: 'Peras dulces y refrescantes', precio: 2.00, cantidad: 2 });

// Mostrar el contenido del carrito después de eliminar un producto
mostrarCarrito(carrito);

// Vaciar el carrito
vaciarCarrito(carrito);

// Mostrar el contenido del carrito después de vaciarlo
mostrarCarrito(carrito);
```

Explicación del código:

* Se definen dos tipos de datos personalizados: `IProducto` y `ICarrito`. Estos tipos de datos se utilizan para representar los productos y el carrito de compras, respectivamente.
* Se crean tres funciones: `agregarProductoAlCarrito`, `eliminarProductoDelCarrito` y `vaciarCarrito`. Estas funciones se utilizan para agregar, eliminar y vaciar el carrito de compras, respectivamente.
* Se crea una función `mostrarCarrito` que se utiliza para mostrar el contenido del carrito de compras.
* Se crea un carrito de compras inicial y se agregan algunos productos al carrito.
* Se muestran los productos en el carrito, se elimina un producto y luego se muestra de nuevo el carrito.
* Finalmente, se vacía el carrito y se muestra de nuevo el carrito.