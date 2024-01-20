```typescript
// Este código simula un sistema de gestión de inventario en una tienda.

// Definimos la interfaz de un producto.
interface Producto {
  id: number;
  nombre: string;
  precio: number;
  cantidad: number;
}

// Creamos un array de productos.
const productos: Producto[] = [
  { id: 1, nombre: "Camiseta", precio: 10, cantidad: 10 },
  { id: 2, nombre: "Pantalón", precio: 20, cantidad: 5 },
  { id: 3, nombre: "Zapatos", precio: 30, cantidad: 15 },
];

// Definimos la interfaz de una venta.
interface Venta {
  id: number;
  fecha: Date;
  productos: Producto[];
  total: number;
}

// Creamos un array de ventas.
const ventas: Venta[] = [];

// Creamos una función para añadir un producto al inventario.
function añadirProducto(producto: Producto) {
  // Comprobamos si el producto ya existe en el inventario.
  const productoExistente = productos.find((p) => p.id === producto.id);

  // Si el producto no existe, lo añadimos al inventario.
  if (!productoExistente) {
    productos.push(producto);
  }
  // Si el producto ya existe, actualizamos su cantidad.
  else {
    productoExistente.cantidad += producto.cantidad;
  }
}

// Creamos una función para vender un producto.
function venderProducto(producto: Producto, cantidad: number) {
  // Comprobamos si el producto existe en el inventario.
  const productoExistente = productos.find((p) => p.id === producto.id);

  // Si el producto no existe, lanzamos un error.
  if (!productoExistente) {
    throw new Error("El producto no existe.");
  }

  // Comprobamos si hay suficiente cantidad del producto en el inventario.
  if (productoExistente.cantidad < cantidad) {
    throw new Error("No hay suficiente cantidad del producto en el inventario.");
  }

  // Actualizamos la cantidad del producto en el inventario.
  productoExistente.cantidad -= cantidad;

  // Creamos una nueva venta.
  const venta: Venta = {
    id: ventas.length + 1,
    fecha: new Date(),
    productos: [producto],
    total: producto.precio * cantidad,
  };

  // Añadimos la venta al array de ventas.
  ventas.push(venta);
}

// Creamos una función para obtener el informe de ventas.
function getInformeVentas() {
  // Creamos un objeto con el informe de ventas.
  const informe: any = {
    totalVentas: 0,
    productosVendidos: 0,
    productosMasVendidos: [],
  };

  // Calculamos el total de ventas y el número de productos vendidos.
  ventas.forEach((venta) => {
    informe.totalVentas += venta.total;
    informe.productosVendidos += venta.productos.length;
  });

  // Obtenemos los productos más vendidos.
  const productosVendidos = ventas.reduce((acc, venta) => {
    venta.productos.forEach((producto) => {
      if (!acc[producto.id]) {
        acc[producto.id] = 0;
      }
      acc[producto.id] += 1;
    });
    return acc;
  }, {});

  const productosMasVendidos = Object.entries(productosVendidos)
    .sort((a, b) => b[1] - a[1])
    .slice(0, 3)
    .map((producto) => productos.find((p) => p.id === parseInt(producto[0])));

  // Añadimos los productos más vendidos al informe.
  informe.productosMasVendidos = productosMasVendidos;

  // Devolvemos el informe de ventas.
  return informe;
}

// Añadimos algunos productos al inventario.
añadirProducto({ id: 1, nombre: "Camiseta", precio: 10, cantidad: 10 });
añadirProducto({ id: 2, nombre: "Pantalón", precio: 20, cantidad: 5 });
añadirProducto({ id: 3, nombre: "Zapatos", precio: 30, cantidad: 15 });

// Vendemos algunos productos.
venderProducto({ id: 1, nombre: "Camiseta", precio: 10, cantidad: 5 });
venderProducto({ id: 2, nombre: "Pantalón", precio: 20, cantidad: 3 });
venderProducto({ id: 3, nombre: "Zapatos", precio: 30, cantidad: 2 });

// Obtenemos el informe de ventas.
const informeVentas = getInformeVentas();

// Mostramos el informe de ventas.
console.log(informeVentas);
```

Explicación:

Este código simula un sistema de gestión de inventario en una tienda.

Primero, definimos la interfaz de un producto, que incluye su ID, nombre, precio y cantidad.

Luego, creamos un array de productos, que contiene objetos con los datos de cada producto.

A continuación, definimos la interfaz de una venta, que incluye su ID, fecha, productos vendidos y total de la venta.

Después, creamos un array de ventas, que contiene objetos con los datos de cada venta.

Seguidamente, creamos una función para añadir un producto al inventario. Esta función comprueba si el producto ya existe en el inventario y, si no existe, lo añade. Si el producto ya existe, actualiza su cantidad.

Luego, creamos una función para vender un producto. Esta función comprueba si el producto existe en el inventario y si hay suficiente cantidad del producto en el inventario. Si se cumplen ambas condiciones, actualiza la cantidad del producto en el inventario y crea una nueva venta.

Después, creamos una función para obtener el informe de ventas. Esta función calcula el total de ventas, el número de productos vendidos y los productos más vendidos.

Finalmente, añadimos algunos productos al inventario, vendemos algunos productos y obtenemos el informe de ventas.