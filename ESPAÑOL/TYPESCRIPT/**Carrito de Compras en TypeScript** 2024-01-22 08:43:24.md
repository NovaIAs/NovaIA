```typescript
// Definición de una interfaz para representar un producto
interface Producto {
  id: number;
  nombre: string;
  precio: number;
  cantidad: number;
}

// Definición de una clase para representar un carrito de compras
class Carrito {
  private productos: Producto[] = [];

  constructor() {
    // Inicializa el carrito con una lista vacía de productos
  }

  // Método para agregar un producto al carrito
  agregarProducto(producto: Producto): void {
    // Busca el producto en el carrito
    const productoExistente = this.productos.find(p => p.id === producto.id);

    // Si el producto ya existe, aumenta su cantidad
    if (productoExistente) {
      productoExistente.cantidad += producto.cantidad;
    } else {
      // Si el producto no existe, lo agrega al carrito
      this.productos.push(producto);
    }
  }

  // Método para eliminar un producto del carrito
  eliminarProducto(idProducto: number): void {
    // Busca el producto en el carrito
    const producto = this.productos.find(p => p.id === idProducto);

    // Si el producto existe, lo elimina del carrito
    if (producto) {
      this.productos = this.productos.filter(p => p.id !== idProducto);
    }
  }

  // Método para obtener el total del carrito
  getTotal(): number {
    // Calcula el total sumando los precios de los productos multiplicados por sus cantidades
    return this.productos.reduce((total, producto) => total + (producto.precio * producto.cantidad), 0);
  }

  // Método para mostrar los productos del carrito en la consola
  mostrarProductos(): void {
    console.log("Productos en el carrito:");
    this.productos.forEach(producto => {
      console.log(`- ${producto.nombre} (cantidad: ${producto.cantidad}) - Precio: $${producto.precio}`);
    });
  }
}

// Crear un nuevo carrito
const carrito = new Carrito();

// Agregar algunos productos al carrito
carrito.agregarProducto({ id: 1, nombre: "Manzana", precio: 10, cantidad: 2 });
carrito.agregarProducto({ id: 2, nombre: "Naranja", precio: 15, cantidad: 3 });
carrito.agregarProducto({ id: 3, nombre: "Pera", precio: 20, cantidad: 1 });

// Eliminar un producto del carrito
carrito.eliminarProducto(2);

// Mostrar los productos del carrito en la consola
carrito.mostrarProductos();

// Obtener el total del carrito
const total = carrito.getTotal();

console.log(`Total del carrito: $${total}`);
```

Este código crea un carrito de compras simple en TypeScript. El carrito puede agregar, eliminar y mostrar productos, y también puede calcular el total del carrito.

El código comienza definiendo una interfaz `Producto` que representa un producto en el carrito. La interfaz tiene cuatro propiedades: `id`, `nombre`, `precio` y `cantidad`.

Luego, el código define una clase `Carrito` que representa el carrito de compras. La clase tiene un constructor que inicializa el carrito con una lista vacía de productos. También tiene métodos para agregar, eliminar y mostrar productos, y para calcular el total del carrito.

Para usar el carrito, primero se crea una nueva instancia de la clase `Carrito`. Luego, se pueden agregar productos al carrito usando el método `agregarProducto`. Los productos se pueden eliminar del carrito usando el método `eliminarProducto`. Para mostrar los productos del carrito, se puede usar el método `mostrarProductos`. Y para obtener el total del carrito, se puede usar el método `getTotal`.

En el ejemplo de código, se crea un nuevo carrito y se agregan algunos productos al carrito. Luego, se elimina un producto del carrito y se muestran los productos del carrito en la consola. Finalmente, se obtiene el total del carrito y se muestra en la consola.