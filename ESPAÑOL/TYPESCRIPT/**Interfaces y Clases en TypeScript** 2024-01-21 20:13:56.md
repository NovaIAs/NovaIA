```typescript
// Definición de Interfaces

// Interfaz para representar un usuario
interface Usuario {
  id: number;
  nombre: string;
  apellido: string;
  correo: string;
  contraseña: string;
}

// Interfaz para representar un producto
interface Producto {
  id: number;
  nombre: string;
  descripcion: string;
  precio: number;
  cantidadDisponible: number;
}

// Interfaz para representar una cesta de compra
interface CestaCompra {
  id: number;
  usuarioId: number;
  productos: Producto[];
  total: number;
}

// Definición de Clases

// Clase para representar un usuario
class UsuarioClase implements Usuario {
  id: number;
  nombre: string;
  apellido: string;
  correo: string;
  contraseña: string;

  constructor(id: number, nombre: string, apellido: string, correo: string, contraseña: string) {
    this.id = id;
    this.nombre = nombre;
    this.apellido = apellido;
    this.correo = correo;
    this.contraseña = contraseña;
  }
}

// Clase para representar un producto
class ProductoClase implements Producto {
  id: number;
  nombre: string;
  descripcion: string;
  precio: number;
  cantidadDisponible: number;

  constructor(id: number, nombre: string, descripcion: string, precio: number, cantidadDisponible: number) {
    this.id = id;
    this.nombre = nombre;
    this.descripcion = descripcion;
    this.precio = precio;
    this.cantidadDisponible = cantidadDisponible;
  }
}

// Clase para representar una cesta de compra
class CestaCompraClase implements CestaCompra {
  id: number;
  usuarioId: number;
  productos: Producto[];
  total: number;

  constructor(id: number, usuarioId: number, productos: Producto[], total: number) {
    this.id = id;
    this.usuarioId = usuarioId;
    this.productos = productos;
    this.total = total;
  }

  // Método para calcular el total de la cesta de compra
  calcularTotal(): number {
    let total = 0;
    for (let producto of this.productos) {
      total += producto.precio;
    }
    return total;
  }
}

// Ejemplo de Uso

// Crear un usuario
const usuario1 = new UsuarioClase(1, 'Juan', 'Pérez', 'juan.perez@correo.com', 'Contraseña123');

// Crear un producto
const producto1 = new ProductoClase(1, 'Camiseta Roja', 'Camiseta de algodón roja', 10.99, 20);

// Crear una cesta de compra
const cesta1 = new CestaCompraClase(1, usuario1.id, [producto1], 10.99);

// Añadir un producto a la cesta de la compra
cesta1.productos.push(producto1);

// Calcular el total de la cesta de la compra
const total = cesta1.calcularTotal();

// Mostrar el total de la cesta de la compra
console.log('Total de la cesta de la compra:', total);
```

Este código es un ejemplo de cómo se pueden utilizar las interfaces y las clases en TypeScript para crear objetos complejos y realizar operaciones con ellos. El código crea un usuario, un producto y una cesta de compra, y luego añade el producto a la cesta de la compra y calcula el total de la cesta.

El código está muy bien documentado y utiliza tipos explícitos para todas las variables y parámetros. También utiliza clases y objetos para representar los datos, lo que hace que el código sea más fácil de leer y mantener.

Este código es un ejemplo de cómo se puede utilizar TypeScript para crear código complejo y bien estructurado.