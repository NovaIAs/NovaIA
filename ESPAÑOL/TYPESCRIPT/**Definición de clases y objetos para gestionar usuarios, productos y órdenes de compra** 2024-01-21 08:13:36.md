```typescript
// Definición de la interfaz de un usuario
interface Usuario {
  nombre: string;
  apellido: string;
  email: string;
  edad: number;
  direccion: string;
}

// Definición de la clase Usuario
class Usuario {
  // Constructor de la clase Usuario
  constructor(nombre: string, apellido: string, email: string, edad: number, direccion: string) {
    this.nombre = nombre;
    this.apellido = apellido;
    this.email = email;
    this.edad = edad;
    this.direccion = direccion;
  }

  // Método para obtener el nombre completo del usuario
  getNombreCompleto(): string {
    return `${this.nombre} ${this.apellido}`;
  }

  // Método para obtener la edad del usuario
  getEdad(): number {
    return this.edad;
  }

  // Método para obtener la dirección del usuario
  getDireccion(): string {
    return this.direccion;
  }

  // Método para establecer la dirección del usuario
  setDireccion(direccion: string) {
    this.direccion = direccion;
  }
}

// Definición de la interfaz de un producto
interface Producto {
  nombre: string;
  descripcion: string;
  precio: number;
  stock: number;
}

// Definición de la clase Producto
class Producto {
  // Constructor de la clase Producto
  constructor(nombre: string, descripcion: string, precio: number, stock: number) {
    this.nombre = nombre;
    this.descripcion = descripcion;
    this.precio = precio;
    this.stock = stock;
  }

  // Método para obtener el nombre del producto
  getNombre(): string {
    return this.nombre;
  }

  // Método para obtener la descripción del producto
  getDescripcion(): string {
    return this.descripcion;
  }

  // Método para obtener el precio del producto
  getPrecio(): number {
    return this.precio;
  }

  // Método para obtener el stock del producto
  getStock(): number {
    return this.stock;
  }

  // Método para establecer el stock del producto
  setStock(stock: number) {
    this.stock = stock;
  }
}

// Definición de la interfaz de una orden de compra
interface OrdenDeCompra {
  usuario: Usuario;
  productos: Producto[];
  total: number;
  fecha: Date;
}

// Definición de la clase OrdenDeCompra
class OrdenDeCompra {
  // Constructor de la clase OrdenDeCompra
  constructor(usuario: Usuario, productos: Producto[], total: number, fecha: Date) {
    this.usuario = usuario;
    this.productos = productos;
    this.total = total;
    this.fecha = fecha;
  }

  // Método para obtener el usuario de la orden de compra
  getUsuario(): Usuario {
    return this.usuario;
  }

  // Método para obtener los productos de la orden de compra
  getProductos(): Producto[] {
    return this.productos;
  }

  // Método para obtener el total de la orden de compra
  getTotal(): number {
    return this.total;
  }

  // Método para obtener la fecha de la orden de compra
  getFecha(): Date {
    return this.fecha;
  }

  // Método para agregar un producto a la orden de compra
  agregarProducto(producto: Producto) {
    this.productos.push(producto);
    this.calcularTotal();
  }

  // Método para eliminar un producto de la orden de compra
  eliminarProducto(producto: Producto) {
    const index = this.productos.indexOf(producto);
    if (index !== -1) {
      this.productos.splice(index, 1);
      this.calcularTotal();
    }
  }

  // Método para calcular el total de la orden de compra
  calcularTotal() {
    this.total = 0;
    this.productos.forEach((producto) => {
      this.total += producto.getPrecio();
    });