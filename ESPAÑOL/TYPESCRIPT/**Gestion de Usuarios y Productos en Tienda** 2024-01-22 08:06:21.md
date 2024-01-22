```typescript
// Definición de la interfaz de usuario
interface Usuario {
  nombre: string;
  correo: string;
  contraseña: string;
  rol: "administrador" | "usuario";
}

// Definición de la interfaz de producto
interface Producto {
  nombre: string;
  precio: number;
  stock: number;
  descripción: string;
  categoría: string;
}

// Definición de la clase Tienda
class Tienda {
  // Atributos de la tienda
  private usuarios: Usuario[] = [];
  private productos: Producto[] = [];

  // Constructor de la tienda
  constructor() {}

  // Métodos de la tienda

  // Función para agregar un usuario a la tienda
  agregarUsuario(usuario: Usuario): void {
    this.usuarios.push(usuario);
  }

  // Función para eliminar un usuario de la tienda
  eliminarUsuario(correo: string): void {
    const index = this.usuarios.findIndex((usuario) => usuario.correo === correo);
    if (index !== -1) {
      this.usuarios.splice(index, 1);
    }
  }

  // Función para obtener todos los usuarios de la tienda
  obtenerUsuarios(): Usuario[] {
    return this.usuarios;
  }

  // Función para agregar un producto a la tienda
  agregarProducto(producto: Producto): void {
    this.productos.push(producto);
  }

  // Función para eliminar un producto de la tienda
  eliminarProducto(nombre: string): void {
    const index = this.productos.findIndex((producto) => producto.nombre === nombre);
    if (index !== -1) {
      this.productos.splice(index, 1);
    }
  }

  // Función para obtener todos los productos de la tienda
  obtenerProductos(): Producto[] {
    return this.productos;
  }

  // Función para obtener un producto por su nombre
  obtenerProductoPorNombre(nombre: string): Producto | undefined {
    return this.productos.find((producto) => producto.nombre === nombre);
  }
}

// Crear una instancia de la tienda
const tienda = new Tienda();

// Agregar usuarios a la tienda
tienda.agregarUsuario({
  nombre: "Juan",
  correo: "juan@ejemplo.com",
  contraseña: "123456",
  rol: "administrador",
});
tienda.agregarUsuario({
  nombre: "María",
  correo: "maria@ejemplo.com",
  contraseña: "654321",
  rol: "usuario",
});

// Agregar productos a la tienda
tienda.agregarProducto({
  nombre: "Camisa",
  precio: 100,
  stock: 10,
  descripción: "Camisa de algodón de manga corta",
  categoría: "Ropa",
});
tienda.agregarProducto({
  nombre: "Pantalón",
  precio: 150,
  stock: 5,
  descripción: "Pantalón de mezclilla de corte recto",
  categoría: "Ropa",
});
tienda.agregarProducto({
  nombre: "Libro",
  precio: 200,
  stock: 3,
  descripción: "Libro de novela de aventuras",
  categoría: "Libros",
});

// Obtener todos los usuarios de la tienda
const usuarios = tienda.obtenerUsuarios();
console.log("Usuarios de la tienda:", usuarios);

// Obtener todos los productos de la tienda
const productos = tienda.obtenerProductos();
console.log("Productos de la tienda:", productos);

// Obtener un producto por su nombre
const camisa = tienda.obtenerProductoPorNombre("Camisa");
console.log("Camisa:", camisa);
```

Explicación del código:

* Se definen las interfaces `Usuario` y `Producto` para representar los datos de los usuarios y productos de la tienda.
* Se define la clase `Tienda` que gestiona los usuarios y productos de la tienda.
* Se crean instancias de las clases `Usuario` y `Producto` y se agregan a la tienda.
* Se obtienen todos los usuarios y productos de la tienda y se muestran en la consola.
* Se obtiene un producto por su nombre y se muestra en la consola.