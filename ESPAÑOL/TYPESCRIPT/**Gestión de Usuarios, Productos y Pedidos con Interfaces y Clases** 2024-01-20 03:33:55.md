```typescript
// Definición de interfaces

interface IUsuario {
  id: number;
  nombre: string;
  apellido: string;
  correo: string;
  contraseña: string;
}

interface IProducto {
  id: number;
  nombre: string;
  precio: number;
  stock: number;
}

interface IPedido {
  id: number;
  usuarioId: number;
  productos: IProducto[];
  total: number;
  estado: string;
}

// Definición de clases

class Usuario implements IUsuario {
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

class Producto implements IProducto {
  id: number;
  nombre: string;
  precio: number;
  stock: number;

  constructor(id: number, nombre: string, precio: number, stock: number) {
    this.id = id;
    this.nombre = nombre;
    this.precio = precio;
    this.stock = stock;
  }
}

class Pedido implements IPedido {
  id: number;
  usuarioId: number;
  productos: IProducto[];
  total: number;
  estado: string;

  constructor(id: number, usuarioId: number, productos: IProducto[], total: number, estado: string) {
    this.id = id;
    this.usuarioId = usuarioId;
    this.productos = productos;
    this.total = total;
    this.estado = estado;
  }
}

// Funciones auxiliares

function buscarUsuarioPorCorreo(correo: string): IUsuario | null {
  // Buscar en la base de datos el usuario con el correo dado
  // y devolverlo si se encuentra, o null si no se encuentra

  // En este ejemplo, simulamos la búsqueda con un array predefinido de usuarios
  const usuarios = [
    new Usuario(1, "Juan", "García", "juan.garcia@ejemplo.com", "123456"),
    new Usuario(2, "María", "Pérez", "maria.perez@ejemplo.com", "654321")
  ];

  const usuarioEncontrado = usuarios.find((usuario) => usuario.correo === correo);

  return usuarioEncontrado || null;
}

function calcularTotalPedido(productos: IProducto[]): number {
  // Calcular el total del pedido sumando los precios de cada producto
  // y devolver el total

  // En este ejemplo, simulamos el cálculo del total con un array predefinido de productos
  const total = productos.reduce((total, producto) => total + producto.precio, 0);

  return total;
}

function crearPedido(usuarioId: number, productos: IProducto[]): IPedido {
  // Crear un nuevo pedido con el usuarioId y los productos dados, 
  // calcular el total y devolver el pedido

  // En este ejemplo, simulamos la creación del pedido con un id predefinido
  const id = 1;
  const total = calcularTotalPedido(productos);
  const estado = "Pendiente";

  const pedido = new Pedido(id, usuarioId, productos, total, estado);

  return pedido;
}

// Uso de las clases y funciones

const usuario = buscarUsuarioPorCorreo("juan.garcia@ejemplo.com");

if (usuario) {
  // Crear un array de productos para el pedido
  const productos = [
    new Producto(1, "Camisa", 1000, 5),
    new Producto(2, "Pantalón", 1500, 3)
  ];

  // Crear un pedido con el usuario y los productos
  const pedido = crearPedido(usuario.id, productos);

  // Mostrar el pedido
  console.log(`Pedido ${pedido.id}:`);
  console.log(`Usuario: ${usuario.nombre} ${usuario.apellido}`);
  console.log("Productos:");
  pedido.productos.forEach((producto) => {
    console.log(`  - ${producto.nombre} (${producto.precio}) x ${producto.stock}`);
  });
  console.log(`Total: ${pedido.total}`);
  console.log(`Estado: ${pedido.estado}`);
} else {
  console.log("Usuario no encontrado");
}
```

Explicación del código:

* Se definen las interfaces `IUsuario`, `IProducto` e `IPedido` para representar los usuarios, los productos y los pedidos del sistema.
* Se definen las clases `Usuario`, `Producto` y `Pedido` que implementan las interfaces correspondientes.
* Se definen las funciones `buscarUsuarioPorCorreo()`, `calcularTotalPedido()` y `crearPedido()` para realizar operaciones auxiliares.
* En la parte final del código se crea un usuario de ejemplo, un array de productos de ejemplo y se crea un pedido con el usuario y los productos utilizando las funciones auxiliares.
* Por último, se muestra la información del pedido en la consola.