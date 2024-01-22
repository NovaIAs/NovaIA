```typescript
// Interfaz para definir la estructura de un usuario
interface Usuario {
  nombre: string;
  correo: string;
  contraseña: string;
  edad: number;
  ciudad: string;
}

// Clase que representa un usuario
class UsuarioImpl implements Usuario {
  constructor(
    public nombre: string,
    public correo: string,
    public contraseña: string,
    public edad: number,
    public ciudad: string
  ) {}
}

// Interfaz para definir la estructura de un producto
interface Producto {
  nombre: string;
  descripcion: string;
  precio: number;
  cantidad: number;
}

// Clase que representa un producto
class ProductoImpl implements Producto {
  constructor(
    public nombre: string,
    public descripcion: string,
    public precio: number,
    public cantidad: number
  ) {}
}

// Interfaz para definir la estructura de una orden
interface Orden {
  usuario: Usuario;
  productos: Producto[];
  total: number;
  fecha: Date;
}

// Clase que representa una orden
class OrdenImpl implements Orden {
  constructor(
    public usuario: Usuario,
    public productos: Producto[],
    public total: number,
    public fecha: Date
  ) {}
}

// Función para crear un nuevo usuario
function crearUsuario(
  nombre: string,
  correo: string,
  contraseña: string,
  edad: number,
  ciudad: string
): Usuario {
  return new UsuarioImpl(nombre, correo, contraseña, edad, ciudad);
}

// Función para crear un nuevo producto
function crearProducto(
  nombre: string,
  descripcion: string,
  precio: number,
  cantidad: number
): Producto {
  return new ProductoImpl(nombre, descripcion, precio, cantidad);
}

// Función para crear una nueva orden
function crearOrden(
  usuario: Usuario,
  productos: Producto[],
  total: number,
  fecha: Date
): Orden {
  return new OrdenImpl(usuario, productos, total, fecha);
}

// Función para calcular el total de una orden
function calcularTotal(productos: Producto[]): number {
  let total = 0;
  for (const producto of productos) {
    total += producto.precio * producto.cantidad;
  }
  return total;
}

// Crear un nuevo usuario
const usuario1 = crearUsuario(
  "Juan",
  "juan@gmail.com",
  "123456",
  25,
  "Madrid"
);

// Crear un nuevo producto
const producto1 = crearProducto(
  "iPhone 13",
  "El último teléfono de Apple",
  1000,
  10
);

// Crear una nueva orden
const orden1 = crearOrden(usuario1, [producto1], 1000, new Date());

// Mostrar el total de la orden
console.log(`El total de la orden es: ${orden1.total}`);
```

Explicación:

Este código está escrito en TypeScript y define una serie de interfaces y clases para representar usuarios, productos y órdenes. También incluye funciones para crear nuevos usuarios, productos y órdenes, y para calcular el total de una orden.

La interfaz `Usuario` define la estructura de un usuario, con propiedades para su nombre, correo, contraseña, edad y ciudad. La clase `UsuarioImpl` implementa la interfaz `Usuario`.

La interfaz `Producto` define la estructura de un producto, con propiedades para su nombre, descripción, precio y cantidad. La clase `ProductoImpl` implementa la interfaz `Producto`.

La interfaz `Orden` define la estructura de una orden, con propiedades para el usuario que la realizó, los productos que contiene, el total de la orden y la fecha en que se realizó. La clase `OrdenImpl` implementa la interfaz `Orden`.

La función `crearUsuario` crea un nuevo usuario a partir de los parámetros que se le pasan. La función `crearProducto` crea un nuevo producto a partir de los parámetros que se le pasan. La función `crearOrden` crea una nueva orden a partir de los parámetros que se le pasan.

La función `calcularTotal` calcula el total de una orden a partir de los productos que contiene.

Finalmente, se crea un nuevo usuario, un nuevo producto y una nueva orden, y se muestra el total de la orden.