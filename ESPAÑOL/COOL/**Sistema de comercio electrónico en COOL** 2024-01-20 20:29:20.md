```cool

class Producto {
    nombre : String;
    precio : Integer;
    iva: Number;

    constructor(nombre: String, precio: Integer) {
        this.nombre = nombre;
        this.precio = precio;
        this.iva = 0.21;
    }

    getPrecioConIVA(): Integer {
        return this.precio + this.precio * this.iva;
    }

    toString(): String {
        return "Nombre: " + this.nombre + ", Precio: " + this.precio + ", IVA: " + this.iva;
    }
}

class Carrito {
    productos: Producto[];

    constructor() {
        this.productos = [];
    }

    agregarProducto(producto: Producto) {
        this.productos.push(producto);
    }

    getPrecioTotal(): Integer {
        let total = 0;
        for (let producto of this.productos) {
            total += producto.getPrecioConIVA();
        }
        return total;
    }

    toString(): String {
        let cadena = "";
        for (let producto of this.productos) {
            cadena += producto.toString() + "\n";
        }
        return cadena;
    }
}

class Usuario {
    nombre: String;
    correo: String;
    contrasena: String;
    carrito: Carrito;

    constructor(nombre: String, correo: String, contrasena: String) {
        this.nombre = nombre;
        this.correo = correo;
        this.contrasena = contrasena;
        this.carrito = new Carrito();
    }

    agregarProductoAlCarrito(producto: Producto) {
        this.carrito.agregarProducto(producto);
    }

    getPrecioTotalCarrito(): Integer {
        return this.carrito.getPrecioTotal();
    }

    toString(): String {
        return "Nombre: " + this.nombre + ", Correo: " + this.correo + ", Contraseña: " + this.contrasena + "\n" + this.carrito.toString();
    }
}

class Sistema {
    usuarios: Usuario[];

    constructor() {
        this.usuarios = [];
    }

    registrarUsuario(usuario: Usuario) {
        this.usuarios.push(usuario);
    }

    iniciarSesion(correo: String, contrasena: String): Usuario | null {
        for (let usuario of this.usuarios) {
            if (usuario.correo == correo && usuario.contrasena == contrasena) {
                return usuario;
            }
        }
        return null;
    }

    toString(): String {
        let cadena = "";
        for (let usuario of this.usuarios) {
            cadena += usuario.toString() + "\n";
        }
        return cadena;
    }
}

// Creamos el sistema
let sistema = new Sistema();

// Creamos algunos productos
let producto1 = new Producto("Manzanas", 100);
let producto2 = new Producto("Peras", 150);
let producto3 = new Producto("Uvas", 200);

// Creamos un usuario
let usuario1 = new Usuario("Juan", "juan@ejemplo.com", "123456");

// Agregamos los productos al carrito del usuario
usuario1.agregarProductoAlCarrito(producto1);
usuario1.agregarProductoAlCarrito(producto2);
usuario1.agregarProductoAlCarrito(producto3);

// Registramos el usuario en el sistema
sistema.registrarUsuario(usuario1);

// Iniciamos sesión en el sistema
let usuarioAutenticado = sistema.iniciarSesion("juan@ejemplo.com", "123456");

// Mostramos la información del usuario autenticado
console.log(usuarioAutenticado);

```

Este código implementa un sistema de comercio electrónico simple en COOL. El sistema consta de las siguientes clases:

* **Producto**: Representa un producto con un nombre y un precio.
* **Carrito**: Representa un carrito de compras que contiene una lista de productos.
* **Usuario**: Representa un usuario del sistema con un nombre, un correo, una contraseña y un carrito de compras.
* **Sistema**: Representa el sistema de comercio electrónico en sí mismo y contiene una lista de usuarios registrados.

El programa principal de COOL crea un sistema y agrega algunos productos. Luego crea un usuario, agrega productos al carrito de compras del usuario, registra el usuario en el sistema e inicia sesión en el sistema. Finalmente, muestra la información del usuario autentificado.

El código utiliza los siguientes conceptos de COOL:

* **Clases**: Las clases son usadas para crear tipos de datos definidos por el usuario.
* **Constructores**: Los constructores son métodos especiales que se utilizan para crear objetos de una clase.
* **Métodos**: Los métodos son funciones que pueden ser llamadas en objetos de una clase.
* **Atributos**: Los atributos son variables que pertenecen a objetos de una clase.
* **Her herencia**: La herencia se utiliza para crear nuevas clases a partir de clases existentes.
* **Polimorfismo**: El polimorfismo es la capacidad de objetos de diferentes