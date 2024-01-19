```typescript
// Crear una interfaz para definir un usuario
interface Usuario {
  nombre: string;
  correo: string;
  contraseña: string;
  rol: string;
}

// Definir una función para crear un nuevo usuario
function crearUsuario(usuario: Usuario): void {
  // Implementar la lógica para crear un nuevo usuario
}

// Definir una función para obtener todos los usuarios
function obtenerUsuarios(): Array<Usuario> {
  // Implementar la lógica para obtener todos los usuarios
}

// Definir una función para obtener un usuario por su ID
function obtenerUsuario(id: string): Usuario {
  // Implementar la lógica para obtener un usuario por su ID
}

// Definir una función para actualizar un usuario
function actualizarUsuario(usuario: Usuario): void {
  // Implementar la lógica para actualizar un usuario
}

// Definir una función para eliminar un usuario
function eliminarUsuario(id: string): void {
  // Implementar la lógica para eliminar un usuario
}

// Crear un nuevo usuario
const usuario1: Usuario = {
  nombre: "Juan",
  correo: "juan@gmail.com",
  contraseña: "12345",
  rol: "admin"
};

crearUsuario(usuario1);

// Obtener todos los usuarios
const usuarios = obtenerUsuarios();

// Obtener un usuario por su ID
const usuario2 = obtenerUsuario('12345');

// Actualizar un usuario
usuario2.nombre = "Pedro";
actualizarUsuario(usuario2);

// Eliminar un usuario
eliminarUsuario('12345');
```

Este código define una interfaz para representar a un usuario, con propiedades como nombre, correo, contraseña y rol. También se definen las funciones para crear, obtener, actualizar y eliminar usuarios.

Para utilizar el código, primero se debe crear un objeto de usuario con los datos del usuario que se desea crear. A continuación, se puede llamar a la función `crearUsuario()` para crear el nuevo usuario.

Para obtener todos los usuarios, se puede llamar a la función `obtenerUsuarios()`. Esta función devolverá un array con todos los usuarios almacenados en el sistema.

Para obtener un usuario por su ID, se puede llamar a la función `obtenerUsuario()` pasando el ID del usuario como argumento. Esta función devolverá el objeto de usuario correspondiente a ese ID.

Para actualizar un usuario, se debe modificar los datos del objeto de usuario y luego llamar a la función `actualizarUsuario()`. Esta función actualizará los datos del usuario en el sistema.

Para eliminar un usuario, se puede llamar a la función `eliminarUsuario()` pasando el ID del usuario como argumento. Esta función eliminará el usuario del sistema.

Este código es un ejemplo de cómo se pueden definir interfaces y funciones en TypeScript para modelar datos y operaciones sobre esos datos.