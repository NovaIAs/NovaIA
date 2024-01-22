```typescript
// Definición de interfaz para un usuario
interface Usuario {
  nombre: string;
  apellido: string;
  edad: number;
  correo: string;
  contrasena: string;
}

// Clase para representar un usuario
class UsuarioClase implements Usuario {
  nombre: string;
  apellido: string;
  edad: number;
  correo: string;
  contrasena: string;

  constructor(nombre: string, apellido: string, edad: number, correo: string, contrasena: string) {
    this.nombre = nombre;
    this.apellido = apellido;
    this.edad = edad;
    this.correo = correo;
    this.contrasena = contrasena;
  }

  // Método para obtener el nombre completo del usuario
  getNombreCompleto(): string {
    return `${this.nombre} ${this.apellido}`;
  }

  // Método para verificar si el usuario es mayor de edad
  esMayorDeEdad(): boolean {
    return this.edad >= 18;
  }
}

// Función para crear un nuevo usuario
function crearUsuario(nombre: string, apellido: string, edad: number, correo: string, contrasena: string): Usuario {
  return new UsuarioClase(nombre, apellido, edad, correo, contrasena);
}

// Función para obtener una lista de usuarios
function obtenerUsuarios(): Usuario[] {
  return [
    crearUsuario("Juan", "Pérez", 25, "juan.perez@ejemplo.com", "123456"),
    crearUsuario("María", "García", 30, "maria.garcia@ejemplo.com", "654321"),
    crearUsuario("Pedro", "López", 35, "pedro.lopez@ejemplo.com", "987654")
  ];
}

// Función para encontrar un usuario por su correo y contraseña
function encontrarUsuario(correo: string, contrasena: string): Usuario | undefined {
  const usuarios = obtenerUsuarios();
  for (const usuario of usuarios) {
    if (usuario.correo === correo && usuario.contrasena === contrasena) {
      return usuario;
    }
  }

  return undefined;
}

// Función para mostrar la información de un usuario
function mostrarUsuario(usuario: Usuario): void {
  console.log(`Nombre: ${usuario.getNombreCompleto()}`);
  console.log(`Edad: ${usuario.edad}`);
  console.log(`Correo: ${usuario.correo}`);
}

// Función principal
function main(): void {
  // Obtener el usuario actual
  const usuarioActual = encontrarUsuario("juan.perez@ejemplo.com", "123456");

  // Verificar si el usuario existe
  if (usuarioActual) {
    // Mostrar la información del usuario
    mostrarUsuario(usuarioActual);
  } else {
    // Mostrar un mensaje de error
    console.log("Usuario no encontrado");
  }
}

// Llamada a la función principal
main();
```

Explicación del código:

* Se define una interfaz `Usuario` que representa las propiedades comunes de un usuario.
* Se define una clase `UsuarioClase` que implementa la interfaz `Usuario`.
* Se define una función `crearUsuario` que crea un nuevo objeto de la clase `UsuarioClase`.
* Se define una función `obtenerUsuarios` que devuelve una lista de usuarios.
* Se define una función `encontrarUsuario` que busca un usuario por su correo y contraseña.
* Se define una función `mostrarUsuario` que muestra la información de un usuario.
* Se define una función `main` que llama a las funciones anteriores para encontrar y mostrar la información del usuario actual.