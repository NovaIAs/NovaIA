```typescript
// Definimos una interfaz para representar a un usuario.
interface Usuario {
  id: number;
  nombre: string;
  apellido: string;
  email: string;
  contraseña: string;
}

// Definimos una función para crear un nuevo usuario.
const crearUsuario = (usuario: Usuario): void => {
  // Aquí se implementaría la lógica para crear un nuevo usuario en una base de datos o sistema de gestión de usuarios.
  console.log(`Se ha creado el usuario ${usuario.nombre} ${usuario.apellido} con el email ${usuario.email}.`);
};

// Definimos una función para obtener todos los usuarios.
const obtenerUsuarios = (): Usuario[] => {
  // Aquí se implementaría la lógica para obtener todos los usuarios de una base de datos o sistema de gestión de usuarios.
  return [
    {
      id: 1,
      nombre: 'Juan',
      apellido: 'Pérez',
      email: 'juanperez@correo.com',
      contraseña: 'clave123'
    },
    {
      id: 2,
      nombre: 'María',
      apellido: 'González',
      email: 'mariagonzalez@correo.com',
      contraseña: 'clave456'
    },
    {
      id: 3,
      nombre: 'Pedro',
      apellido: 'García',
      email: 'pedrog@correo.com',
      contraseña: 'clave789'
    }
  ];
};

// Definimos una función para obtener un usuario por su id.
const obtenerUsuarioPorId = (id: number): Usuario | undefined => {
  // Aquí se implementaría la lógica para obtener un usuario por su id de una base de datos o sistema de gestión de usuarios.
  const usuarios = obtenerUsuarios();
  const usuario = usuarios.find((usuario) => usuario.id === id);
  return usuario;
};

// Definimos una función para actualizar un usuario.
const actualizarUsuario = (usuario: Usuario): void => {
  // Aquí se implementaría la lógica para actualizar un usuario en una base de datos o sistema de gestión de usuarios.
  console.log(`Se ha actualizado el usuario ${usuario.nombre} ${usuario.apellido} con el email ${usuario.email}.`);
};

// Definimos una función para eliminar un usuario.
const eliminarUsuario = (id: number): void => {
  // Aquí se implementaría la lógica para eliminar un usuario de una base de datos o sistema de gestión de usuarios.
  console.log(`Se ha eliminado el usuario con id ${id}.`);
};

// Definimos una función para iniciar sesión.
const iniciarSesion = (email: string, contraseña: string): boolean => {
  // Aquí se implementaría la lógica para iniciar sesión en una base de datos o sistema de gestión de usuarios.
  const usuarios = obtenerUsuarios();
  const usuario = usuarios.find((usuario) => usuario.email === email && usuario.contraseña === contraseña);
  return usuario !== undefined;
};

// Definimos una función para cerrar sesión.
const cerrarSesion = (): void => {
  // Aquí se implementaría la lógica para cerrar sesión en una base de datos o sistema de gestión de usuarios.
  console.log('Se ha cerrado la sesión.');
};

// Ejemplo de uso de las funciones definidas anteriormente.

// Creamos un nuevo usuario.
const nuevoUsuario: Usuario = {
  id: 4,
  nombre: 'Ana',
  apellido: 'Fernández',
  email: 'anafernandez@correo.com',
  contraseña: 'clave012'
};
crearUsuario(nuevoUsuario);

// Obtenemos todos los usuarios.
const usuarios = obtenerUsuarios();
console.log('Lista de todos los usuarios:');
usuarios.forEach((usuario) => {
  console.log(`${usuario.id} - ${usuario.nombre} ${usuario.apellido}`);
});

// Obtenemos un usuario por su id.
const usuarioPorId = obtenerUsuarioPorId(2);
if (usuarioPorId) {
  console.log(`Usuario con id 2: ${usuarioPorId.nombre} ${usuarioPorId.apellido}`);
} else {
  console.log('No se ha encontrado ningún usuario con id 2.');
}

// Actualizamos un usuario.
const usuarioActualizado: Usuario = {
  id: 3,
  nombre: 'Pedro',
  apellido: 'García',
  email: 'pedrog@correo.com',
  contraseña: 'clave987'
};
actualizarUsuario(usuarioActualizado);

// Eliminamos un usuario.
eliminarUsuario(1);

// Iniciamos sesión.
const resultadoInicioSesion = iniciarSesion('mariagonzalez@correo.com', 'clave456');
if (resultadoInicioSesion) {
  console.log('Se ha iniciado sesión correctamente.');
} else {
  console.log('No se ha podido iniciar sesión.');
}

// Cerramos sesión.
cerrarSesion();
```

Este código define una interfaz para representar a un usuario, y luego define varias funciones para crear, obtener, actualizar y eliminar usuarios. También define funciones para iniciar y cerrar sesión.

El código incluye un ejemplo de uso de las funciones definidas, donde se crea un nuevo usuario, se obtienen todos los usuarios, se obtiene un usuario por su id, se actualiza un usuario, se elimina un usuario, se inicia sesión y se cierra sesión.