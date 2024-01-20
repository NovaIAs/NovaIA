```typescript
// Definir una interfaz para representar un usuario.
interface Usuario {
  id: number;
  nombre: string;
  apellido: string;
  correo_electronico: string;
  contrasena: string;
}

// Crear una clase para representar un servicio de autenticación.
class ServicioAutenticacion {

  // Definir un método para registrar un nuevo usuario.
  registrarUsuario(usuario: Usuario): Promise<Usuario> {
    // Código para registrar el usuario en la base de datos.
    return Promise.resolve(usuario);
  }

  // Definir un método para iniciar sesión de un usuario.
  iniciarSesion(correo_electronico: string, contrasena: string): Promise<Usuario> {
    // Código para iniciar sesión del usuario en la base de datos.
    return Promise.resolve(usuario);
  }

  // Definir un método para cerrar sesión de un usuario.
  cerrarSesion(): Promise<void> {
    // Código para cerrar sesión del usuario.
    return Promise.resolve();
  }
}

// Definir una interfaz para representar una película.
interface Pelicula {
  id: number;
  titulo: string;
  director: string;
  actores: string[];
  genero: string;
  duracion: number;
}

// Crear una clase para representar un servicio de películas.
class ServicioPeliculas {

  // Definir un método para obtener todas las películas.
  obtenerTodasLasPeliculas(): Promise<Pelicula[]> {
    // Código para obtener todas las películas de la base de datos.
    return Promise.resolve([]);
  }

  // Definir un método para obtener una película por su ID.
  obtenerPeliculaPorId(id: number): Promise<Pelicula> {
    // Código para obtener la película con el ID especificado de la base de datos.
    return Promise.resolve(pelicula);
  }

  // Definir un método para crear una nueva película.
  crearPelicula(pelicula: Pelicula): Promise<Pelicula> {
    // Código para crear una nueva película en la base de datos.
    return Promise.resolve(pelicula);
  }

  // Definir un método para actualizar una película.
  actualizarPelicula(pelicula: Pelicula): Promise<Pelicula> {
    // Código para actualizar la película especificada en la base de datos.
    return Promise.resolve(pelicula);
  }

  // Definir un método para eliminar una película.
  eliminarPelicula(id: number): Promise<void> {
    // Código para eliminar la película con el ID especificado de la base de datos.
    return Promise.resolve();
  }
}

// Crear una clase principal para el programa.
class ProgramaPrincipal {

  // Definir un método para inicializar el programa.
  inicializar(): void {
    // Crear una instancia del servicio de autenticación.
    const servicioAutenticacion = new ServicioAutenticacion();

    // Crear una instancia del servicio de películas.
    const servicioPeliculas = new ServicioPeliculas();

    // Registrar un nuevo usuario.
    servicioAutenticacion.registrarUsuario({
      id: 1,
      nombre: "Juan",
      apellido: "Pérez",
      correo_electronico: "juan.perez@ejemplo.com",
      contrasena: "123456"
    }).then((usuario) => {
      console.log(`Usuario registrado: ${usuario.nombre} ${usuario.apellido}`);
    });

    // Iniciar sesión de un usuario.
    servicioAutenticacion.iniciarSesion("juan.perez@ejemplo.com", "123456").then((usuario) => {
      console.log(`Usuario iniciado sesión: ${usuario.nombre} ${usuario.apellido}`);
    });

    // Obtener todas las películas.
    servicioPeliculas.obtenerTodasLasPeliculas().then((peliculas) => {
      console.log("Todas las películas:");
      peliculas.forEach((pelicula) => {
        console.log(`  - ${pelicula.titulo}`);
      });
    });

    // Obtener una película por su ID.
    servicioPeliculas.obtenerPeliculaPorId(1).then((pelicula) => {
      console.log(`Película obtenida por su ID: ${pelicula.titulo}`);
    });

    // Crear una nueva película.
    servicioPeliculas.crearPelicula({
      id: 2,
      titulo: "El Señor de los Anillos: El Retorno del Rey",
      director: "Peter Jackson",
      actores: ["Elijah Wood", "Viggo Mortensen", "Ian McKellen"],
      genero: "Fantasía",
      duracion: 201
    }).then((pelicula) => {
      console.log(`Película creada: ${pelicula.titulo}`);
    });

    // Actualizar una película.
    servicioPeliculas.actualizarPelicula({
      id: 2,
      titulo: "El Señor de los Anillos: El Retorno del Rey (Edición Extendida)",
      duracion: 228
    }).then((pelicula) => {
      console.log(`Película actualizada: ${pelicula.titulo}`);
    });

    // Eliminar una película.
    servicioPeliculas.eliminarPelicula(2).then(() => {
      console.log("Película eliminada");
    });

    // Cerrar sesión de un usuario.
    servicioAutenticacion.cerrarSesion();
  }
}

// Crear una instancia de la clase principal y ejecutar el método de inicialización.
const programaPrincipal = new ProgramaPrincipal();
programaPrincipal.inicializar();
```

Explicación:

* El código tiene dos interfaces, `Usuario` y `Pelicula`, que definen los tipos para objetos de usuario y película, respectivamente.
* Hay dos clases de servicio, `ServicioAutenticacion` y `ServicioPeliculas`, que manejan la lógica relacionada con la autenticación de usuarios y la gestión de películas, respectivamente.
* La clase `ProgramaPrincipal` es la clase principal del programa y contiene el método `inicializar()` que es el punto de entrada del programa.
* En el método `inicializar()`, se crea una instancia de cada uno de los servicios y se realizan varias operaciones, como registrar un usuario, iniciar sesión de un usuario, obtener todas las películas, obtener una película por su ID, crear una nueva película, actualizar una película, eliminar una película y cerrar sesión de un usuario.
* El código utiliza promesas para manejar las operaciones asincrónicas.