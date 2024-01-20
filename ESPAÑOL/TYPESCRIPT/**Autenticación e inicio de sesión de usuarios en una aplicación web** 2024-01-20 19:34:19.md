```typescript
// Definimos una interfaz para representar un usuario.
interface Usuario {
  nombre: string;
  apellido: string;
  correo: string;
  contraseña: string;
}

// Definimos una clase para representar un servicio de autenticación.
class ServicioAutenticacion {
  // Método para registrar un nuevo usuario.
  registrarUsuario(usuario: Usuario): void {
    // Aquí se guardaría el usuario en una base de datos o en un servicio remoto.
    console.log(`Usuario registrado: ${usuario.nombre} ${usuario.apellido}`);
  }

  // Método para iniciar sesión de un usuario.
  iniciarSesion(correo: string, contraseña: string): boolean {
    // Aquí se verificaría si el correo y la contraseña coinciden con los de un usuario existente.
    if (correo === "admin@ejemplo.com" && contraseña === "123456") {
      console.log("Sesión iniciada correctamente");
      return true;
    } else {
      console.log("Correo o contraseña incorrectos");
      return false;
    }
  }
}

// Definimos una clase para representar una aplicación que utiliza el servicio de autenticación.
class Aplicacion {
  private servicioAutenticacion: ServicioAutenticacion;

  constructor() {
    this.servicioAutenticacion = new ServicioAutenticacion();
  }

  // Método para mostrar el formulario de registro de usuario.
  mostrarFormularioRegistro(): void {
    // Aquí se mostraría un formulario HTML para registrar un nuevo usuario.
    console.log("Formulario de registro mostrado");
  }

  // Método para mostrar el formulario de inicio de sesión de usuario.
  mostrarFormularioInicioSesion(): void {
    // Aquí se mostraría un formulario HTML para iniciar sesión de un usuario.
    console.log("Formulario de inicio de sesión mostrado");
  }

  // Método para manejar el evento de envío del formulario de registro de usuario.
  manejarEnvioFormularioRegistro(event: Event): void {
    event.preventDefault();

    // Obtenemos los datos del formulario.
    const nombre = (document.getElementById("nombre") as HTMLInputElement).value;
    const apellido = (document.getElementById("apellido") as HTMLInputElement).value;
    const correo = (document.getElementById("correo") as HTMLInputElement).value;
    const contraseña = (document.getElementById("contraseña") as HTMLInputElement).value;

    // Creamos un objeto de usuario con los datos obtenidos.
    const usuario: Usuario = {
      nombre: nombre,
      apellido: apellido,
      correo: correo,
      contraseña: contraseña,
    };

    // Registramos el usuario utilizando el servicio de autenticación.
    this.servicioAutenticacion.registrarUsuario(usuario);
  }

  // Método para manejar el evento de envío del formulario de inicio de sesión de usuario.
  manejarEnvioFormularioInicioSesion(event: Event): void {
    event.preventDefault();

    // Obtenemos los datos del formulario.
    const correo = (document.getElementById("correo") as HTMLInputElement).value;
    const contraseña = (document.getElementById("contraseña") as HTMLInputElement).value;

    // Iniciamos sesión del usuario utilizando el servicio de autenticación.
    const autenticado = this.servicioAutenticacion.iniciarSesion(correo, contraseña);

    // Si el usuario fue autenticado correctamente, mostramos el mensaje de bienvenida.
    if (autenticado) {
      console.log("Bienvenido a la aplicación");
    }
  }
}

// Creamos una instancia de la aplicación.
const aplicacion = new Aplicacion();

// Mostramos el formulario de registro de usuario.
aplicacion.mostrarFormularioRegistro();

// Mostramos el formulario de inicio de sesión de usuario.
aplicacion.mostrarFormularioInicioSesion();

// Manejamos el evento de envío del formulario de registro de usuario.
document
  .getElementById("formulario-registro")
  ?.addEventListener("submit", aplicacion.manejarEnvioFormularioRegistro.bind(aplicacion));

// Manejamos el evento de envío del formulario de inicio de sesión de usuario.
document
  .getElementById("formulario-inicio-sesion")
  ?.addEventListener("submit", aplicacion.manejarEnvioFormularioInicioSesion.bind(aplicacion));
```

Explicación del código:

* Definimos una interfaz para representar un usuario, con las propiedades `nombre`, `apellido`, `correo` y `contraseña`.
* Definimos una clase para representar un servicio de autenticación, con los métodos `registrarUsuario` e `iniciarSesion`.
* Definimos una clase para representar una aplicación que utiliza el servicio de autenticación, con los métodos `mostrarFormularioRegistro`, `mostrarFormularioInicioSesion`, `manejarEnvioFormularioRegistro` y `manejarEnvioFormularioInicioSesion`.
* Creamos una instancia de la aplicación y mostramos los formularios de registro de usuario y de inicio de sesión de usuario.
* Manejamos los eventos de envío de los formularios de registro de usuario y de inicio de sesión de usuario, llamando a los métodos correspondientes de la aplicación.

Este código es complejo porque utiliza varias clases, interfaces y métodos, y maneja eventos de envío de formularios. Sin embargo, es un ejemplo típico de cómo se puede implementar un sistema de autenticación de usuarios en una aplicación web.