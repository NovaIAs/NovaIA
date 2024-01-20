```typescript
// Definición de una interfaz para representar un usuario.
interface Usuario {
  nombre: string;
  apellidos: string;
  correo: string;
  contraseña: string;
}

// Definición de una clase para representar un servicio de autenticación.
class ServicioAutenticacion {

  // Método para registrar un nuevo usuario.
  registrarUsuario(usuario: Usuario): Promise<void> {
    // Lógica para registrar el usuario en una base de datos o sistema de autenticación.
  }

  // Método para iniciar sesión de un usuario.
  iniciarSesion(correo: string, contraseña: string): Promise<Usuario> {
    // Lógica para verificar las credenciales del usuario y obtener su información.
  }

  // Método para cerrar la sesión de un usuario.
  cerrarSesion(): void {
    // Lógica para cerrar la sesión del usuario y eliminar la información de autenticación.
  }
}

// Definición de una clase para representar un servicio de gestión de usuarios.
class ServicioGestionUsuarios {

  // Método para obtener todos los usuarios.
  obtenerTodosLosUsuarios(): Promise<Usuario[]> {
    // Lógica para obtener todos los usuarios de una base de datos o sistema de gestión de usuarios.
  }

  // Método para obtener un usuario por su ID.
  obtenerUsuarioPorId(id: number): Promise<Usuario> {
    // Lógica para obtener un usuario específico de una base de datos o sistema de gestión de usuarios.
  }

  // Método para crear un nuevo usuario.
  crearUsuario(usuario: Usuario): Promise<Usuario> {
    // Lógica para crear un nuevo usuario en una base de datos o sistema de gestión de usuarios.
  }

  // Método para actualizar un usuario.
  actualizarUsuario(usuario: Usuario): Promise<Usuario> {
    // Lógica para actualizar la información de un usuario en una base de datos o sistema de gestión de usuarios.
  }

  // Método para eliminar un usuario.
  eliminarUsuario(id: number): Promise<void> {
    // Lógica para eliminar un usuario de una base de datos o sistema de gestión de usuarios.
  }
}

// Definición de una clase para representar un componente de Angular que gestiona el registro de usuarios.
@Component({
  selector: 'app-registro-usuario',
  templateUrl: './registro-usuario.component.html',
  styleUrls: ['./registro-usuario.component.css']
})
export class RegistroUsuarioComponent {

  // Definición de las propiedades del componente.
  nombre: string;
  apellidos: string;
  correo: string;
  contraseña: string;

  // Definición del servicio de autenticación.
  private servicioAutenticacion: ServicioAutenticacion;

  // Constructor del componente.
  constructor(servicioAutenticacion: ServicioAutenticacion) {
    this.servicioAutenticacion = servicioAutenticacion;
  }

  // Método para registrar un nuevo usuario.
  registrarUsuario(): void {
    // Crear un objeto de usuario.
    const usuario: Usuario = {
      nombre: this.nombre,
      apellidos: this.apellidos,
      correo: this.correo,
      contraseña: this.contraseña
    };

    // Llamar al método de registro de usuario del servicio de autenticación.
    this.servicioAutenticacion.registrarUsuario(usuario)
      .then(() => {
        // Mostrar un mensaje de éxito.
        alert('Usuario registrado con éxito.');
      })
      .catch((error) => {
        // Mostrar un mensaje de error.
        alert('Error al registrar el usuario.');
      });
  }
}

// Definición de una clase para representar un componente de Angular que gestiona el inicio de sesión de usuarios.
@Component({
  selector: 'app-inicio-sesion',
  templateUrl: './inicio-sesion.component.html',
  styleUrls: ['./inicio-sesion.component.css']
})
export class InicioSesionComponent {

  // Definición de las propiedades del componente.
  correo: string;
  contraseña: string;

  // Definición del servicio de autenticación.
  private servicioAutenticacion: ServicioAutenticacion;

  // Constructor del componente.
  constructor(servicioAutenticacion: ServicioAutenticacion) {
    this.servicioAutenticacion = servicioAutenticacion;
  }

  // Método para iniciar sesión de un usuario.
  iniciarSesion(): void {
    // Llamar al método de inicio de sesión del servicio de autenticación.
    this.servicioAutenticacion.iniciarSesion(this.correo, this.contraseña)
      .then((usuario) => {
        // Almacenar la información del usuario en el almacenamiento local.
        localStorage.setItem('usuario', JSON.stringify(usuario));

        // Redirigir a la página principal.
        window.location.href = '/';
      })
      .catch((error) => {
        // Mostrar un mensaje de error.
        alert('Error al iniciar sesión.');
      });
  }
}
```