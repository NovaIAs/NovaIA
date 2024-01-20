```typescript
// **Archivos adjuntos**

// Definición de una interfaz para adjuntar archivos
interface ArchivoAdjunto {
  nombre: string;
  tipo: string;
  tamaño: number;
  contenido: string;
}

// Función para crear un nuevo archivo adjunto
function crearArchivoAdjunto(nombre: string, tipo: string, tamaño: number, contenido: string): ArchivoAdjunto {
  return {
    nombre,
    tipo,
    tamaño,
    contenido,
  };
}

// **Correos electrónicos**

// Definición de una interfaz para representar correos electrónicos
interface CorreoElectronico {
  asunto: string;
  cuerpo: string;
  archivosAdjuntos: ArchivoAdjunto[];
}

// Función para crear un nuevo correo electrónico
function crearCorreoElectronico(asunto: string, cuerpo: string, archivosAdjuntos: ArchivoAdjunto[]): CorreoElectronico {
  return {
    asunto,
    cuerpo,
    archivosAdjuntos,
  };
}

// **Usuarios**

// Definición de una interfaz para representar usuarios
interface Usuario {
  nombre: string;
  correoElectronico: string;
  contraseña: string;
}

// Función para crear un nuevo usuario
function crearUsuario(nombre: string, correoElectronico: string, contraseña: string): Usuario {
  return {
    nombre,
    correoElectronico,
    contraseña,
  };
}

// **Sistema de gestión de correo electrónico**

// Definición de una clase para representar el sistema de gestión de correo electrónico
class SistemaGestionCorreoElectronico {
  // Atributos
  usuarios: Usuario[] = [];
  correosElectronicos: CorreoElectronico[] = [];

  // Métodos

  // Método para registrar un nuevo usuario
  registrarUsuario(usuario: Usuario) {
    this.usuarios.push(usuario);
  }

  // Método para enviar un correo electrónico
  enviarCorreoElectronico(correoElectronico: CorreoElectronico) {
    this.correosElectronicos.push(correoElectronico);
  }

  // Método para obtener todos los correos electrónicos de un usuario
  obtenerCorreosElectronicosUsuario(usuario: Usuario): CorreoElectronico[] {
    return this.correosElectronicos.filter((correo) => correo.receptor === usuario.correoElectronico);
  }

  // Método para obtener todos los usuarios del sistema
  obtenerUsuarios(): Usuario[] {
    return this.usuarios;
  }
}

// **Uso del sistema de gestión de correo electrónico**

// Crear un nuevo sistema de gestión de correo electrónico
const sistemaCorreoElectronico = new SistemaGestionCorreoElectronico();

// Crear nuevos usuarios
const usuario1 = crearUsuario("Juan", "juan@ejemplo.com", "123456");
const usuario2 = crearUsuario("María", "maria@ejemplo.com", "654321");

// Registrar los usuarios en el sistema
sistemaCorreoElectronico.registrarUsuario(usuario1);
sistemaCorreoElectronico.registrarUsuario(usuario2);

// Crear un nuevo correo electrónico
const correoElectronico1 = crearCorreoElectronico("Hola, Juan", "Te envío este correo para saludarte", []);

// Enviar el correo electrónico al usuario 1
sistemaCorreoElectronico.enviarCorreoElectronico(correoElectronico1);

// Obtener todos los correos electrónicos del usuario 1
const correosElectronicosUsuario1 = sistemaCorreoElectronico.obtenerCorreosElectronicosUsuario(usuario1);

// Imprimir los asuntos de los correos electrónicos del usuario 1
correosElectronicosUsuario1.forEach((correo) => console.log(correo.asunto));

// Obtener todos los usuarios del sistema
const usuarios = sistemaCorreoElectronico.obtenerUsuarios();

// Imprimir los nombres de los usuarios del sistema
usuarios.forEach((usuario) => console.log(usuario.nombre));
```

**Explicación del código:**

1. **Archivos adjuntos**: Se define una interfaz para representar archivos adjuntos y una función para crear nuevos archivos adjuntos.
2. **Correos electrónicos**: Se define una interfaz para representar correos electrónicos y una función para crear nuevos correos electrónicos.
3. **Usuarios**: Se define una interfaz para representar usuarios y una función para crear nuevos usuarios.
4. **Sistema de gestión de correo electrónico**: Se define una clase para representar el sistema de gestión de correo electrónico con sus atributos y métodos.
5. **Uso del sistema de gestión de correo electrónico**: Se crea un nuevo sistema de gestión de correo electrónico, se registran nuevos usuarios, se crea un nuevo correo electrónico, se envía al usuario 1, se obtienen todos los correos electrónicos del usuario 1 y se imprimen sus asuntos, y se obtienen todos los usuarios del sistema y se imprimen sus nombres.