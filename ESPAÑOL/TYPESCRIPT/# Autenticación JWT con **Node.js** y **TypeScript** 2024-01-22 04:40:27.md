```typescript
// 1. Definir una interfaz para un objeto de datos de usuario.
interface Usuario {
  nombre: string;
  correoElectronico: string;
  contrasena: string;
  direccion: string;
  ciudad: string;
  pais: string;
}

// 2. Crear una función para validar los datos de usuario.
function validarDatosUsuario(usuario: Usuario): boolean {
  // Comprobar si todos los campos están presentes y no están vacíos.
  if (!usuario.nombre || !usuario.correoElectronico || !usuario.contrasena || !usuario.direccion || !usuario.ciudad || !usuario.pais) {
    return false;
  }

  // Comprobar si el correo electrónico tiene un formato válido.
  const regex = /^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$/;
  if (!regex.test(usuario.correoElectronico)) {
    return false;
  }

  // Comprobar si la contraseña tiene al menos 8 caracteres.
  if (usuario.contrasena.length < 8) {
    return false;
  }

  // Devolver true si todos los campos son válidos.
  return true;
}

// 3. Crear una función para generar un token de acceso JSON Web Token (JWT).
function generarTokenJWT(usuario: Usuario): string {
  // Crear una carga útil para el JWT.
  const payload = {
    nombre: usuario.nombre,
    correoElectronico: usuario.correoElectronico,
    fechaExpiracion: new Date(Date.now() + 3600000).toISOString(), // 1 hora de expiración
  };

  // Firmar el JWT con una clave secreta.
  const token = jwt.sign(payload, 'mi-clave-secreta');

  // Devolver el JWT.
  return token;
}

// 4. Crear una función para autenticar a un usuario.
function autenticarUsuario(correoElectronico: string, contrasena: string): string | null {
  // Buscar al usuario en la base de datos.
  const usuario = usuarios.find(u => u.correoElectronico === correoElectronico && u.contrasena === contrasena);

  // Si el usuario no existe, devolver null.
  if (!usuario) {
    return null;
  }

  // Generar un JWT para el usuario.
  const token = generarTokenJWT(usuario);

  // Devolver el JWT.
  return token;
}

// 5. Crear una función para proteger una ruta con autenticación JWT.
function protegerRuta(req: Request, res: Response, next: NextFunction) {
  // Extraer el token JWT de la cabecera de la solicitud.
  const token = req.headers['authorization'];

  // Si no hay token, devolver un error 401 Unauthorized.
  if (!token) {
    return res.status(401).json({
      error: 'No se ha proporcionado ningún token de acceso.',
    });
  }

  // Verificar el JWT.
  try {
    const decoded = jwt.verify(token, 'mi-clave-secreta');

    // Si el JWT es válido, adjuntar el usuario decodificado a la solicitud.
    req.usuario = decoded;

    // Continuar con la siguiente función en el middleware.
    next();
  } catch (err) {
    // Si el JWT no es válido, devolver un error 401 Unauthorized.
    return res.status(401).json({
      error: 'El token de acceso no es válido.',
    });
  }
}

// 6. Crear un servidor web.
const app = express();

// Añadir el middleware de protección de rutas a todas las rutas que requieran autenticación.
app.use('/api/*', protegerRuta);

// Añadir una ruta para el registro de usuarios.
app.post('/api/registro', async (req, res) => {
  // Validar los datos del usuario.
  const usuario = req.body;
  const isValid = validarDatosUsuario(usuario);

  // Si los datos no son válidos, devolver un error 400 Bad Request.
  if (!isValid) {
    return res.status(400).json({
      error: 'Los datos del usuario no son válidos.',
    });
  }

  // Crear un nuevo usuario en la base de datos.
  const newUser = new Usuario(usuario);
  await newUser.save();

  // Generar un JWT para el nuevo usuario.
  const token = generarTokenJWT(newUser);

  // Devolver el JWT y el id del usuario.
  res.status(201).json({
    token,
    id: newUser._id,
  });
});

// Añadir una ruta para el inicio de sesión de usuarios.
app.post('/api/login', async (req, res) => {
  // Obtener el correo electrónico y la contraseña del usuario.
  const { correoElectronico, contrasena } = req.body;

  // Autenticar al usuario.
  const token = autenticarUsuario(correoElectronico, contrasena);

  // Si la autenticación falla, devolver un error 401 Unauthorized.
  if (!token) {
    return res.status(401).json({
      error: 'El correo electrónico o la contraseña son incorrectos.',
    });
  }

  // Devolver el JWT.
  res.status(200).json({
    token,
  });
});

// Añadir una ruta protegida para obtener los datos del usuario.
app.get('/api/usuario', protegerRuta, async (req, res) => {
  // Obtener el id del usuario de la solicitud.
  const id = req.usuario.id;

  // Buscar al usuario en la base de datos.
  const usuario = await Usuario.findById(id);

  // Si el usuario no existe, devolver un error 404 Not Found.
  if (!usuario) {
    return res.status(404).json({
      error: 'El usuario no existe.',
    });
  }

  // Devolver los datos del usuario.
  res.status(200).json({
    nombre: usuario.nombre,
    correoElectronico: usuario.correoElectronico,
    direccion: usuario.direccion,
    ciudad: usuario.ciudad,
    pais: usuario.pais,
  });
});

// Iniciar el servidor web.
app.listen(3000, () => {
  console.log('Servidor escuchando en el puerto 3000.');
});
```

**Explicación del código:**

1. En primer lugar, definimos una interfaz para un objeto de datos de usuario. Esto nos permite asegurarnos de que todos los objetos de usuario tienen las mismas propiedades y tipos de datos.

2. A continuación, creamos una función para validar los datos de usuario. Esta función comprueba que todos los campos estén presentes y no estén vacíos, que el correo electrónico tenga un formato válido y que la contraseña tenga al menos 8 caracteres.

3. A continuación, creamos una función para generar un token de acceso JSON Web Token (JWT). Esta función toma un objeto de usuario como entrada y devuelve un JWT firmado con una clave secreta.

4. A continuación, creamos una función para autenticar a un usuario. Esta función toma un correo electrónico y una contraseña como entrada y devuelve un JWT si el usuario existe en la base de datos y la contraseña es correcta.

5. A continuación, creamos una función para proteger una ruta con autenticación JWT. Esta función extrae el token JWT de la cabecera de la solicitud y verifica el token. Si el token es válido, adjunta el usuario decodificado a la solicitud y continúa con la siguiente función en el middleware.

6. A continuación, creamos un servidor web con Express. Añadimos el middleware de protección de rutas a todas las rutas que requieran autenticación.

7. Por último, añadimos rutas para el registro de usuarios, el inicio de sesión de usuarios y la obtención de los datos del usuario.

Este código es un ejemplo de cómo implementar la autenticación JWT en una aplicación web. Es importante tener en cuenta que este código es sólo un ejemplo y que debe adaptarse a las necesidades específicas de su aplicación.